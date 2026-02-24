/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Lean.Data.Lsp.Communication
import ImpLab.Debugger.Core
import ImpLab.Debugger.DAP.Launch
import ImpLab.Debugger.DAP.Capabilities
import ImpLab.Debugger.DAP.ProgramInfoLoader

open Lean

namespace ImpLab.Debugger.DAP

structure AdapterState where
  nextSeq : Nat := 1
  core : SessionStore := {}
  defaultSessionId? : Option Nat := none
  pendingBreakpoints : Array Nat := #[]
  pendingExceptionFilters : Array String := #[]
  sourcePathBySession : Std.HashMap Nat String := {}
  deriving Inhabited

structure DapRequest where
  seq : Nat
  command : String
  arguments : Json := Json.null

private def normalizedStoppedReason (reason : String) : String :=
  if reason = "terminated" then "pause" else reason

private def requestArgs (req : DapRequest) : Json :=
  match req.arguments with
  | .obj _ => req.arguments
  | _ => Json.mkObj []

private def decodeRequest? (payload : String) : Except String (Option DapRequest) := do
  let json ← Json.parse payload
  let msgType ← json.getObjValAs? String "type"
  if msgType != "request" then
    pure none
  else
    pure <| some
      { seq := (← json.getObjValAs? Nat "seq")
        command := (← json.getObjValAs? String "command")
        arguments := (json.getObjVal? "arguments").toOption.getD Json.null }

private def nextSeq (stRef : IO.Ref AdapterState) : IO Nat := do
  stRef.modifyGet fun st =>
    (st.nextSeq, { st with nextSeq := st.nextSeq + 1 })

private def sendJson (stdout : IO.FS.Stream) (msg : Json) : IO Unit := do
  stdout.writeSerializedLspMessage msg.compress

private def sendResponse (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) (body : Json := Json.mkObj [])
    (success : Bool := true) (message? : Option String := none) : IO Unit := do
  let seq ← nextSeq stRef
  let fields :=
    [ ("seq", toJson seq),
      ("type", toJson "response"),
      ("request_seq", toJson req.seq),
      ("success", toJson success),
      ("command", toJson req.command),
      ("body", body) ] ++
    match message? with
    | some message => [("message", toJson message)]
    | none => []
  sendJson stdout (Json.mkObj fields)

private def sendErrorResponse (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) (message : String) : IO Unit := do
  let body := Json.mkObj [("error", Json.mkObj [("id", toJson 1), ("format", toJson message)])]
  sendResponse stdout stRef req (body := body) (success := false) (message? := some message)

private def sendEvent (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (event : String) (body : Json := Json.mkObj []) : IO Unit := do
  let seq ← nextSeq stRef
  sendJson stdout <| Json.mkObj
    [ ("seq", toJson seq),
      ("type", toJson "event"),
      ("event", toJson event),
      ("body", body) ]

private def emitStopOrTerminate (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (stopReason : String) (terminated : Bool) (description? : Option String := none) : IO Unit := do
  if terminated || stopReason = "terminated" then
    sendEvent stdout stRef "terminated"
  else
    let fields :=
      [ ("reason", toJson (normalizedStoppedReason stopReason)),
        ("threadId", toJson 1),
        ("allThreadsStopped", toJson true) ] ++
      match description? with
      | some description => [("text", toJson description)]
      | none => []
    sendEvent stdout stRef "stopped" <| Json.mkObj fields

private def parseBreakpointsArray (json : Json) : Array Nat :=
  match json.getArr?.toOption with
  | none => #[]
  | some breakpoints =>
    breakpoints.foldl (init := #[]) fun acc bp =>
      match (bp.getObjValAs? Nat "line").toOption with
      | some line =>
        if line > 0 then
          acc.push line
        else
          acc
      | none =>
        acc

private def parseBreakpointLines (args : Json) : Array Nat :=
  match (args.getObjVal? "breakpoints").toOption with
  | some breakpointsJson => parseBreakpointsArray breakpointsJson
  | none => #[]

private def parseStringArrayField (args : Json) (field : String) : Array String :=
  match (args.getObjVal? field).toOption with
  | none => #[]
  | some value =>
    match value.getArr?.toOption with
    | none => #[]
    | some values =>
      values.foldl
        (init := #[])
        (fun acc item =>
          match (fromJson? item : Except String String) with
          | .ok str => acc.push str
          | .error _ => acc)

private def decodeProgramInfoRef (json : Json) : Except String (String × String) := do
  let moduleName ← json.getObjValAs? String "module"
  let decl := (json.getObjValAs? String "decl").toOption.getD "mainProgram"
  pure (moduleName, decl)

private def requireProgramInfoFromRef (args : Json) : IO ProgramInfo := do
  let refJson ←
    match (args.getObjVal? "programInfoRef").toOption with
    | some json => pure json
    | none =>
      throw <| IO.userError
        "launch requires 'programInfo' (a ImpLab.ProgramInfo JSON payload) or 'programInfoRef' with 'module' and optional 'decl'."
  let (moduleName, decl) ←
    match decodeProgramInfoRef refJson with
    | .ok value => pure value
    | .error err =>
      throw <| IO.userError s!"Invalid 'programInfoRef' payload: {err}"
  ImpLab.loadProgramInfoFromModuleDecl moduleName decl

private def requireProgramInfo (args : Json) : IO ProgramInfo := do
  match (args.getObjVal? "programInfo").toOption with
  | some programInfoJson =>
    match ImpLab.decodeProgramInfoJson programInfoJson with
    | .ok programInfo =>
      pure programInfo
    | .error err =>
      throw <| IO.userError err
  | none =>
    requireProgramInfoFromRef args

private def sourceJson? (sourcePath? : Option String) : Option Json := do
  let sourcePath ← sourcePath?
  let name := (System.FilePath.mk sourcePath).fileName.getD sourcePath
  pure <| Json.mkObj [("name", toJson name), ("path", toJson sourcePath)]

private def toBreakpointJson (view : BreakpointView) : Json :=
  Json.mkObj <|
    [ ("line", toJson view.line),
      ("verified", toJson view.verified) ] ++
    match view.message? with
    | some msg => [("message", toJson msg)]
    | none => []

private def sessionIdFromArgs? (args : Json) : Option Nat :=
  (args.getObjValAs? Nat "sessionId").toOption

private def requireSessionId (stRef : IO.Ref AdapterState) (args : Json) : IO Nat := do
  match sessionIdFromArgs? args with
  | some sessionId =>
    pure sessionId
  | none =>
    let some sessionId := (← stRef.get).defaultSessionId?
      | throw <| IO.userError "No default DAP session. Launch first or pass arguments.sessionId."
    pure sessionId

private def handleInitialize (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let exceptionBreakpointFilters := Json.arr
    #[Json.mkObj
      [ ("filter", toJson "runtime"),
        ("label", toJson "Runtime errors"),
        ("default", toJson true) ]]
  sendResponse stdout stRef req <| Json.mkObj
    [ ("supportsConfigurationDoneRequest", toJson dapCapabilities.supportsConfigurationDoneRequest),
      ("supportsStepBack", toJson dapCapabilities.supportsStepBack),
      ("supportsRestartRequest", toJson dapCapabilities.supportsRestartRequest),
      ("supportsEvaluateForHovers", toJson dapCapabilities.supportsEvaluateForHovers),
      ("supportsSetVariable", toJson dapCapabilities.supportsSetVariable),
      ("supportsExceptionInfoRequest", toJson dapCapabilities.supportsExceptionInfoRequest),
      ("exceptionBreakpointFilters", exceptionBreakpointFilters) ]
  sendEvent stdout stRef "initialized"

private def handleLaunch (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let programInfo ← requireProgramInfo args
  let stopOnEntry := (args.getObjValAs? Bool "stopOnEntry").toOption.getD true
  let sourcePath? := (args.getObjValAs? String "source").toOption
  let breakpoints := parseBreakpointLines args
  let pending := (← stRef.get).pendingBreakpoints
  let activeBreakpoints := if breakpoints.isEmpty then pending else breakpoints
  let st ← stRef.get
  let (coreAfterLaunch, launch) ←
    match ImpLab.launchFromProgramInfo st.core programInfo stopOnEntry activeBreakpoints with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  let core ←
    if st.pendingExceptionFilters.isEmpty then
      pure coreAfterLaunch
    else
      match ImpLab.setExceptionBreakpoints coreAfterLaunch launch.sessionId st.pendingExceptionFilters with
      | .ok (core, _) => pure core
      | .error err => throw <| IO.userError err
  stRef.modify fun st =>
    let sourcePathBySession :=
      match sourcePath? with
      | some sourcePath => st.sourcePathBySession.insert launch.sessionId sourcePath
      | none => st.sourcePathBySession.erase launch.sessionId
    { st with
      core
      defaultSessionId? := some launch.sessionId
      pendingBreakpoints := activeBreakpoints
      sourcePathBySession }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef launch.stopReason launch.terminated

private def handleSetBreakpoints (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let lines := parseBreakpointLines args
  stRef.modify fun st => { st with pendingBreakpoints := lines }
  let st ← stRef.get
  let targetSessionId? := (sessionIdFromArgs? args) <|> st.defaultSessionId?
  match targetSessionId? with
  | none =>
    let breakpoints := Json.arr <| lines.map fun line =>
      toBreakpointJson
        { line
          verified := false
          message? := some "Breakpoint pending: it will be verified when the program launches."
          : BreakpointView }
    sendResponse stdout stRef req <| Json.mkObj [("breakpoints", breakpoints)]
  | some sessionId =>
    let (core, response) ←
      match ImpLab.setBreakpoints st.core sessionId lines with
      | .ok value => pure value
      | .error err => throw <| IO.userError err
    stRef.modify fun st => { st with core }
    let breakpoints := Json.arr <| response.breakpoints.map toBreakpointJson
    sendResponse stdout stRef req <| Json.mkObj [("breakpoints", breakpoints)]

private def handleSetExceptionBreakpoints (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let filters := parseStringArrayField args "filters"
  stRef.modify fun st => { st with pendingExceptionFilters := filters }
  let st ← stRef.get
  let targetSessionId? := (sessionIdFromArgs? args) <|> st.defaultSessionId?
  match targetSessionId? with
  | none =>
    sendResponse stdout stRef req
  | some sessionId =>
    let (core, response) ←
      match ImpLab.setExceptionBreakpoints st.core sessionId filters with
      | .ok value => pure value
      | .error err => throw <| IO.userError err
    stRef.modify fun st => { st with core }
    sendResponse stdout stRef req <| Json.mkObj [("enabled", toJson response.enabled)]

private def handleThreads (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let threads := ImpLab.threads (← stRef.get).core
  let payload := Json.arr <| threads.threads.map fun t =>
    Json.mkObj [("id", toJson t.id), ("name", toJson t.name)]
  sendResponse stdout stRef req <| Json.mkObj [("threads", payload)]

private def handleStackTrace (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let sessionId ← requireSessionId stRef args
  let startFrame := (args.getObjValAs? Nat "startFrame").toOption.getD 0
  let levels := (args.getObjValAs? Nat "levels").toOption.getD 20
  let st ← stRef.get
  let response ←
    match ImpLab.stackTrace st.core sessionId startFrame levels with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  let sourceField? := sourceJson? (st.sourcePathBySession.get? sessionId)
  let stackFrames := response.stackFrames.map fun frame =>
    let base :=
      [ ("id", toJson frame.id),
        ("name", toJson frame.name),
        ("line", toJson frame.line),
        ("column", toJson frame.column) ]
    match sourceField? with
    | some source => Json.mkObj <| base ++ [("source", source)]
    | none => Json.mkObj base
  sendResponse stdout stRef req <| Json.mkObj
    [ ("stackFrames", Json.arr stackFrames),
      ("totalFrames", toJson response.totalFrames) ]

private def handleScopes (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let sessionId ← requireSessionId stRef args
  let frameId := (args.getObjValAs? Nat "frameId").toOption.getD 0
  let response ←
    match ImpLab.scopes (← stRef.get).core sessionId frameId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  let scopes := response.scopes.map fun scope =>
    Json.mkObj
      [ ("name", toJson scope.name),
        ("variablesReference", toJson scope.variablesReference),
        ("expensive", toJson scope.expensive) ]
  sendResponse stdout stRef req <| Json.mkObj [("scopes", Json.arr scopes)]

private def handleVariables (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let sessionId ← requireSessionId stRef args
  let variablesReference := (args.getObjValAs? Nat "variablesReference").toOption.getD 0
  let response ←
    match ImpLab.variables (← stRef.get).core sessionId variablesReference with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  let variables := response.variables.map fun var =>
    Json.mkObj
      [ ("name", toJson var.name),
        ("value", toJson var.value),
        ("variablesReference", toJson var.variablesReference) ]
  sendResponse stdout stRef req <| Json.mkObj [("variables", Json.arr variables)]

private def handleEvaluate (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let sessionId ← requireSessionId stRef args
  let expression ←
    match (args.getObjValAs? String "expression").toOption with
    | some expression => pure expression
    | none => throw <| IO.userError "evaluate requires arguments.expression"
  let frameId := (args.getObjValAs? Nat "frameId").toOption.getD 0
  let response ←
    match ImpLab.evaluate (← stRef.get).core sessionId expression frameId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  sendResponse stdout stRef req <| Json.mkObj
    [ ("result", toJson response.result),
      ("variablesReference", toJson response.variablesReference) ]

private def handleSetVariable (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let sessionId ← requireSessionId stRef args
  -- `variablesReference` can refer to either a locals scope or a heap scope.
  let variablesReference ←
    match (args.getObjValAs? Nat "variablesReference").toOption with
    | some variablesReference => pure variablesReference
    | none => throw <| IO.userError "setVariable requires arguments.variablesReference"
  let name ←
    match (args.getObjValAs? String "name").toOption with
    | some name => pure name
    | none => throw <| IO.userError "setVariable requires arguments.name"
  let valueExpression ←
    match (args.getObjValAs? String "value").toOption with
    | some value => pure value
    | none => throw <| IO.userError "setVariable requires arguments.value"
  let (core, response) ←
    match ImpLab.setVariable (← stRef.get).core sessionId variablesReference name valueExpression with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendResponse stdout stRef req <| Json.mkObj
    [ ("value", toJson response.value),
      ("variablesReference", toJson response.variablesReference) ]

private def handleExceptionInfo (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let response ←
    match ImpLab.exceptionInfo (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  let fields :=
    [ ("exceptionId", toJson response.exceptionId),
      ("breakMode", toJson response.breakMode) ] ++
    match response.description? with
    | some description => [("description", toJson description)]
    | none => []
  sendResponse stdout stRef req <| Json.mkObj fields

private def handleNext (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let (core, response) ←
    match ImpLab.next (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handleStepIn (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let (core, response) ←
    match ImpLab.stepIn (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handleStepOut (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let (core, response) ←
    match ImpLab.stepOut (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handleStepBack (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let (core, response) ←
    match ImpLab.stepBack (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handleContinue (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let (core, response) ←
    match ImpLab.continueExecution (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  stRef.modify fun st => { st with core }
  sendEvent stdout stRef "continued" <| Json.mkObj
    [ ("threadId", toJson 1),
      ("allThreadsContinued", toJson true) ]
  sendResponse stdout stRef req <| Json.mkObj [("allThreadsContinued", toJson true)]
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handlePause (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let sessionId ← requireSessionId stRef (requestArgs req)
  let response ←
    match ImpLab.pause (← stRef.get).core sessionId with
    | .ok value => pure value
    | .error err => throw <| IO.userError err
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef response.stopReason response.terminated response.description?

private def handleDisconnect (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args := requestArgs req
  let st ← stRef.get
  let targetSessionId? := (sessionIdFromArgs? args) <|> st.defaultSessionId?
  let core :=
    match targetSessionId? with
    | some sessionId => (ImpLab.disconnect st.core sessionId).1
    | none => st.core
  let sourcePathBySession :=
    match targetSessionId? with
    | some sessionId => st.sourcePathBySession.erase sessionId
    | none => st.sourcePathBySession
  let defaultSessionId? :=
    match st.defaultSessionId? with
    | some defaultSessionId =>
      if (core.sessions.get? defaultSessionId).isSome then
        some defaultSessionId
      else
        none
    | none =>
      none
  stRef.modify fun st => { st with core, defaultSessionId?, sourcePathBySession }
  sendResponse stdout stRef req

private def handleRequest (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  try
    match req.command with
    | "initialize" => handleInitialize stdout stRef req
    | "launch" => handleLaunch stdout stRef req
    | "setBreakpoints" => handleSetBreakpoints stdout stRef req
    | "setExceptionBreakpoints" => handleSetExceptionBreakpoints stdout stRef req
    | "configurationDone" => sendResponse stdout stRef req
    | "threads" => handleThreads stdout stRef req
    | "stackTrace" => handleStackTrace stdout stRef req
    | "scopes" => handleScopes stdout stRef req
    | "variables" => handleVariables stdout stRef req
    | "evaluate" => handleEvaluate stdout stRef req
    | "setVariable" => handleSetVariable stdout stRef req
    | "exceptionInfo" => handleExceptionInfo stdout stRef req
    | "next" => handleNext stdout stRef req
    | "stepIn" => handleStepIn stdout stRef req
    | "stepOut" => handleStepOut stdout stRef req
    | "stepBack" => handleStepBack stdout stRef req
    | "continue" => handleContinue stdout stRef req
    | "pause" => handlePause stdout stRef req
    | "disconnect" => handleDisconnect stdout stRef req
    | "terminate" => handleDisconnect stdout stRef req
    | _ => sendErrorResponse stdout stRef req s!"Unsupported request: {req.command}"
  catch e =>
    sendErrorResponse stdout stRef req e.toString

partial def loop (stdin stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState) : IO Unit := do
  let payload? ←
    try
      pure <| some (← stdin.readLspMessageAsString)
    catch e =>
      let msg := e.toString
      if msg.contains "Stream was closed" then
        pure none
      else
        throw e
  match payload? with
  | none => pure ()
  | some payload =>
    match decodeRequest? payload with
    | .ok none =>
      loop stdin stdout stRef
    | .ok (some req) =>
      handleRequest stdout stRef req
      loop stdin stdout stRef
    | .error err =>
      IO.eprintln s!"[toydap] ignoring malformed message: {err}"
      loop stdin stdout stRef

def run : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let stRef ← IO.mkRef ({} : AdapterState)
  loop stdin stdout stRef

end ImpLab.Debugger.DAP
