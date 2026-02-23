import Lean
import Lean.Data.Lsp.Communication
import Dap.DebugModel
import Dap.Examples

open Lean

namespace Dap.ToyDap

structure AdapterState where
  nextSeq : Nat := 1
  session? : Option DebugSession := none
  pendingBreakpoints : Array Nat := #[]
  stmtSpans : Array StmtSpan := #[]
  sourcePath? : Option String := none
  deriving Inhabited

structure LaunchProgram where
  program : Program
  stmtSpans : Array StmtSpan := #[]

structure DapRequest where
  seq : Nat
  command : String
  arguments : Json := Json.null

private def currentFrameName (session : DebugSession) : String :=
  let pc := session.currentPc
  match session.trace.program[pc]? with
  | some stmt => toString stmt
  | none => "<terminated>"

private def normalizedStoppedReason (reason : StopReason) : String :=
  match reason with
  | .entry => "entry"
  | .step => "step"
  | .breakpoint => "breakpoint"
  | .pause => "pause"
  | .terminated => "pause"

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
    (session : DebugSession) (reason : StopReason) : IO Unit := do
  if session.atEnd || reason = .terminated then
    sendEvent stdout stRef "terminated"
  else
    sendEvent stdout stRef "stopped" <| Json.mkObj
      [ ("reason", toJson (normalizedStoppedReason reason)),
        ("threadId", toJson 1),
        ("allThreadsStopped", toJson true) ]

private def parseBreakpointLines (args : Json) : Array Nat :=
  match (args.getObjVal? "breakpoints").toOption.bind (fun v => v.getArr?.toOption) with
  | none => #[]
  | some breakpoints =>
    breakpoints.foldl (init := #[]) fun acc bp =>
      match (bp.getObjValAs? Nat "line").toOption with
      | some line =>
        if line > 0 then acc.push line else acc
      | none => acc

private def spansFromProgramInfo (programInfo : ProgramInfo) : Array StmtSpan :=
  programInfo.located.map (·.span)

private def sourceLineToStmtLine? (spans : Array StmtSpan) (line : Nat) : Option Nat :=
  if spans.isEmpty then
    none
  else
    let rec go (idx : Nat) : Option Nat :=
      if h : idx < spans.size then
        let span := spans[idx]
        if span.startLine ≤ line && line ≤ span.endLine then
          some (idx + 1)
        else
          go (idx + 1)
      else
        none
    go 0

private def stmtLineToSourceLine (spans : Array StmtSpan) (stmtLine : Nat) : Nat :=
  if spans.isEmpty then
    stmtLine
  else
    match spans[stmtLine - 1]? with
    | some span => span.startLine
    | none => stmtLine

private def breakpointsFromRequested (programSize : Nat) (spans : Array StmtSpan) (lines : Array Nat) : Array Nat :=
  lines.foldl
    (init := #[])
    (fun acc line =>
      let stmtLine? :=
        if spans.isEmpty then
          if DebugSession.isValidBreakpointLine programSize line then some line else none
        else
          sourceLineToStmtLine? spans line
      match stmtLine? with
      | some stmtLine =>
        if !acc.contains stmtLine then
          acc.push stmtLine
        else
          acc
      | none =>
        acc)

private def mkBreakpointResponseView (programSize : Nat) (spans : Array StmtSpan) (line : Nat) : Json :=
  let stmtLine? :=
    if spans.isEmpty then
      if DebugSession.isValidBreakpointLine programSize line then some line else none
    else
      sourceLineToStmtLine? spans line
  match stmtLine? with
  | some _ =>
    Json.mkObj [("line", toJson line), ("verified", toJson true)]
  | none =>
    let msg :=
      if spans.isEmpty then
        s!"Line {line} is outside the valid range 1..{programSize}"
      else
        s!"No statement maps to source line {line}"
    Json.mkObj [("line", toJson line), ("verified", toJson false), ("message", toJson msg)]

private def programFromEntryPoint? (entryPoint : String) : Option LaunchProgram :=
  let entryPoint := entryPoint.trimAscii.toString
  match entryPoint with
  | "mainProgram" | "sampleProgram"
  | "Dap.Examples.mainProgram" | "Dap.Examples.sampleProgram" =>
    let info := Dap.Examples.sampleProgramInfo
    some { program := info.program, stmtSpans := spansFromProgramInfo info }
  | _ => none

private def decodeProgram (json : Json) : Except String Program :=
  match fromJson? json with
  | .ok program => pure program
  | .error err => throw s!"Invalid 'program' payload: {err}"

private def decodeProgramInfo (json : Json) : Except String ProgramInfo :=
  match (fromJson? json : Except String ProgramInfo) with
  | .ok programInfo => pure programInfo
  | .error err => throw s!"Invalid 'programInfo' payload: {err}"

private def decodeLaunchProgram (json : Json) : Except String LaunchProgram :=
  match decodeProgramInfo json with
  | .ok programInfo =>
    pure { program := programInfo.program, stmtSpans := spansFromProgramInfo programInfo }
  | .error _ =>
    match decodeProgram json with
    | .ok program => pure { program }
    | .error err =>
      throw s!"Invalid program payload: {err}"

private def readLaunchProgramFile (path : String) : IO LaunchProgram := do
  let raw ← IO.FS.readFile path
  let json ← IO.ofExcept (Json.parse raw)
  match decodeLaunchProgram json with
  | .ok program => pure program
  | .error err => throw <| IO.userError s!"{err} (from '{path}')"

private def resolveLaunchProgram (args : Json) : IO LaunchProgram := do
  if let some programInfoJson := (args.getObjVal? "programInfo").toOption then
    match decodeProgramInfo programInfoJson with
    | .ok programInfo =>
      pure { program := programInfo.program, stmtSpans := spansFromProgramInfo programInfo }
    | .error err =>
      throw <| IO.userError err
  else if let some programJson := (args.getObjVal? "program").toOption then
    match decodeProgram programJson with
    | .ok program => pure { program }
    | .error err => throw <| IO.userError err
  else if let some programInfoFile := (args.getObjValAs? String "programInfoFile").toOption then
    readLaunchProgramFile programInfoFile
  else if let some programFile := (args.getObjValAs? String "programFile").toOption then
    readLaunchProgramFile programFile
  else
    let rawEntry := (args.getObjValAs? String "entryPoint").toOption.getD "mainProgram"
    let entry := if rawEntry.trimAscii.toString = "" then "mainProgram" else rawEntry
    match programFromEntryPoint? entry with
    | some program => pure program
    | none =>
      throw <| IO.userError
        s!"Unsupported entryPoint '{entry}' in standalone mode. Provide 'program', 'programFile', 'programInfo', or 'programInfoFile'."

private def sourceJson? (sourcePath? : Option String) : Option Json := do
  let sourcePath ← sourcePath?
  let name := (System.FilePath.mk sourcePath).fileName.getD sourcePath
  pure <| Json.mkObj [("name", toJson name), ("path", toJson sourcePath)]

private def handleInitialize (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  sendResponse stdout stRef req <| Json.mkObj
    [ ("supportsConfigurationDoneRequest", toJson true),
      ("supportsStepBack", toJson true),
      ("supportsRestartRequest", toJson false) ]
  sendEvent stdout stRef "initialized"

private def handleLaunch (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args :=
    match req.arguments with
    | .obj _ => req.arguments
    | _ => Json.mkObj []
  let launchProgram ← resolveLaunchProgram args
  let program := launchProgram.program
  let stmtSpans := launchProgram.stmtSpans
  let stopOnEntry := (args.getObjValAs? Bool "stopOnEntry").toOption.getD true
  let sourcePath? := (args.getObjValAs? String "source").toOption
  let breakpoints := parseBreakpointLines args
  let session ←
    match DebugSession.fromProgram program with
    | .ok session => pure session
    | .error err => throw <| IO.userError s!"Launch failed: {err}"
  let pending := (← stRef.get).pendingBreakpoints
  let activeBreakpoints := if breakpoints.isEmpty then pending else breakpoints
  let stmtBreakpoints := breakpointsFromRequested program.size stmtSpans activeBreakpoints
  let session := session.setBreakpoints stmtBreakpoints
  let (session, reason) := session.initialStop stopOnEntry
  stRef.modify fun st =>
    { st with
      session? := some session
      pendingBreakpoints := activeBreakpoints
      stmtSpans := stmtSpans
      sourcePath? := sourcePath? }
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef session reason

private def handleSetBreakpoints (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let args :=
    match req.arguments with
    | .obj _ => req.arguments
    | _ => Json.mkObj []
  let lines := parseBreakpointLines args
  stRef.modify fun st => { st with pendingBreakpoints := lines }
  let st ← stRef.get
  match st.session? with
  | none =>
    let fallbackSize := lines.foldl (init := 0) max
    let breakpoints := lines.map (mkBreakpointResponseView fallbackSize st.stmtSpans)
    sendResponse stdout stRef req <| Json.mkObj [("breakpoints", Json.arr breakpoints)]
  | some session =>
    let programSize := session.trace.program.size
    let normalized := breakpointsFromRequested programSize st.stmtSpans lines
    let session := { session with breakpoints := normalized }
    stRef.modify fun st => { st with session? := some session }
    let breakpoints := lines.map (mkBreakpointResponseView programSize st.stmtSpans)
    sendResponse stdout stRef req <| Json.mkObj [("breakpoints", Json.arr breakpoints)]

private def handleThreads (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let threads := Json.arr #[Json.mkObj [("id", toJson 1), ("name", toJson "main")]]
  sendResponse stdout stRef req <| Json.mkObj [("threads", threads)]

private def handleStackTrace (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let some session := (← stRef.get).session?
    | throw <| IO.userError "No active DAP session. Launch first."
  let args :=
    match req.arguments with
    | .obj _ => req.arguments
    | _ => Json.mkObj []
  let startFrame := (args.getObjValAs? Nat "startFrame").toOption.getD 0
  let levels := (args.getObjValAs? Nat "levels").toOption.getD 20
  let st ← stRef.get
  let sourceField? := sourceJson? st.sourcePath?
  let line := stmtLineToSourceLine st.stmtSpans session.currentLine
  let frameBase :=
    [ ("id", toJson 0),
      ("name", toJson (currentFrameName session)),
      ("line", toJson line),
      ("column", toJson 1) ]
  let frame :=
    match sourceField? with
    | some source => Json.mkObj <| frameBase ++ [("source", source)]
    | none => Json.mkObj frameBase
  let frames :=
    if startFrame > 0 || levels = 0 then
      #[]
    else
      #[frame]
  sendResponse stdout stRef req <| Json.mkObj
    [ ("stackFrames", Json.arr frames),
      ("totalFrames", toJson 1) ]

private def handleScopes (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let some _ := (← stRef.get).session?
    | throw <| IO.userError "No active DAP session. Launch first."
  let args :=
    match req.arguments with
    | .obj _ => req.arguments
    | _ => Json.mkObj []
  let frameId := (args.getObjValAs? Nat "frameId").toOption.getD 0
  let scopes :=
    if frameId = 0 then
      #[Json.mkObj
        [ ("name", toJson "locals"),
          ("variablesReference", toJson 1),
          ("expensive", toJson false) ]]
    else
      #[]
  sendResponse stdout stRef req <| Json.mkObj [("scopes", Json.arr scopes)]

private def handleVariables (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let some session := (← stRef.get).session?
    | throw <| IO.userError "No active DAP session. Launch first."
  let args :=
    match req.arguments with
    | .obj _ => req.arguments
    | _ => Json.mkObj []
  let variablesReference := (args.getObjValAs? Nat "variablesReference").toOption.getD 0
  let variables :=
    if variablesReference != 1 then
      #[]
    else
      session.bindings.map fun (name, value) =>
        Json.mkObj
          [ ("name", toJson name),
            ("value", toJson (toString value)),
            ("variablesReference", toJson 0) ]
  sendResponse stdout stRef req <| Json.mkObj [("variables", Json.arr variables)]

private def withSessionControl (stRef : IO.Ref AdapterState) (f : DebugSession → DebugSession × StopReason) :
    IO (DebugSession × StopReason) := do
  let some session := (← stRef.get).session?
    | throw <| IO.userError "No active DAP session. Launch first."
  let (session, reason) := f session
  stRef.modify fun st => { st with session? := some session }
  pure (session, reason)

private def handleNext (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let (session, reason) ← withSessionControl stRef DebugSession.next
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef session reason

private def handleStepBack (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let (session, reason) ← withSessionControl stRef DebugSession.stepBack
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef session reason

private def handleContinue (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let (session, reason) ← withSessionControl stRef DebugSession.continueExecution
  sendEvent stdout stRef "continued" <| Json.mkObj
    [ ("threadId", toJson 1),
      ("allThreadsContinued", toJson true) ]
  sendResponse stdout stRef req <| Json.mkObj [("allThreadsContinued", toJson true)]
  emitStopOrTerminate stdout stRef session reason

private def handlePause (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  let some session := (← stRef.get).session?
    | throw <| IO.userError "No active DAP session. Launch first."
  sendResponse stdout stRef req
  emitStopOrTerminate stdout stRef session .pause

private def handleDisconnect (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  stRef.modify fun st => { st with session? := none }
  sendResponse stdout stRef req

private def handleRequest (stdout : IO.FS.Stream) (stRef : IO.Ref AdapterState)
    (req : DapRequest) : IO Unit := do
  try
    match req.command with
    | "initialize" => handleInitialize stdout stRef req
    | "launch" => handleLaunch stdout stRef req
    | "setBreakpoints" => handleSetBreakpoints stdout stRef req
    | "configurationDone" => sendResponse stdout stRef req
    | "threads" => handleThreads stdout stRef req
    | "stackTrace" => handleStackTrace stdout stRef req
    | "scopes" => handleScopes stdout stRef req
    | "variables" => handleVariables stdout stRef req
    | "next" => handleNext stdout stRef req
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

end Dap.ToyDap
