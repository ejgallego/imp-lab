/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Debugger.Session

open Lean

namespace ImpLab

inductive SessionStatus where
  | stopped
  | terminated
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString SessionStatus where
  toString
    | .stopped => "stopped"
    | .terminated => "terminated"

structure SessionData where
  session : DebugSession
  programInfo : ProgramInfo
  status : SessionStatus := .stopped
  exceptionBreakpointsEnabled : Bool := false
  lastException? : Option EvalError := none
  deriving Repr

structure SessionStore where
  nextId : Nat := 1
  sessions : Std.HashMap Nat SessionData := {}
  deriving Inhabited

structure LaunchResponse where
  sessionId : Nat
  threadId : Nat
  line : Nat
  stopReason : String
  terminated : Bool
  deriving Inhabited, Repr, FromJson, ToJson

structure BreakpointView where
  line : Nat
  verified : Bool
  message? : Option String := none
  deriving Inhabited, Repr, FromJson, ToJson

structure SetBreakpointsResponse where
  breakpoints : Array BreakpointView
  deriving Inhabited, Repr, FromJson, ToJson

structure ThreadView where
  id : Nat
  name : String
  deriving Inhabited, Repr, FromJson, ToJson

structure ThreadsResponse where
  threads : Array ThreadView
  deriving Inhabited, Repr, FromJson, ToJson

structure ControlResponse where
  line : Nat
  stopReason : String
  terminated : Bool
  description? : Option String := none
  deriving Inhabited, Repr, FromJson, ToJson

structure StackFrameView where
  id : Nat
  name : String
  line : Nat
  column : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure StackTraceResponse where
  stackFrames : Array StackFrameView
  totalFrames : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure ScopeView where
  name : String
  variablesReference : Nat
  expensive : Bool := false
  deriving Inhabited, Repr, FromJson, ToJson

structure ScopesResponse where
  scopes : Array ScopeView
  deriving Inhabited, Repr, FromJson, ToJson

structure VariableView where
  name : String
  value : String
  variablesReference : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

structure VariablesResponse where
  variables : Array VariableView
  deriving Inhabited, Repr, FromJson, ToJson

structure EvaluateResponse where
  result : String
  variablesReference : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

structure SetVariableResponse where
  value : String
  variablesReference : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

structure SetExceptionBreakpointsResponse where
  enabled : Bool
  deriving Inhabited, Repr, FromJson, ToJson

structure ExceptionInfoResponse where
  exceptionId : String
  description? : Option String := none
  breakMode : String := "always"
  deriving Inhabited, Repr, FromJson, ToJson

private def getSessionData (store : SessionStore) (sessionId : Nat) : Except String SessionData :=
  match store.sessions.get? sessionId with
  | some data => pure data
  | none => throw s!"Unknown DAP session id: {sessionId}"

private def putSessionData (store : SessionStore) (sessionId : Nat) (data : SessionData) : SessionStore :=
  { store with sessions := store.sessions.insert sessionId data }

private def statusFromStopReason (reason : StopReason) : SessionStatus :=
  if reason = .terminated then
    .terminated
  else
    .stopped

private def ensureControllable (data : SessionData) (sessionId : Nat) : Except String Unit := do
  if data.status = .terminated then
    throw s!"Session {sessionId} is terminated"
  else
    pure ()

private def requestedLineToLocation? (info : ProgramInfo) (line : Nat) : Option StmtLocation :=
  let loc? := info.sourceLineToLocation? line
  loc?.bind fun loc =>
    if DebugSession.isValidBreakpointLocation info.program loc then
      some loc
    else
      none

private def normalizeRequestedBreakpoints (info : ProgramInfo) (lines : Array Nat) : Array StmtLocation :=
  lines.foldl
    (init := #[])
    (fun acc line =>
      match requestedLineToLocation? info line with
      | some loc =>
        if !acc.contains loc then
          acc.push loc
        else
          acc
      | none =>
        acc)

private def mkBreakpointView (info : ProgramInfo) (line : Nat) : BreakpointView :=
  match info.sourceLineToLocation? line with
  | none =>
    { line, verified := false, message? := some s!"No statement maps to source line {line}" }
  | some loc =>
    if DebugSession.isValidBreakpointLocation info.program loc then
      { line, verified := true }
    else
      { line
        verified := false
        message? := some s!"Source line {line} maps to invalid location {loc.func}:{loc.stmtLine}" }

private def frameName (session : DebugSession) (frame : CallFrame) : String :=
  match session.frameStmt? frame with
  | some stmt => s!"{frame.func}: {stmt}"
  | none => s!"{frame.func}: <terminated>"

private def stackFramesInDisplayOrder (session : DebugSession) : Array CallFrame :=
  session.callFrames.reverse

private def stackFrameAt? (session : DebugSession) (frameId : Nat) : Option CallFrame :=
  (stackFramesInDisplayOrder session)[frameId]?

private def frameSourceLine (info : ProgramInfo) (session : DebugSession) (frame : CallFrame) : Nat :=
  info.locationToSourceLine (session.frameLocation frame)

private def requireStackFrame (session : DebugSession) (frameId : Nat) : Except String CallFrame := do
  match stackFrameAt? session frameId with
  | some frame => pure frame
  | none => throw s!"Unknown stack frame id: {frameId}"

private def mkControlResponse
    (data : SessionData)
    (reason : StopReason)
    (description? : Option String := none) : ControlResponse :=
  let session := data.session
  let line :=
    match stackFrameAt? session 0 with
    | some frame => frameSourceLine data.programInfo session frame
    | none => 1
  { line
    stopReason := toString reason
    terminated := session.atEnd || reason = .terminated
    description? }

private def evalErrorId (err : EvalError) : String :=
  match err with
  | .unboundVar _ => "unboundVar"
  | .unknownFunction _ => "unknownFunction"
  | .arityMismatch .. => "arityMismatch"
  | .missingReturn _ => "missingReturn"
  | .divByZero .. => "divByZero"
  | .undeclaredGlobal _ => "undeclaredGlobal"
  | .invalidPc .. => "invalidPc"
  | .outOfFuel _ => "outOfFuel"

private def tokenizeExpression (expression : String) : Array String :=
  ((expression.trimAscii.toString.splitOn " ").filter (· != "")).toArray

private def parseBinOpToken? (token : String) : Option BinOp :=
  match token with
  | "+" => some .add
  | "-" => some .sub
  | "*" => some .mul
  | "/" => some .div
  | "add" => some .add
  | "sub" => some .sub
  | "mul" => some .mul
  | "div" => some .div
  | _ => none

private def evalToken (env : Env) (token : String) : Except String Int :=
  match token.toInt? with
  | some value =>
    pure value
  | none =>
    match env.find? token with
    | some value => pure value
    | none => throw s!"Unknown variable '{token}'"

private def evalBinExpr (op : BinOp) (lhs rhs : Int) : Except String Int :=
  match evalBinOp op lhs rhs with
  | .ok value => pure value
  | .error err => throw (toString err)

private def evalExpression (env : Env) (expression : String) : Except String Int := do
  let tokens := tokenizeExpression expression
  match tokens.size with
  | 0 =>
    throw "evaluate requires a non-empty expression"
  | 1 =>
    evalToken env tokens[0]!
  | 3 =>
    let t0 := tokens[0]!
    let t1 := tokens[1]!
    let t2 := tokens[2]!
    match parseBinOpToken? t0 with
    | some op =>
      let lhs ← evalToken env t1
      let rhs ← evalToken env t2
      evalBinExpr op lhs rhs
    | none =>
      match parseBinOpToken? t1 with
      | some op =>
        let lhs ← evalToken env t0
        let rhs ← evalToken env t2
        evalBinExpr op lhs rhs
      | none =>
        throw s!"Unsupported expression form '{expression}'"
  | _ =>
    throw s!"Unsupported expression form '{expression}'"

private def updateSessionContext (session : DebugSession) (ctx : Context) : DebugSession :=
  let session := session.normalize
  { session with history := (session.history.extract 0 session.cursor).push ctx }

private def updateFrameEnv
    (session : DebugSession)
    (frameId : Nat)
    (f : Env → Except String Env) : Except String DebugSession := do
  let session := session.normalize
  let some ctx := session.current?
    | throw "Session has no active frame"
  let framesDisplay := ctx.frames.reverse
  if h : frameId < framesDisplay.size then
    let frame := framesDisplay[frameId]'h
    let updatedEnv ← f frame.env
    let updatedFrame : CallFrame := { frame with env := updatedEnv }
    let updatedDisplay := framesDisplay.set frameId updatedFrame
    let updatedFrames := updatedDisplay.reverse
    let some current := updatedFrames.back?
      | throw "Session has no active frame"
    let callers := updatedFrames.pop
    let updatedCtx : Context := { current, callers }
    pure <| updateSessionContext session updatedCtx
  else
    throw s!"Unknown stack frame id: {frameId}"

def launchFromProgramInfo
    (store : SessionStore)
    (programInfo : ProgramInfo)
    (stopOnEntry : Bool)
    (breakpoints : Array Nat) : Except String (SessionStore × LaunchResponse) := do
  let programInfo ← programInfo.validate
  let session ←
    match DebugSession.fromProgram programInfo.program with
    | .ok session => pure session
    | .error err => throw s!"Launch failed: {err}"
  let normalizedBreakpoints := normalizeRequestedBreakpoints programInfo breakpoints
  let session := session.setBreakpoints normalizedBreakpoints
  let (session, stopReason) ←
    match session.initialStop stopOnEntry with
    | .ok value => pure value
    | .error err => throw s!"Launch failed: {err}"
  let sessionId := store.nextId
  let data : SessionData := { session, programInfo, status := statusFromStopReason stopReason }
  let store :=
    { nextId := sessionId + 1
      sessions := store.sessions.insert sessionId data }
  let line :=
    match stackFrameAt? session 0 with
    | some frame => frameSourceLine programInfo session frame
    | none => 1
  let response : LaunchResponse :=
    { sessionId
      threadId := 1
      line
      stopReason := toString stopReason
      terminated := session.atEnd || stopReason = .terminated }
  pure (store, response)

def setBreakpoints
    (store : SessionStore)
    (sessionId : Nat)
    (breakpoints : Array Nat) : Except String (SessionStore × SetBreakpointsResponse) := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let normalized := normalizeRequestedBreakpoints data.programInfo breakpoints
  let data := { data with session := data.session.setBreakpoints normalized }
  let store := putSessionData store sessionId data
  let views := breakpoints.map (mkBreakpointView data.programInfo)
  pure (store, { breakpoints := views })

def setExceptionBreakpoints
    (store : SessionStore)
    (sessionId : Nat)
    (filters : Array String) : Except String (SessionStore × SetExceptionBreakpointsResponse) := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let enabled := !filters.isEmpty
  let data := { data with exceptionBreakpointsEnabled := enabled, lastException? := none }
  let store := putSessionData store sessionId data
  pure (store, { enabled })

def threads (_store : SessionStore) : ThreadsResponse :=
  { threads := #[{ id := 1, name := "main" }] }

private def applyControl
    (store : SessionStore)
    (sessionId : Nat)
    (allowTerminated : Bool := false)
    (f : DebugSession → Except DebugSession.ControlFailure (DebugSession × StopReason)) :
    Except String (SessionStore × ControlResponse) := do
  let data ← getSessionData store sessionId
  if !allowTerminated then
    ensureControllable data sessionId
  match f data.session with
  | .ok (session, reason) =>
    let data := { data with session, status := statusFromStopReason reason, lastException? := none }
    let store := putSessionData store sessionId data
    pure (store, mkControlResponse data reason)
  | .error (failedSession, err) =>
    if data.exceptionBreakpointsEnabled then
      let data :=
        { data with
          session := failedSession
          status := .stopped
          lastException? := some err }
      let store := putSessionData store sessionId data
      pure (store, mkControlResponse data .exception (description? := some (toString err)))
    else
      throw s!"Debug operation failed: {err}"

def next (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.nextWithState)

def stepIn (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.stepInWithState)

def stepOut (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.stepOutWithState)

def stepBack (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (allowTerminated := true) (fun s => pure (DebugSession.stepBack s))

def continueExecution (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.continueExecutionWithState)

def pause (store : SessionStore) (sessionId : Nat) : Except String ControlResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  pure (mkControlResponse data .pause)

def stackTrace
    (store : SessionStore)
    (sessionId : Nat)
    (startFrame : Nat := 0)
    (levels : Nat := 20) : Except String StackTraceResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let session := data.session
  let fullFrames :=
    (stackFramesInDisplayOrder session).foldl
      (init := #[])
      (fun acc frame =>
        let frameId := acc.size
        acc.push
          { id := frameId
            name := frameName session frame
            line := frameSourceLine data.programInfo session frame
            column := 1 : StackFrameView })
  let start := min startFrame fullFrames.size
  let stop :=
    if levels = 0 then
      start
    else
      min (start + levels) fullFrames.size
  pure
    { stackFrames := fullFrames.extract start stop
      totalFrames := fullFrames.size }

/--
Encode `(frameId, locals)` into one DAP `variablesReference`.
We keep this stateless so transports do not need a per-session reference table.
-/
private def localsReference (frameId : Nat) : Nat :=
  frameId * 2 + 1

/--
Encode `(frameId, heap)` into one DAP `variablesReference`.
Odd refs are locals, even refs are heap.
-/
private def heapReference (frameId : Nat) : Nat :=
  frameId * 2 + 2

/-- Decode a locals reference; returns `none` when the reference is not locals. -/
private def localsFrameId? (variablesReference : Nat) : Option Nat :=
  if variablesReference != 0 && variablesReference % 2 = 1 then
    some ((variablesReference - 1) / 2)
  else
    none

/-- Decode a heap reference; returns `none` when the reference is not heap. -/
private def heapFrameId? (variablesReference : Nat) : Option Nat :=
  if variablesReference != 0 && variablesReference % 2 = 0 then
    some ((variablesReference - 2) / 2)
  else
    none

def scopes (store : SessionStore) (sessionId : Nat) (frameId : Nat := 0) : Except String ScopesResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  if (stackFrameAt? data.session frameId).isSome then
    pure
      { scopes :=
          #[ { name := "locals", variablesReference := localsReference frameId },
             { name := "heap", variablesReference := heapReference frameId } ] }
  else
    pure { scopes := #[] }

def variables
    (store : SessionStore)
    (sessionId : Nat)
    (variablesReference : Nat) : Except String VariablesResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  if variablesReference = 0 then
    pure { variables := #[] }
  else if let some frameId := localsFrameId? variablesReference then
    match stackFrameAt? data.session frameId with
    | none =>
      pure { variables := #[] }
    | some frame =>
      let variables :=
        frame.env.toArray.map fun (name, value) =>
          { name, value := toString value : VariableView }
      pure { variables }
  else if let some frameId := heapFrameId? variablesReference then
    match stackFrameAt? data.session frameId with
    | none =>
      pure { variables := #[] }
    | some _ =>
      let variables :=
        data.session.heapBindings.map fun (name, value) =>
          { name, value := toString value : VariableView }
      pure { variables }
  else
    pure { variables := #[] }

def evaluate
    (store : SessionStore)
    (sessionId : Nat)
    (expression : String)
    (frameId : Nat := 0) : Except String EvaluateResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let frame ← requireStackFrame data.session frameId
  let value ← evalExpression frame.env expression
  pure { result := toString value }

def setVariable
    (store : SessionStore)
    (sessionId : Nat)
    (variablesReference : Nat)
    (name : String)
    (valueExpression : String) : Except String (SessionStore × SetVariableResponse) := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  if variablesReference = 0 then
    throw "setVariable requires variablesReference > 0"
  let (session, value) ←
    if let some frameId := localsFrameId? variablesReference then
      -- Local variable mutation is frame-scoped.
      let frame ← requireStackFrame data.session frameId
      if (frame.env.find? name).isNone then
        throw s!"Unknown variable '{name}' in selected frame"
      let value ← evalExpression frame.env valueExpression
      let session ←
        updateFrameEnv data.session frameId fun env =>
          pure <| env.insert name value
      pure (session, value)
    else if let some frameId := heapFrameId? variablesReference then
      -- Heap mutation is global, but expression evaluation still uses the selected frame env.
      let frame ← requireStackFrame data.session frameId
      let session := data.session.normalize
      let some ctx := session.current?
        | throw "Session has no active frame"
      if (ctx.heap.find? name).isNone then
        throw s!"Unknown variable '{name}' in heap"
      let value ← evalExpression frame.env valueExpression
      let updatedCtx : Context := { ctx with heap := ctx.heap.insert name value }
      pure (updateSessionContext session updatedCtx, value)
    else
      throw s!"Unsupported variablesReference: {variablesReference}"
  let data := { data with session, lastException? := none }
  let store := putSessionData store sessionId data
  -- Return a scalar result: the updated variable value itself is not expandable.
  pure (store, { value := toString value })

def exceptionInfo (store : SessionStore) (sessionId : Nat) : Except String ExceptionInfoResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let some err := data.lastException?
    | throw s!"No exception information available for session {sessionId}"
  pure
    { exceptionId := evalErrorId err
      description? := some (toString err)
      breakMode := "always" }

def disconnect (store : SessionStore) (sessionId : Nat) : SessionStore × Bool :=
  let existed := (store.sessions.get? sessionId).isSome
  let store := { store with sessions := store.sessions.erase sessionId }
  (store, existed)

def inspectSession (store : SessionStore) (sessionId : Nat) : Except String SessionData :=
  getSessionData store sessionId

end ImpLab
