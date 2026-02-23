import Lean
import Dap.DebugModel

open Lean

namespace Dap

structure SessionData where
  session : DebugSession
  stmtSpans : Array StmtSpan := #[]
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

private def getSessionData (store : SessionStore) (sessionId : Nat) : Except String SessionData :=
  match store.sessions.get? sessionId with
  | some data => pure data
  | none => throw s!"Unknown DAP session id: {sessionId}"

private def putSessionData (store : SessionStore) (sessionId : Nat) (data : SessionData) : SessionStore :=
  { store with sessions := store.sessions.insert sessionId data }

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

private def normalizeRequestedBreakpoints
    (programSize : Nat) (spans : Array StmtSpan) (lines : Array Nat) : Array Nat :=
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

private def mkBreakpointView (programSize : Nat) (spans : Array StmtSpan) (line : Nat) : BreakpointView :=
  let stmtLine? :=
    if spans.isEmpty then
      if DebugSession.isValidBreakpointLine programSize line then some line else none
    else
      sourceLineToStmtLine? spans line
  match stmtLine? with
  | some _ =>
    { line, verified := true }
  | none =>
    let message? :=
      if spans.isEmpty then
        some s!"Line {line} is outside the valid range 1..{programSize}"
      else
        some s!"No statement maps to source line {line}"
    { line, verified := false, message? }

private def currentFrameName (session : DebugSession) : String :=
  let pc := session.currentPc
  match session.trace.program[pc]? with
  | some stmt => toString stmt
  | none => "<terminated>"

private def mkControlResponse (data : SessionData) (reason : StopReason) : ControlResponse :=
  let session := data.session
  { line := stmtLineToSourceLine data.stmtSpans session.currentLine
    stopReason := toString reason
    terminated := session.atEnd || reason = .terminated }

def launchFromProgram
    (store : SessionStore)
    (program : Program)
    (stopOnEntry : Bool)
    (breakpoints : Array Nat)
    (stmtSpans : Array StmtSpan := #[]) : Except String (SessionStore × LaunchResponse) := do
  let session ←
    match DebugSession.fromProgram program with
    | .ok session => pure session
    | .error err => throw s!"Launch failed: {err}"
  let normalizedBreakpoints := normalizeRequestedBreakpoints program.size stmtSpans breakpoints
  let session := session.setBreakpoints normalizedBreakpoints
  let (session, stopReason) := session.initialStop stopOnEntry
  let sessionId := store.nextId
  let data : SessionData := { session, stmtSpans }
  let store :=
    { nextId := sessionId + 1
      sessions := store.sessions.insert sessionId data }
  let response : LaunchResponse :=
    { sessionId
      threadId := 1
      line := stmtLineToSourceLine stmtSpans session.currentLine
      stopReason := toString stopReason
      terminated := session.atEnd || stopReason = .terminated }
  pure (store, response)

def setBreakpoints
    (store : SessionStore)
    (sessionId : Nat)
    (breakpoints : Array Nat) : Except String (SessionStore × SetBreakpointsResponse) := do
  let data ← getSessionData store sessionId
  let programSize := data.session.trace.program.size
  let normalized := normalizeRequestedBreakpoints programSize data.stmtSpans breakpoints
  let data := { data with session := { data.session with breakpoints := normalized } }
  let store := putSessionData store sessionId data
  let views := breakpoints.map (mkBreakpointView programSize data.stmtSpans)
  pure (store, { breakpoints := views })

def threads (_store : SessionStore) : ThreadsResponse :=
  { threads := #[{ id := 1, name := "main" }] }

private def applyControl
    (store : SessionStore)
    (sessionId : Nat)
    (f : DebugSession → DebugSession × StopReason) : Except String (SessionStore × ControlResponse) := do
  let data ← getSessionData store sessionId
  let (session, reason) := f data.session
  let data := { data with session }
  let store := putSessionData store sessionId data
  pure (store, mkControlResponse data reason)

def next (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId DebugSession.next

def stepBack (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId DebugSession.stepBack

def continueExecution (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId DebugSession.continueExecution

def pause (store : SessionStore) (sessionId : Nat) : Except String ControlResponse := do
  let data ← getSessionData store sessionId
  pure (mkControlResponse data .pause)

def stackTrace
    (store : SessionStore)
    (sessionId : Nat)
    (startFrame : Nat := 0)
    (levels : Nat := 20) : Except String StackTraceResponse := do
  let data ← getSessionData store sessionId
  let session := data.session
  let fullFrames :=
    #[{ id := 0
        name := currentFrameName session
        line := stmtLineToSourceLine data.stmtSpans session.currentLine
        column := 1 : StackFrameView }]
  let frames :=
    if startFrame > 0 || levels = 0 then
      #[]
    else
      fullFrames
  pure
    { stackFrames := frames
      totalFrames := fullFrames.size }

def scopes (store : SessionStore) (sessionId : Nat) (frameId : Nat := 0) : Except String ScopesResponse := do
  let _ ← getSessionData store sessionId
  if frameId = 0 then
    pure { scopes := #[{ name := "locals", variablesReference := 1 }] }
  else
    pure { scopes := #[] }

def variables
    (store : SessionStore)
    (sessionId : Nat)
    (variablesReference : Nat) : Except String VariablesResponse := do
  let data ← getSessionData store sessionId
  if variablesReference != 1 then
    pure { variables := #[] }
  else
    let variables :=
      data.session.bindings.map fun (name, value) =>
        { name, value := toString value : VariableView }
    pure { variables }

def disconnect (store : SessionStore) (sessionId : Nat) : SessionStore × Bool :=
  let existed := (store.sessions.get? sessionId).isSome
  let store := { store with sessions := store.sessions.erase sessionId }
  (store, existed)

end Dap
