/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.DAP.Session

open Lean

namespace Dap

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
  stmtSpans : Array StmtSpan := #[]
  status : SessionStatus := .stopped
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

private def requestedLineToStmtLine?
    (programSize : Nat) (spans : Array StmtSpan) (line : Nat) : Option Nat :=
  let stmtLine? :=
    if spans.isEmpty then
      some line
    else
      ProgramInfo.sourceLineToStmtLine? spans line
  stmtLine?.bind fun stmtLine =>
    if DebugSession.isValidBreakpointLine programSize stmtLine then
      some stmtLine
    else
      none

private def ensureCompatibleStmtSpans
    (programSize : Nat) (stmtSpans : Array StmtSpan) : Except String Unit :=
  if ProgramInfo.spansCompatibleWithProgramSize programSize stmtSpans then
    .ok ()
  else
    .error <|
      s!"Launch failed: incompatible statement spans. Program has {programSize} statements " ++
      s!"but span array has {stmtSpans.size} entries (must be 0 or match program size)."

private def normalizeRequestedBreakpoints
    (programSize : Nat) (spans : Array StmtSpan) (lines : Array Nat) : Array Nat :=
  lines.foldl
    (init := #[])
    (fun acc line =>
      let stmtLine? := requestedLineToStmtLine? programSize spans line
      match stmtLine? with
      | some stmtLine =>
        if !acc.contains stmtLine then
          acc.push stmtLine
        else
          acc
      | none =>
        acc)

private def mkBreakpointView (programSize : Nat) (spans : Array StmtSpan) (line : Nat) : BreakpointView :=
  if spans.isEmpty then
    match requestedLineToStmtLine? programSize spans line with
    | some _ =>
      { line, verified := true }
    | none =>
      { line, verified := false, message? := some s!"Line {line} is outside the valid range 1..{programSize}" }
  else
    match ProgramInfo.sourceLineToStmtLine? spans line with
    | none =>
      { line, verified := false, message? := some s!"No statement maps to source line {line}" }
    | some stmtLine =>
      if DebugSession.isValidBreakpointLine programSize stmtLine then
        { line, verified := true }
      else
        { line
          verified := false
          message? := some
            s!"Source line {line} maps to statement line {stmtLine}, outside the valid range 1..{programSize}." }

private def currentFrameName (session : DebugSession) : String :=
  let pc := session.currentPc
  match session.program[pc]? with
  | some stmt => toString stmt
  | none => "<terminated>"

private def mkControlResponse (data : SessionData) (reason : StopReason) : ControlResponse :=
  let session := data.session
  { line := ProgramInfo.stmtLineToSourceLine data.stmtSpans session.currentLine
    stopReason := toString reason
    terminated := session.atEnd || reason = .terminated }

def launchFromProgram
    (store : SessionStore)
    (program : Program)
    (stopOnEntry : Bool)
    (breakpoints : Array Nat)
    (stmtSpans : Array StmtSpan := #[]) : Except String (SessionStore × LaunchResponse) := do
  ensureCompatibleStmtSpans program.size stmtSpans
  let session ←
    match DebugSession.fromProgram program with
    | .ok session => pure session
    | .error err => throw s!"Launch failed: {err}"
  let normalizedBreakpoints := normalizeRequestedBreakpoints program.size stmtSpans breakpoints
  let session := session.setBreakpoints normalizedBreakpoints
  let (session, stopReason) ←
    match session.initialStop stopOnEntry with
    | .ok value => pure value
    | .error err => throw s!"Launch failed: {err}"
  let sessionId := store.nextId
  let data : SessionData := { session, stmtSpans, status := statusFromStopReason stopReason }
  let store :=
    { nextId := sessionId + 1
      sessions := store.sessions.insert sessionId data }
  let response : LaunchResponse :=
    { sessionId
      threadId := 1
      line := ProgramInfo.stmtLineToSourceLine stmtSpans session.currentLine
      stopReason := toString stopReason
      terminated := session.atEnd || stopReason = .terminated }
  pure (store, response)

def setBreakpoints
    (store : SessionStore)
    (sessionId : Nat)
    (breakpoints : Array Nat) : Except String (SessionStore × SetBreakpointsResponse) := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let programSize := data.session.program.size
  let normalized := normalizeRequestedBreakpoints programSize data.stmtSpans breakpoints
  let data := { data with session := data.session.setBreakpoints normalized }
  let store := putSessionData store sessionId data
  let views := breakpoints.map (mkBreakpointView programSize data.stmtSpans)
  pure (store, { breakpoints := views })

def threads (_store : SessionStore) : ThreadsResponse :=
  { threads := #[{ id := 1, name := "main" }] }

private def applyControl
    (store : SessionStore)
    (sessionId : Nat)
    (f : DebugSession → Except EvalError (DebugSession × StopReason)) :
    Except String (SessionStore × ControlResponse) := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  let (session, reason) ←
    match f data.session with
    | .ok value => pure value
    | .error err => throw s!"Debug operation failed: {err}"
  let data := { data with session, status := statusFromStopReason reason }
  let store := putSessionData store sessionId data
  pure (store, mkControlResponse data reason)

def next (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId DebugSession.next

def stepBack (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (fun s => pure (DebugSession.stepBack s))

def continueExecution (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId DebugSession.continueExecution

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
    #[{ id := 0
        name := currentFrameName session
        line := ProgramInfo.stmtLineToSourceLine data.stmtSpans session.currentLine
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
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  if frameId = 0 then
    pure { scopes := #[{ name := "locals", variablesReference := 1 }] }
  else
    pure { scopes := #[] }

def variables
    (store : SessionStore)
    (sessionId : Nat)
    (variablesReference : Nat) : Except String VariablesResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
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
