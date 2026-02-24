/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
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

private def mkControlResponse (data : SessionData) (reason : StopReason) : ControlResponse :=
  let session := data.session
  let line :=
    match stackFrameAt? session 0 with
    | some frame => frameSourceLine data.programInfo session frame
    | none => 1
  { line
    stopReason := toString reason
    terminated := session.atEnd || reason = .terminated }

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

def threads (_store : SessionStore) : ThreadsResponse :=
  { threads := #[{ id := 1, name := "main" }] }

private def applyControl
    (store : SessionStore)
    (sessionId : Nat)
    (allowTerminated : Bool := false)
    (f : DebugSession → Except EvalError (DebugSession × StopReason)) :
    Except String (SessionStore × ControlResponse) := do
  let data ← getSessionData store sessionId
  if !allowTerminated then
    ensureControllable data sessionId
  let (session, reason) ←
    match f data.session with
    | .ok value => pure value
    | .error err => throw s!"Debug operation failed: {err}"
  let data := { data with session, status := statusFromStopReason reason }
  let store := putSessionData store sessionId data
  pure (store, mkControlResponse data reason)

def next (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.next)

def stepIn (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.stepIn)

def stepOut (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.stepOut)

def stepBack (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (allowTerminated := true) (fun s => pure (DebugSession.stepBack s))

def continueExecution (store : SessionStore) (sessionId : Nat) : Except String (SessionStore × ControlResponse) :=
  applyControl store sessionId (f := DebugSession.continueExecution)

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

def scopes (store : SessionStore) (sessionId : Nat) (frameId : Nat := 0) : Except String ScopesResponse := do
  let data ← getSessionData store sessionId
  ensureControllable data sessionId
  if (stackFrameAt? data.session frameId).isSome then
    pure { scopes := #[{ name := "locals", variablesReference := frameId + 1 }] }
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
  else
    let frameId := variablesReference - 1
    match stackFrameAt? data.session frameId with
    | none =>
      pure { variables := #[] }
    | some frame =>
      let variables :=
        frame.env.toArray.map fun (name, value) =>
          { name, value := toString value : VariableView }
      pure { variables }

def disconnect (store : SessionStore) (sessionId : Nat) : SessionStore × Bool :=
  let existed := (store.sessions.get? sessionId).isSome
  let store := { store with sessions := store.sessions.erase sessionId }
  (store, existed)

def inspectSession (store : SessionStore) (sessionId : Nat) : Except String SessionData :=
  getSessionData store sessionId

end ImpLab
