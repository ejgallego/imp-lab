/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import ImpLab.Lang.Eval
import ImpLab.Lang.History

namespace ImpLab

inductive StopReason where
  | entry
  | step
  | breakpoint
  | exception
  | pause
  | terminated
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString StopReason where
  toString
    | .entry => "entry"
    | .step => "step"
    | .breakpoint => "breakpoint"
    | .exception => "exception"
    | .pause => "pause"
    | .terminated => "terminated"

structure DebugSession where
  program : Program
  history : Array Context := #[]
  cursor : Nat := 0
  breakpoints : Array StmtLocation := #[]
  deriving Repr

namespace DebugSession

def fromProgram (program : Program) : Except EvalError DebugSession := do
  if !program.hasMain then
    throw (.unknownFunction Program.mainName)
  if !program.mainHasNoParams then
    throw (.arityMismatch Program.mainName 0 (((program.mainFunction?).map (·.params.size)).getD 0))
  let initial := Context.initialForProgram program
  pure { program, history := #[initial], cursor := 0 }

def maxCursor (session : DebugSession) : Nat :=
  History.maxCursor session.history

def normalize (session : DebugSession) : DebugSession :=
  { session with cursor := History.normalizeCursor session.history session.cursor }

def current? (session : DebugSession) : Option Context :=
  History.current? session.history session.cursor

def currentPc (session : DebugSession) : Nat :=
  (session.current?.map (·.pc)).getD 0

def currentFuncName (session : DebugSession) : FuncName :=
  (session.current?.map (·.functionName)).getD Program.mainName

def currentLine (session : DebugSession) : Nat :=
  let bodySize := session.program.bodySizeOf session.currentFuncName
  let fallback := max bodySize 1
  if bodySize = 0 then
    1
  else
    match session.current? with
    | none => fallback
    | some ctx =>
      if ctx.pc < bodySize then
        ctx.pc + 1
      else
        bodySize

def frameLine (session : DebugSession) (frame : CallFrame) : Nat :=
  let bodySize := session.program.bodySizeOf frame.func
  if bodySize = 0 then
    1
  else if frame.pc < bodySize then
    frame.pc + 1
  else
    bodySize

def currentLocation? (session : DebugSession) : Option StmtLocation := do
  let ctx ← session.current?
  let line := session.frameLine ctx.current
  pure { func := ctx.functionName, stmtLine := line }

def frameLocation (session : DebugSession) (frame : CallFrame) : StmtLocation :=
  { func := frame.func, stmtLine := session.frameLine frame }

def currentStmt? (session : DebugSession) : Option Stmt := do
  let ctx ← session.current?
  session.program.stmtAt? ctx.functionName ctx.pc

def frameStmt? (session : DebugSession) (frame : CallFrame) : Option Stmt :=
  session.program.stmtAt? frame.func frame.pc

def callFrames (session : DebugSession) : Array CallFrame :=
  (session.current?.map Context.frames).getD #[]

def atEnd (session : DebugSession) : Bool :=
  match session.current? with
  | none => true
  | some ctx =>
    let bodySize := session.program.bodySizeOf ctx.functionName
    ctx.callers.isEmpty && ctx.current.retDest?.isNone && ctx.pc >= bodySize

def isValidBreakpointLocation (program : Program) (loc : StmtLocation) : Bool :=
  match program.bodyOf? loc.func with
  | some body => 0 < loc.stmtLine && loc.stmtLine <= body.size
  | none => false

def normalizeBreakpoints (program : Program) (locs : Array StmtLocation) : Array StmtLocation :=
  locs.foldl
    (init := #[])
    (fun acc loc =>
      if isValidBreakpointLocation program loc && !acc.contains loc then
        acc.push loc
      else
        acc)

def setBreakpoints (session : DebugSession) (locs : Array StmtLocation) : DebugSession :=
  { session with breakpoints := normalizeBreakpoints session.program locs }

def isBreakpointLocation (session : DebugSession) (loc : StmtLocation) : Bool :=
  session.breakpoints.contains loc

def hitBreakpoint (session : DebugSession) : Bool :=
  match session.currentLocation? with
  | some loc => session.isBreakpointLocation loc
  | none => false

def bindings (session : DebugSession) : Array (Var × Value) :=
  (session.current?.map Context.bindings).getD #[]

def heapBindings (session : DebugSession) : Array (GlobalName × Value) :=
  (session.current?.map Context.heapBindings).getD #[]

def currentCallDepth (session : DebugSession) : Nat :=
  (session.current?.map Context.callDepth).getD 0

private def advanceOne (session : DebugSession) : Except EvalError (DebugSession × StopReason) := do
  let session := session.normalize
  if History.hasNext session.history session.cursor then
    let next := { session with cursor := History.forwardCursor session.history session.cursor }
    if next.atEnd then
      pure (next, .terminated)
    else
      pure (next, .step)
  else
    let ctx ←
      match session.current? with
      | some ctx => pure ctx
      | none => throw (.invalidPc session.cursor session.history.size)
    if session.atEnd then
      pure (session, .terminated)
    else
      match ← step session.program ctx with
      | none =>
        pure (session, .terminated)
      | some nextCtx =>
        let history := session.history.push nextCtx
        let nextSession := { session with history, cursor := History.forwardCursor history session.cursor }
        if nextSession.atEnd then
          pure (nextSession, .terminated)
        else
          pure (nextSession, .step)

abbrev ControlFailure := DebugSession × EvalError

def stepInWithState (session : DebugSession) : Except ControlFailure (DebugSession × StopReason) :=
  match advanceOne session with
  | .ok value => .ok value
  | .error err => .error (session.normalize, err)

def nextWithState (session : DebugSession) : Except ControlFailure (DebugSession × StopReason) := do
  let session := session.normalize
  if session.atEnd then
    pure (session, .terminated)
  else
    let startDepth := session.currentCallDepth
    let (session, reason) ← session.stepInWithState
    match reason with
    | .terminated =>
      pure (session, .terminated)
    | _ =>
      if session.currentCallDepth ≤ startDepth then
        pure (session, .step)
      else if session.hitBreakpoint then
        pure (session, .breakpoint)
      else
        let fuel := session.program.defaultFuel
        let rec go : Nat → DebugSession → Except ControlFailure (DebugSession × StopReason)
          | 0, s =>
            pure (s, .pause)
          | fuel' + 1, s => do
            let (s, reason) ← s.stepInWithState
            match reason with
            | .terminated =>
              pure (s, .terminated)
            | _ =>
              let depth := s.currentCallDepth
              if depth ≤ startDepth then
                if s.hitBreakpoint then
                  pure (s, .breakpoint)
                else
                  pure (s, .step)
              else if s.hitBreakpoint then
                pure (s, .breakpoint)
              else
                go fuel' s
        go fuel session

def stepBack (session : DebugSession) : DebugSession × StopReason :=
  let session := session.normalize
  if !History.hasPrev session.history session.cursor then
    (session, .pause)
  else
    ({ session with cursor := History.backCursor session.history session.cursor }, .step)

def continueExecutionWithState (session : DebugSession) : Except ControlFailure (DebugSession × StopReason) := do
  let session := session.normalize
  if session.atEnd then
    pure (session, .terminated)
  else
    let fuel := session.program.defaultFuel
    let rec go : Nat → DebugSession → Except ControlFailure (DebugSession × StopReason)
      | 0, s =>
        pure (s, .pause)
      | fuel' + 1, s => do
        let (s, reason) ← s.stepInWithState
        match reason with
        | .terminated =>
          pure (s, .terminated)
        | _ =>
          if s.hitBreakpoint then
            pure (s, .breakpoint)
          else
            go fuel' s
    go fuel session

def stepOutWithState (session : DebugSession) : Except ControlFailure (DebugSession × StopReason) := do
  let session := session.normalize
  if session.atEnd then
    pure (session, .terminated)
  else
    let startDepth := session.currentCallDepth
    if startDepth ≤ 1 then
      session.continueExecutionWithState
    else
      let fuel := session.program.defaultFuel
      let rec go : Nat → DebugSession → Except ControlFailure (DebugSession × StopReason)
        | 0, s =>
          pure (s, .pause)
        | fuel' + 1, s => do
          let (s, reason) ← s.stepInWithState
          match reason with
          | .terminated =>
            pure (s, .terminated)
          | _ =>
            let depth := s.currentCallDepth
            if depth < startDepth then
              pure (s, .step)
            else if s.hitBreakpoint then
              pure (s, .breakpoint)
            else
              go fuel' s
      go fuel session

def stepIn (session : DebugSession) : Except EvalError (DebugSession × StopReason) :=
  match session.stepInWithState with
  | .ok value => .ok value
  | .error (_, err) => .error err

def next (session : DebugSession) : Except EvalError (DebugSession × StopReason) :=
  match session.nextWithState with
  | .ok value => .ok value
  | .error (_, err) => .error err

def continueExecution (session : DebugSession) : Except EvalError (DebugSession × StopReason) :=
  match session.continueExecutionWithState with
  | .ok value => .ok value
  | .error (_, err) => .error err

def stepOut (session : DebugSession) : Except EvalError (DebugSession × StopReason) :=
  match session.stepOutWithState with
  | .ok value => .ok value
  | .error (_, err) => .error err

def initialStop (session : DebugSession) (stopOnEntry : Bool) :
    Except EvalError (DebugSession × StopReason) := do
  let session := session.normalize
  if stopOnEntry then
    pure (session, .entry)
  else if session.hitBreakpoint then
    pure (session, .breakpoint)
  else
    session.continueExecution

end DebugSession

end ImpLab
