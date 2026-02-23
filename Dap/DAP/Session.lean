/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Dap.Lang.Eval
import Dap.Lang.History

namespace Dap

inductive StopReason where
  | entry
  | step
  | breakpoint
  | pause
  | terminated
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString StopReason where
  toString
    | .entry => "entry"
    | .step => "step"
    | .breakpoint => "breakpoint"
    | .pause => "pause"
    | .terminated => "terminated"

structure DebugSession where
  program : Program
  history : Array Context := #[Context.initial]
  cursor : Nat := 0
  breakpoints : Array Nat := #[]
  deriving Repr

namespace DebugSession

def fromProgram (program : Program) : Except EvalError DebugSession := do
  pure { program }

def maxCursor (session : DebugSession) : Nat :=
  History.maxCursor session.history

def normalize (session : DebugSession) : DebugSession :=
  { session with cursor := History.normalizeCursor session.history session.cursor }

def current? (session : DebugSession) : Option Context :=
  History.current? session.history session.cursor

def currentPc (session : DebugSession) : Nat :=
  (session.current?.map (·.pc)).getD 0

def currentLine (session : DebugSession) : Nat :=
  let psize := session.program.size
  let fallback := max psize 1
  if psize = 0 then
    1
  else
    match session.current? with
    | none => fallback
    | some ctx =>
      if ctx.pc < psize then
        ctx.pc + 1
      else
        psize

def atEnd (session : DebugSession) : Bool :=
  (session.current?.map (fun ctx => decide (ctx.pc >= session.program.size))).getD true

def isValidBreakpointLine (programSize line : Nat) : Bool :=
  0 < line && line <= programSize

def normalizeBreakpoints (programSize : Nat) (lines : Array Nat) : Array Nat :=
  lines.foldl
    (init := #[])
    (fun acc line =>
      if isValidBreakpointLine programSize line && !acc.contains line then
        acc.push line
      else
        acc)

def setBreakpoints (session : DebugSession) (lines : Array Nat) : DebugSession :=
  { session with breakpoints := normalizeBreakpoints session.program.size lines }

def isBreakpointLine (session : DebugSession) (line : Nat) : Bool :=
  session.breakpoints.contains line

def hitBreakpoint (session : DebugSession) : Bool :=
  session.isBreakpointLine session.currentLine

def bindings (session : DebugSession) : Array (Var × Value) :=
  (session.current?.map Context.bindings).getD #[]

def next (session : DebugSession) : Except EvalError (DebugSession × StopReason) := do
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
    if ctx.pc >= session.program.size then
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

def stepBack (session : DebugSession) : DebugSession × StopReason :=
  let session := session.normalize
  if !History.hasPrev session.history session.cursor then
    (session, .pause)
  else
    ({ session with cursor := History.backCursor session.history session.cursor }, .step)

def continueExecution (session : DebugSession) : Except EvalError (DebugSession × StopReason) := do
  let session := session.normalize
  if session.atEnd then
    pure (session, .terminated)
  else
    let fuel := session.program.size + 1
    let rec go : Nat → DebugSession → Except EvalError (DebugSession × StopReason)
      | 0, s =>
        pure (s, .pause)
      | fuel' + 1, s => do
        let (s, reason) ← s.next
        match reason with
        | .terminated =>
          pure (s, .terminated)
        | _ =>
          if s.hitBreakpoint then
            pure (s, .breakpoint)
          else
            go fuel' s
    go fuel session

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

end Dap
