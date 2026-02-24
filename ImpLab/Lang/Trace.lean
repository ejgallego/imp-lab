/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import ImpLab.Lang.Eval
import ImpLab.Lang.History

namespace ImpLab

structure ExecutionTrace where
  program : Program
  states : Array Context
  deriving Repr

namespace ExecutionTrace

def initial? (trace : ExecutionTrace) : Option Context :=
  trace.states[0]?

def final? (trace : ExecutionTrace) : Option Context :=
  trace.states[trace.states.size - 1]?

def state? (trace : ExecutionTrace) (idx : Nat) : Option Context :=
  trace.states[idx]?

def build (program : Program) (start : Context := Context.initial) : Except EvalError ExecutionTrace := do
  let fuel := program.defaultFuel
  let rec go : Nat → Context → Array Context → Except EvalError (Array Context)
    | 0, _, _ =>
      throw (.outOfFuel fuel)
    | fuel' + 1, ctx, acc => do
      match ← step program ctx with
      | none =>
        pure acc
      | some next =>
        go fuel' next (acc.push next)
  let states ← go fuel start #[start]
  pure { program, states }

end ExecutionTrace

structure Explorer where
  trace : ExecutionTrace
  cursor : Nat := 0
  deriving Repr

namespace Explorer

def ofTrace (trace : ExecutionTrace) : Explorer :=
  { trace, cursor := 0 }

def ofProgram (program : Program) : Except EvalError Explorer := do
  let trace ← ExecutionTrace.build program
  pure (ofTrace trace)

def maxCursor (explorer : Explorer) : Nat :=
  History.maxCursor explorer.trace.states

def normalized (explorer : Explorer) : Explorer :=
  { explorer with cursor := History.normalizeCursor explorer.trace.states explorer.cursor }

def current? (explorer : Explorer) : Option Context :=
  History.current? explorer.trace.states explorer.cursor

def hasPrev (explorer : Explorer) : Bool :=
  History.hasPrev explorer.trace.states explorer.cursor

def hasNext (explorer : Explorer) : Bool :=
  History.hasNext explorer.trace.states explorer.cursor

def back (explorer : Explorer) : Explorer :=
  { explorer with cursor := History.backCursor explorer.trace.states explorer.cursor }

def forward (explorer : Explorer) : Explorer :=
  { explorer with cursor := History.forwardCursor explorer.trace.states explorer.cursor }

def jump (explorer : Explorer) (cursor : Nat) : Explorer :=
  { explorer with cursor := History.jumpCursor explorer.trace.states cursor }

end Explorer

end ImpLab
