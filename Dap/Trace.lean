import Dap.Eval

namespace Dap

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
  let fuel := program.size + 1
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
  explorer.trace.states.size - 1

def normalized (explorer : Explorer) : Explorer :=
  { explorer with cursor := min explorer.cursor explorer.maxCursor }

def current? (explorer : Explorer) : Option Context :=
  let explorer := explorer.normalized
  explorer.trace.states[explorer.cursor]?

def hasPrev (explorer : Explorer) : Bool :=
  explorer.cursor > 0

def hasNext (explorer : Explorer) : Bool :=
  let explorer := explorer.normalized
  explorer.cursor + 1 < explorer.trace.states.size

def back (explorer : Explorer) : Explorer :=
  if explorer.hasPrev then
    { explorer with cursor := explorer.cursor - 1 }
  else
    explorer

def forward (explorer : Explorer) : Explorer :=
  let explorer := explorer.normalized
  if explorer.hasNext then
    { explorer with cursor := explorer.cursor + 1 }
  else
    explorer

def jump (explorer : Explorer) (cursor : Nat) : Explorer :=
  { explorer with cursor := min cursor explorer.maxCursor }

end Explorer

end Dap
