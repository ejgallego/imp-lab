import Lean
import Dap.Syntax

namespace Dap

abbrev Value := Int
abbrev Env := Lean.RBMap Var Value compare

namespace Env

def lookup? (env : Env) (name : Var) : Option Value :=
  env.find? name

def bind (env : Env) (name : Var) (value : Value) : Env :=
  env.insert name value

def bindings (env : Env) : Array (Var × Value) :=
  env.toArray

end Env

inductive EvalError where
  | unboundVar (name : Var)
  | divByZero (lhs rhs : Int)
  | invalidPc (pc : Nat) (programLength : Nat)
  | outOfFuel (fuel : Nat)
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString EvalError where
  toString
    | .unboundVar name =>
      s!"unbound variable '{name}'"
    | .divByZero lhs rhs =>
      s!"division by zero while evaluating {lhs} / {rhs}"
    | .invalidPc pc programLength =>
      s!"invalid program counter {pc} for program length {programLength}"
    | .outOfFuel fuel =>
      s!"evaluation exhausted fuel ({fuel})"

structure Context where
  pc : Nat := 0
  env : Env := {}
  deriving Repr, Inhabited

namespace Context

def initial : Context :=
  {}

def lookup? (ctx : Context) (name : Var) : Option Value :=
  ctx.env.find? name

def bindings (ctx : Context) : Array (Var × Value) :=
  ctx.env.toArray

end Context

def evalBinOp (op : BinOp) (lhs rhs : Int) : Except EvalError Int :=
  match op with
  | .add => pure (lhs + rhs)
  | .sub => pure (lhs - rhs)
  | .mul => pure (lhs * rhs)
  | .div =>
    if rhs == 0 then
      throw (.divByZero lhs rhs)
    else
      pure (lhs / rhs)

def lookupVar (env : Env) (name : Var) : Except EvalError Int :=
  match env.find? name with
  | some value => pure value
  | none => throw (.unboundVar name)

def evalRhs (env : Env) (rhs : Rhs) : Except EvalError Int := do
  match rhs with
  | .const value =>
    pure value
  | .bin op lhsName rhsName =>
    let lhs ← lookupVar env lhsName
    let rhs ← lookupVar env rhsName
    evalBinOp op lhs rhs

def executeStmt (ctx : Context) (stmt : Stmt) : Except EvalError Context := do
  let value ← evalRhs ctx.env stmt.rhs
  pure
    { pc := ctx.pc + 1
      env := ctx.env.bind stmt.dest value }

/-- One small-step transition. `none` means normal termination. -/
def step (program : Program) (ctx : Context) : Except EvalError (Option Context) :=
  if ctx.pc = program.size then
    pure none
  else
    match program[ctx.pc]? with
    | some stmt =>
      return some (← executeStmt ctx stmt)
    | none =>
      throw (EvalError.invalidPc ctx.pc program.size)

def runFrom (program : Program) (start : Context := Context.initial) : Except EvalError Context :=
  let fuel := program.size + 1
  let rec go : Nat → Context → Except EvalError Context
    | 0, _ =>
      throw (.outOfFuel fuel)
    | fuel' + 1, ctx => do
      match ← step program ctx with
      | none => pure ctx
      | some next => go fuel' next
  go fuel start

def run (program : Program) : Except EvalError Context :=
  runFrom program

end Dap
