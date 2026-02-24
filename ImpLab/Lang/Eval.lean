/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Lang.Ast

namespace ImpLab

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
  | unknownFunction (name : FuncName)
  | arityMismatch (name : FuncName) (expected actual : Nat)
  | missingReturn (name : FuncName)
  | divByZero (lhs rhs : Int)
  | invalidPc (pc : Nat) (programLength : Nat)
  | outOfFuel (fuel : Nat)
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString EvalError where
  toString
    | .unboundVar name =>
      s!"unbound variable '{name}'"
    | .unknownFunction name =>
      s!"unknown function '{name}'"
    | .arityMismatch name expected actual =>
      s!"arity mismatch in call to '{name}': expected {expected} args, got {actual}"
    | .missingReturn name =>
      s!"function '{name}' terminated without `return`"
    | .divByZero lhs rhs =>
      s!"division by zero while evaluating {lhs} / {rhs}"
    | .invalidPc pc programLength =>
      s!"invalid program counter {pc} for program length {programLength}"
    | .outOfFuel fuel =>
      s!"evaluation exhausted fuel ({fuel})"

structure CallFrame where
  func : FuncName := Program.mainName
  pc : Nat := 0
  env : Env := {}
  retDest? : Option Var := none
  deriving Repr, Inhabited

structure Context where
  current : CallFrame := {}
  callers : Array CallFrame := #[]
  deriving Repr, Inhabited

namespace Context

def initial : Context :=
  {}

def pc (ctx : Context) : Nat :=
  ctx.current.pc

def env (ctx : Context) : Env :=
  ctx.current.env

def functionName (ctx : Context) : FuncName :=
  ctx.current.func

def frames (ctx : Context) : Array CallFrame :=
  ctx.callers.push ctx.current

def callDepth (ctx : Context) : Nat :=
  ctx.frames.size

def lookup? (ctx : Context) (name : Var) : Option Value :=
  ctx.env.find? name

def bindings (ctx : Context) : Array (Var × Value) :=
  ctx.env.toArray

end Context

private def lookupFunction (program : Program) (name : FuncName) : Except EvalError FuncDef :=
  match program.findFunction? name with
  | some fn => pure fn
  | none => throw (.unknownFunction name)

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

private def bindParams (params : Array Var) (args : Array Value) : Env :=
  let rec go (idx : Nat) (env : Env) : Env :=
    if h : idx < params.size then
      let env :=
        match args[idx]? with
        | some value => env.bind params[idx] value
        | none => env
      go (idx + 1) env
    else
      env
  go 0 {}

private def currentBodySize (program : Program) (ctx : Context) : Nat :=
  program.bodySizeOf ctx.functionName

private def advanceCurrent (ctx : Context) (env : Env) : Context :=
  { ctx with current := { ctx.current with pc := ctx.pc + 1, env } }

private def executeCall
    (program : Program)
    (ctx : Context)
    (dest : Var)
    (fnName : FuncName)
    (argNames : Array Var) : Except EvalError Context := do
  let fn ← lookupFunction program fnName
  if fn.params.size != argNames.size then
    throw (.arityMismatch fn.name fn.params.size argNames.size)
  let args ← argNames.mapM (lookupVar ctx.env)
  let callee : CallFrame :=
    { func := fn.name
      pc := 0
      env := bindParams fn.params args
      retDest? := some dest }
  pure { current := callee, callers := ctx.callers.push ctx.current }

private def executeAssign
    (program : Program)
    (ctx : Context)
    (dest : Var)
    (rhs : Rhs) : Except EvalError Context := do
  match rhs with
  | .const value =>
    pure <| advanceCurrent ctx (ctx.env.bind dest value)
  | .bin op lhsName rhsName =>
    let lhs ← lookupVar ctx.env lhsName
    let rhs ← lookupVar ctx.env rhsName
    let value ← evalBinOp op lhs rhs
    pure <| advanceCurrent ctx (ctx.env.bind dest value)
  | .call fnName argNames =>
    executeCall program ctx dest fnName argNames

private def executeReturn (program : Program) (ctx : Context) (valueName : Var) :
    Except EvalError Context := do
  let value ← lookupVar ctx.env valueName
  match ctx.current.retDest? with
  | none =>
    pure { ctx with current := { ctx.current with pc := currentBodySize program ctx } }
  | some dest =>
    let some caller := ctx.callers.back?
      | throw (.invalidPc ctx.pc ctx.callers.size)
    let callers := ctx.callers.pop
    let resumed :=
      { caller with
        pc := caller.pc + 1
        env := caller.env.bind dest value }
    pure { current := resumed, callers }

def executeStmt (program : Program) (ctx : Context) (stmt : Stmt) : Except EvalError Context := do
  match stmt with
  | .assign dest rhs =>
    executeAssign program ctx dest rhs
  | .return_ value =>
    executeReturn program ctx value

/-- One small-step transition. `none` means normal termination. -/
def step (program : Program) (ctx : Context) : Except EvalError (Option Context) := do
  let fn ← lookupFunction program ctx.functionName
  if ctx.pc = fn.body.size then
    if ctx.current.retDest?.isSome then
      throw (.missingReturn fn.name)
    else if ctx.callers.isEmpty then
      pure none
    else
      throw (.invalidPc ctx.pc fn.body.size)
  else
    match fn.body[ctx.pc]? with
    | some stmt =>
      return some (← executeStmt program ctx stmt)
    | none =>
      throw (.invalidPc ctx.pc fn.body.size)

def runFrom (program : Program) (start : Context := Context.initial) : Except EvalError Context :=
  let fuel := program.defaultFuel
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

end ImpLab
