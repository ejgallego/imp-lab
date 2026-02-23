/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Lang.Trace

open Lean

namespace Dap

structure BindingView where
  name : String
  value : Int
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure StateView where
  pc : Nat
  bindings : Array BindingView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure TraceWidgetProps where
  program : Array String
  states : Array StateView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

def BindingView.ofPair (entry : Var × Value) : BindingView :=
  { name := entry.1, value := entry.2 }

def StateView.ofContext (ctx : Context) : StateView :=
  { pc := ctx.pc
    bindings := ctx.bindings.map BindingView.ofPair }

def TraceWidgetProps.ofTrace (trace : ExecutionTrace) : TraceWidgetProps :=
  { program := trace.program.render
    states := trace.states.map StateView.ofContext }

def traceWidgetProps (program : Program) : Except EvalError TraceWidgetProps := do
  let trace ← ExecutionTrace.build program
  pure (TraceWidgetProps.ofTrace trace)

end Dap
