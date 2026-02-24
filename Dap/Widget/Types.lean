/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Debugger.Core

open Lean

namespace Dap

structure ProgramLineView where
  functionName : String
  stmtLine : Nat
  sourceLine : Nat
  text : String
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure TraceCallFrameView where
  functionName : String
  stmtLine : Nat
  sourceLine : Nat
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure BindingView where
  name : String
  value : Int
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure StateView where
  functionName : String
  pc : Nat
  stmtLine : Nat
  sourceLine : Nat
  callDepth : Nat
  callStack : Array TraceCallFrameView
  bindings : Array BindingView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

structure TraceWidgetProps where
  program : Array ProgramLineView
  states : Array StateView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable

def ProgramLineView.ofLocatedStmt (located : LocatedStmt) : ProgramLineView :=
  { functionName := located.func
    stmtLine := located.stmtLine
    sourceLine := located.span.startLine
    text := toString located.stmt }

def BindingView.ofPair (entry : Var × Value) : BindingView :=
  { name := entry.1, value := entry.2 }

private def singletonSession (program : Program) (ctx : Context) : DebugSession :=
  { program, history := #[ctx], cursor := 0 }

def TraceCallFrameView.ofFrame
    (programInfo : ProgramInfo)
    (session : DebugSession)
    (frame : CallFrame) : TraceCallFrameView :=
  let stmtLine := session.frameLine frame
  let sourceLine := programInfo.locationToSourceLine { func := frame.func, stmtLine }
  { functionName := frame.func
    stmtLine
    sourceLine }

def StateView.ofContext (programInfo : ProgramInfo) (ctx : Context) : StateView :=
  let session := singletonSession programInfo.program ctx
  let stmtLine := session.currentLine
  let sourceLine := programInfo.locationToSourceLine { func := session.currentFuncName, stmtLine }
  let callStack := (session.callFrames.reverse.map (TraceCallFrameView.ofFrame programInfo session))
  { functionName := session.currentFuncName
    pc := session.currentPc
    stmtLine := stmtLine
    sourceLine := sourceLine
    callDepth := session.currentCallDepth
    callStack := callStack
    bindings := session.bindings.map BindingView.ofPair }

def TraceWidgetProps.ofContexts (programInfo : ProgramInfo) (states : Array Context) : TraceWidgetProps :=
  { program := programInfo.located.map ProgramLineView.ofLocatedStmt
    states := states.map (StateView.ofContext programInfo) }

def traceWidgetProps (programInfo : ProgramInfo) : Except String TraceWidgetProps := do
  let (programInfo, states) ← Dap.buildTimeline programInfo
  pure (TraceWidgetProps.ofContexts programInfo states)

end Dap
