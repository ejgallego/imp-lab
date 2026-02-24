/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Debugger.Core

open Lean

namespace ImpLab

structure ProgramLineView where
  functionName : String
  stmtLine : Nat
  sourceLine : Nat
  text : String
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

structure TraceCallFrameView where
  functionName : String
  stmtLine : Nat
  sourceLine : Nat
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

structure BindingView where
  name : String
  value : Int
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

structure StateView where
  functionName : String
  pc : Nat
  stmtLine : Nat
  sourceLine : Nat
  callDepth : Nat
  callStack : Array TraceCallFrameView
  bindings : Array BindingView
  heapBindings : Array BindingView
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

structure TraceWidgetInitProps where
  programInfo : ProgramInfo
  stopOnEntry : Bool := true
  breakpoints : Array Nat := #[]
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

structure TraceWidgetSessionView where
  sessionId : Nat
  program : Array ProgramLineView
  state : StateView
  stopReason : String
  terminated : Bool
  deriving Repr, Inhabited, BEq, Server.RpcEncodable, FromJson, ToJson

def ProgramLineView.ofLocatedStmt (located : LocatedStmt) : ProgramLineView :=
  { functionName := located.func
    stmtLine := located.stmtLine
    sourceLine := located.span.startLine
    text := toString located.stmt }

def BindingView.ofPair (entry : Var × Value) : BindingView :=
  { name := entry.1, value := entry.2 }

def TraceCallFrameView.ofFrame
    (programInfo : ProgramInfo)
    (session : DebugSession)
    (frame : CallFrame) : TraceCallFrameView :=
  let stmtLine := session.frameLine frame
  let sourceLine := programInfo.locationToSourceLine { func := frame.func, stmtLine }
  { functionName := frame.func
    stmtLine
    sourceLine }

def StateView.ofSession (programInfo : ProgramInfo) (session : DebugSession) : StateView :=
  let stmtLine := session.currentLine
  let sourceLine := programInfo.locationToSourceLine { func := session.currentFuncName, stmtLine }
  let callStack := (session.callFrames.reverse.map (TraceCallFrameView.ofFrame programInfo session))
  { functionName := session.currentFuncName
    pc := session.currentPc
    stmtLine := stmtLine
    sourceLine := sourceLine
    callDepth := session.currentCallDepth
    callStack := callStack
    bindings := session.bindings.map BindingView.ofPair
    heapBindings := session.heapBindings.map BindingView.ofPair }

def ProgramLineView.ofProgramInfo (programInfo : ProgramInfo) : Array ProgramLineView :=
  programInfo.located.map ProgramLineView.ofLocatedStmt

def TraceWidgetSessionView.ofSessionData
    (sessionId : Nat)
    (data : SessionData)
    (stopReason : String := "entry") : TraceWidgetSessionView :=
  { sessionId
    program := ProgramLineView.ofProgramInfo data.programInfo
    state := StateView.ofSession data.programInfo data.session
    stopReason
    terminated := data.status = .terminated || data.session.atEnd }

end ImpLab
