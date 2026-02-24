/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Test.Util

open ImpLab
open Lean

namespace ImpLab.Tests

private def mkProgram
    (mainBody : Array Stmt)
    (helpers : Array FuncDef := #[])
    (globals : Array GlobalDecl := #[]) : Program :=
  { globals, functions := #[{ name := Program.mainName, params := #[], body := mainBody }] ++ helpers }

private def mkProgramInfo (program : Program) : ProgramInfo :=
  let located :=
    Id.run do
      let mut acc : Array LocatedStmt := #[]
      for fn in program.functions do
        for i in [:fn.body.size] do
          let stmt := fn.body[i]!
          let stmtLine := i + 1
          let span : StmtSpan :=
            { startLine := stmtLine, startColumn := 0, endLine := stmtLine, endColumn := 0 }
          acc := acc.push { func := fn.name, stmtLine, stmt, span }
      pure acc
  { program, located }

def testRunProgram : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "a" 10,
      Stmt.letConst "b" 7,
      Stmt.letBin "sum" .add "a" "b",
      Stmt.letBin "out" .mul "sum" "b"
    ]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testRunProgram failed: {err}"
  | .ok ctx =>
    assertEq "pc after run" ctx.pc program.size
    assertSomeEq "out value" (ctx.lookup? "out") 119
    assertSomeEq "sum value" (ctx.lookup? "sum") 17

def testUnboundVariable : IO Unit := do
  let program := mkProgram #[Stmt.letBin "z" .add "x" "y"]
  match run program with
  | .ok _ =>
    throw <| IO.userError "testUnboundVariable failed: expected an error"
  | .error err =>
    assertEq "unbound variable error" err (.unboundVar "x")

def testGlobalGetSetProgram : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letGet "before" "counter",
      Stmt.letConst "one" 1,
      Stmt.letBin "next" .add "before" "one",
      Stmt.setGlobal "counter" "next",
      Stmt.letGet "after" "counter"
    ]
    #[]
    #[{ name := "counter", init := 41 }]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testGlobalGetSetProgram failed: {err}"
  | .ok ctx =>
    assertSomeEq "global read before set" (ctx.lookup? "before") 41
    assertSomeEq "global read after set" (ctx.lookup? "after") 42
    assertSomeEq "global final heap value" (ctx.heap.find? "counter") 42

def testUndeclaredGlobalAccess : IO Unit := do
  let getProgram := mkProgram #[Stmt.letGet "x" "missing"]
  match run getProgram with
  | .ok _ =>
    throw <| IO.userError "testUndeclaredGlobalAccess get should fail"
  | .error err =>
    assertEq "undeclared global get error" err (.undeclaredGlobal "missing")
  let setProgram := mkProgram #[Stmt.letConst "x" 3, Stmt.setGlobal "missing" "x"]
  match run setProgram with
  | .ok _ =>
    throw <| IO.userError "testUndeclaredGlobalAccess set should fail"
  | .error err =>
    assertEq "undeclared global set error" err (.undeclaredGlobal "missing")

def testGlobalHeapPersistsAcrossCall : IO Unit := do
  let identity : FuncDef :=
    { name := "identity"
      params := #["x"]
      body := #[.return_ "x"] }
  let program := mkProgram
    #[
      Stmt.letConst "one" 1,
      Stmt.setGlobal "counter" "one",
      Stmt.letCall "echo" "identity" #["one"],
      Stmt.letGet "after" "counter"
    ]
    #[identity]
    #[{ name := "counter", init := 0 }]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testGlobalHeapPersistsAcrossCall failed: {err}"
  | .ok ctx =>
    assertSomeEq "global survives call/return" (ctx.lookup? "after") 1
    assertSomeEq "heap value survives call/return" (ctx.heap.find? "counter") 1

def testRunFunctionCallProgram : IO Unit := do
  let scaleAndShift : FuncDef :=
    { name := "scaleAndShift"
      params := #["x", "y"]
      body :=
        #[
          Stmt.letBin "prod" .mul "x" "y",
          Stmt.letConst "shift" 5,
          Stmt.letBin "out" .add "prod" "shift",
          .return_ "out"
        ] }
  let program := mkProgram
    #[
      Stmt.letConst "a" 6,
      Stmt.letConst "b" 4,
      Stmt.letCall "res" "scaleAndShift" #["a", "b"]
    ]
    #[scaleAndShift]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testRunFunctionCallProgram failed: {err}"
  | .ok ctx =>
    assertEq "function call final pc" ctx.pc program.size
    assertSomeEq "function call result" (ctx.lookup? "res") 29

def testUnknownFunctionCall : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "x" 1,
      Stmt.letCall "y" "missing" #["x"]
    ]
  match run program with
  | .ok _ =>
    throw <| IO.userError "testUnknownFunctionCall failed: expected an error"
  | .error err =>
    assertEq "unknown function error" err (.unknownFunction "missing")

def testArityMismatch : IO Unit := do
  let inc : FuncDef :=
    { name := "inc"
      params := #["x"]
      body := #[Stmt.letConst "one" 1, Stmt.letBin "out" .add "x" "one", .return_ "out"] }
  let program := mkProgram
    #[
      Stmt.letConst "x" 1,
      Stmt.letConst "y" 2,
      Stmt.letCall "z" "inc" #["x", "y"]
    ]
    #[inc]
  match run program with
  | .ok _ =>
    throw <| IO.userError "testArityMismatch failed: expected an error"
  | .error err =>
    assertEq "arity mismatch error" err (.arityMismatch "inc" 1 2)

def testMissingReturn : IO Unit := do
  let bad : FuncDef :=
    { name := "bad"
      params := #["x"]
      body := #[Stmt.letConst "tmp" 1] }
  let program := mkProgram
    #[
      Stmt.letConst "x" 2,
      Stmt.letCall "out" "bad" #["x"]
    ]
    #[bad]
  match run program with
  | .ok _ =>
    throw <| IO.userError "testMissingReturn failed: expected an error"
  | .error err =>
    assertEq "missing return error" err (.missingReturn "bad")

def testHistoryCursorHelpers : IO Unit := do
  let items : Array Int := #[10, 20, 30]
  assertEq "history maxCursor" (History.maxCursor items) 2
  assertEq "history normalize clamps" (History.normalizeCursor items 99) 2
  assertSomeEq "history current? uses normalized cursor" (History.current? items 99) 30
  assertEq "history back at zero stays zero" (History.backCursor items 0) 0
  assertEq "history back normalizes before stepping" (History.backCursor items 99) 1
  assertEq "history forward at end stays end" (History.forwardCursor items 99) 2
  assertEq "history jump clamps" (History.jumpCursor items 100) 2

def testTraceShape : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "x" 1,
      Stmt.letConst "y" 2,
      Stmt.letBin "z" .add "x" "y"
    ]
  match ExecutionTrace.build program with
  | .error err =>
    throw <| IO.userError s!"testTraceShape failed: {err}"
  | .ok trace =>
    assertEq "trace length" trace.states.size (program.size + 1)
    assertSomeEq "first state pc" (trace.initial?.map (·.pc)) 0
    assertSomeEq "last state pc" (trace.final?.map (·.pc)) program.size

private def forwardN : Nat → Explorer → Explorer
  | 0, explorer => explorer
  | n + 1, explorer => forwardN n (Explorer.forward explorer)

private def backN : Nat → Explorer → Explorer
  | 0, explorer => explorer
  | n + 1, explorer => backN n (Explorer.back explorer)

def testExplorerNavigation : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "u" 9,
      Stmt.letConst "v" 3,
      Stmt.letBin "w" .sub "u" "v"
    ]
  match Explorer.ofProgram program with
  | .error err =>
    throw <| IO.userError s!"testExplorerNavigation failed: {err}"
  | .ok explorer =>
    assertEq "initial cursor" explorer.cursor 0
    let last := forwardN 100 explorer
    assertEq "forward clamps at end" last.cursor last.maxCursor
    let first := backN 100 last
    assertEq "back clamps at zero" first.cursor 0
    let mid := Explorer.jump explorer 1
    assertEq "jump to 1" mid.cursor 1

def testWidgetProps : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "x" 2,
      Stmt.letConst "y" 8,
      Stmt.letBin "z" .div "y" "x"
    ]
  let info := mkProgramInfo program
  let (store, launch) ← expectCore "widget launch" <| ImpLab.launchFromProgramInfo {} info true #[]
  let data ← expectCore "widget session inspect" <| ImpLab.inspectSession store launch.sessionId
  let props := TraceWidgetSessionView.ofSessionData launch.sessionId data launch.stopReason
  assertEq "widget session id" props.sessionId launch.sessionId
  assertEq "widget program length" props.program.size program.totalStmtCount
  assertEq "widget call stack depth" props.state.callStack.size props.state.callDepth
  assertEq "widget initial function" props.state.functionName Program.mainName

def testWidgetSessionProjectionAfterStep : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 2,
      let y := 8,
      let z := div y x
    }
  ]
  let (store1, launch) ← expectCore "widget step launch" <| ImpLab.launchFromProgramInfo {} info true #[]
  let (store2, step) ← expectCore "widget step forward" <| ImpLab.stepIn store1 launch.sessionId
  let data ← expectCore "widget step inspect" <| ImpLab.inspectSession store2 launch.sessionId
  let props := TraceWidgetSessionView.ofSessionData launch.sessionId data step.stopReason
  assertEq "widget step reason propagated" props.stopReason step.stopReason
  assertEq "widget step state pc" props.state.pc 1
  assertEq "widget step state stmt line" props.state.stmtLine 2

def testWidgetHeapProjection : IO Unit := do
  let info : ProgramInfo := imp%[
    global counter := 7,
    def main() := {
      let x := get counter
    }
  ]
  let (store, launch) ← expectCore "widget heap launch" <| ImpLab.launchFromProgramInfo {} info true #[]
  let data ← expectCore "widget heap inspect" <| ImpLab.inspectSession store launch.sessionId
  let props := TraceWidgetSessionView.ofSessionData launch.sessionId data launch.stopReason
  assertTrue "widget heap projection includes counter"
    (props.state.heapBindings.any fun b => b.name == "counter" && b.value == 7)

def testStepBackAfterTermination : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 1
    }
  ]
  let (store1, launch) ← expectCore "stepBack term launch" <| ImpLab.launchFromProgramInfo {} info false #[]
  assertEq "stepBack term launch terminated" launch.terminated true
  let (store2, back) ← expectCore "stepBack term backward" <| ImpLab.stepBack store1 launch.sessionId
  assertEq "stepBack term reason" back.stopReason "step"
  assertEq "stepBack term terminated false" back.terminated false
  let data ← expectCore "stepBack term inspect" <| ImpLab.inspectSession store2 launch.sessionId
  assertEq "stepBack term cursor rewound" data.session.currentPc 0

def testWidgetInitProps : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 1
    }
  ]
  let props : TraceWidgetInitProps := { programInfo := info, stopOnEntry := false, breakpoints := #[1] }
  let decoded ←
    match (fromJson? (toJson props) : Except String TraceWidgetInitProps) with
    | .ok decoded => pure decoded
    | .error err =>
      throw <| IO.userError s!"testWidgetInitProps decode failed: {err}"
  assertEq "widget init stopOnEntry" decoded.stopOnEntry false
  assertEq "widget init breakpoints" decoded.breakpoints #[1]

def testWidgetSessionViewJsonRoundtrip : IO Unit := do
  let view : TraceWidgetSessionView :=
    { sessionId := 7
      program := #[]
      state := default
      stopReason := "entry"
      terminated := false }
  let decoded ←
    match (fromJson? (toJson view) : Except String TraceWidgetSessionView) with
    | .ok decoded => pure decoded
    | .error err =>
      throw <| IO.userError s!"testWidgetSessionViewJsonRoundtrip decode failed: {err}"
  assertEq "widget session view roundtrip id" decoded.sessionId view.sessionId
  assertEq "widget session view roundtrip stopReason" decoded.stopReason view.stopReason

def testDebugSessionContinueAndBreakpoints : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "x" 1,
      Stmt.letConst "y" 2,
      Stmt.letBin "z" .add "x" "y",
      Stmt.letBin "w" .mul "z" "y"
    ]
  match DebugSession.fromProgram program with
  | .error err =>
    throw <| IO.userError s!"testDebugSessionContinueAndBreakpoints failed to launch: {err}"
  | .ok session =>
    let session := session.setBreakpoints #[{ func := Program.mainName, stmtLine := 3 }]
    let (stopped, reason) ←
      match session.initialStop (stopOnEntry := false) with
      | .ok value => pure value
      | .error err =>
        throw <| IO.userError s!"testDebugSessionContinueAndBreakpoints initialStop failed: {err}"
    assertEq "continue initial reason" reason .breakpoint
    assertEq "continue stopped line" stopped.currentLine 3
    let (done, doneReason) ←
      match stopped.continueExecution with
      | .ok value => pure value
      | .error err =>
        throw <| IO.userError s!"testDebugSessionContinueAndBreakpoints continue failed: {err}"
    assertEq "continue end reason" doneReason .terminated
    assertEq "continue end cursor" done.cursor done.maxCursor

def testDebugSessionStepBack : IO Unit := do
  let program := mkProgram
    #[
      Stmt.letConst "a" 4,
      Stmt.letConst "b" 5
    ]
  match DebugSession.fromProgram program with
  | .error err =>
    throw <| IO.userError s!"testDebugSessionStepBack failed to launch: {err}"
  | .ok session =>
    let (forwarded, _) ←
      match session.next with
      | .ok value => pure value
      | .error err =>
        throw <| IO.userError s!"testDebugSessionStepBack next failed: {err}"
    let (backward, reason) := forwarded.stepBack
    assertEq "stepBack reason" reason .step
    assertEq "stepBack cursor" backward.cursor 0
    let (replayed, replayReason) ←
      match backward.next with
      | .ok value => pure value
      | .error err =>
        throw <| IO.userError s!"testDebugSessionStepBack replay next failed: {err}"
    assertEq "stepBack replay cursor" replayed.cursor forwarded.cursor
    assertEq "stepBack replay reason" replayReason .step
    assertEq "stepBack replay current pc" replayed.currentPc forwarded.currentPc

def testDebugCoreStepInOut : IO Unit := do
  let info : ProgramInfo := imp%[
    def inner(x) := {
      let two := 2,
      let out := mul x two,
      return out
    },
    def outer(a) := {
      let mid := call inner(a),
      let out := add mid a,
      return out
    },
    def main() := {
      let n := 3,
      let out := call outer(n),
      let final := add out n
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "step in/out launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  assertEq "step in/out launch reason" launch.stopReason "entry"
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "step in/out main step 1" <| ImpLab.stepIn store1 sessionId
  let (store3, _) ← expectCore "step in/out enter outer" <| ImpLab.stepIn store2 sessionId
  let stackOuter ← expectCore "step in/out stack outer" <| ImpLab.stackTrace store3 sessionId
  assertEq "step in/out depth after entering outer" stackOuter.totalFrames 2
  assertTrue "step in/out top frame is outer"
    ((stackOuter.stackFrames[0]?.map (·.name.contains "outer")).getD false)
  let (store4, _) ← expectCore "step in/out enter inner" <| ImpLab.stepIn store3 sessionId
  let stackInner ← expectCore "step in/out stack inner" <| ImpLab.stackTrace store4 sessionId
  assertEq "step in/out depth after entering inner" stackInner.totalFrames 3
  assertTrue "step in/out top frame is inner"
    ((stackInner.stackFrames[0]?.map (·.name.contains "inner")).getD false)
  let (store5, outInner) ← expectCore "step in/out return from inner" <| ImpLab.stepOut store4 sessionId
  assertEq "step in/out return from inner reason" outInner.stopReason "step"
  assertEq "step in/out return from inner terminated" outInner.terminated false
  let stackAfterInner ← expectCore "step in/out stack after inner" <| ImpLab.stackTrace store5 sessionId
  assertEq "step in/out depth after inner return" stackAfterInner.totalFrames 2
  assertTrue "step in/out top frame returns to outer"
    ((stackAfterInner.stackFrames[0]?.map (·.name.contains "outer")).getD false)
  let (store6, outOuter) ← expectCore "step in/out return from outer" <| ImpLab.stepOut store5 sessionId
  assertEq "step in/out return from outer reason" outOuter.stopReason "step"
  assertEq "step in/out return from outer terminated" outOuter.terminated false
  let stackAfterOuter ← expectCore "step in/out stack after outer" <| ImpLab.stackTrace store6 sessionId
  assertEq "step in/out depth after outer return" stackAfterOuter.totalFrames 1
  assertTrue "step in/out top frame returns to main"
    ((stackAfterOuter.stackFrames[0]?.map (·.name.contains "main")).getD false)
  let (_store7, outMain) ← expectCore "step in/out return from main" <| ImpLab.stepOut store6 sessionId
  assertEq "step in/out return from main reason" outMain.stopReason "terminated"
  assertEq "step in/out return from main terminated" outMain.terminated true

def testDebugCoreNextStepsOverCall : IO Unit := do
  let info : ProgramInfo := imp%[
    def double(x) := {
      let two := 2,
      let out := mul x two,
      return out
    },
    def main() := {
      let a := 4,
      let b := call double(a),
      let c := add b a
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "step over launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  assertEq "step over launch reason" launch.stopReason "entry"
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "step over next 1" <| ImpLab.next store1 sessionId
  let (store3, nextOver) ← expectCore "step over call" <| ImpLab.next store2 sessionId
  assertEq "step over call reason" nextOver.stopReason "step"
  assertEq "step over call terminated" nextOver.terminated false
  let stackAfterCall ← expectCore "step over stack after call" <| ImpLab.stackTrace store3 sessionId
  assertEq "step over stays in caller frame" stackAfterCall.totalFrames 1
  let vars ← expectCore "step over vars after call" <| ImpLab.variables store3 sessionId 1
  assertTrue "step over computed call result in caller"
    (vars.variables.any fun v => v.name == "b" && v.value == "8")

def testDslProgram : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 6,
      let y := 7,
      let z := add x y
    }
  ]
  match run info.program with
  | .error err =>
    throw <| IO.userError s!"testDslProgram failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl result" (ctx.lookup? "z") 13

def testDslNegativeLiteral : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := -6,
      let y := 2,
      let z := add x y
    }
  ]
  match run info.program with
  | .error err =>
    throw <| IO.userError s!"testDslNegativeLiteral failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl negative literal result" (ctx.lookup? "z") (-4)

def testDslFunctionCall : IO Unit := do
  let info : ProgramInfo := imp%[
    def addMul(x, y) := {
      let sum := add x y,
      let out := mul sum y,
      return out
    },
    def main() := {
      let a := 2,
      let b := 5,
      let z := call addMul(a, b)
    }
  ]
  match run info.program with
  | .error err =>
    throw <| IO.userError s!"testDslFunctionCall failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl function call result" (ctx.lookup? "z") 35

def testDslGlobals : IO Unit := do
  let info : ProgramInfo := imp%[
    global counter := 10,
    def main() := {
      let before := get counter,
      let one := 1,
      let next := add before one,
      set counter := next,
      let after := get counter
    }
  ]
  assertEq "dsl globals declaration count" info.program.globals.size 1
  match run info.program with
  | .error err =>
    throw <| IO.userError s!"testDslGlobals failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl globals before value" (ctx.lookup? "before") 10
    assertSomeEq "dsl globals after value" (ctx.lookup? "after") 11
    assertSomeEq "dsl globals heap value" (ctx.heap.find? "counter") 11

def testDslProgramInfo : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let a := 1,
      let b := 2,
      let c := mul a b
    }
  ]
  assertEq "programInfo size" info.program.size 3
  assertEq "programInfo located size" info.located.size 3
  let line0 := (info.located[0]?.map (·.span.startLine)).getD 0
  let loc0 := (info.sourceLineToLocation? line0).getD default
  assertEq "source line maps to main" loc0.func Program.mainName
  assertEq "source line maps to first stmt" loc0.stmtLine 1
  let sourceLine := info.locationToSourceLine { func := Program.mainName, stmtLine := 1 }
  assertEq "location->source line mapping" sourceLine line0

def testProgramInfoValidation : IO Unit := do
  let stmt0 := Stmt.letConst "x" 1
  let stmt1 := Stmt.letConst "y" 2
  let span : StmtSpan := { startLine := 1, startColumn := 0, endLine := 1, endColumn := 10 }
  let missingMain : ProgramInfo :=
    { program := { functions := #[{ name := "helper", params := #[], body := #[stmt0] }] }
      located := #[{ func := "helper", stmtLine := 1, stmt := stmt0, span }] }
  match missingMain.validate with
  | .ok _ =>
    throw <| IO.userError "testProgramInfoValidation should reject missing main"
  | .error err =>
    assertTrue "missing main error mentions main" (err.contains "main")
  let badInfo : ProgramInfo :=
    { program := mkProgram #[stmt0, stmt1]
      located := #[{ func := Program.mainName, stmtLine := 1, stmt := stmt0, span }] }
  match badInfo.validate with
  | .ok _ =>
    throw <| IO.userError "testProgramInfoValidation should reject mismatched ProgramInfo"
  | .error err =>
    assertTrue "programInfo mismatch error mentions located size" (err.contains "located")
  let dupGlobalInfo : ProgramInfo :=
    { program :=
        { globals := #[{ name := "counter", init := 0 }, { name := "counter", init := 1 }]
          functions := #[{ name := Program.mainName, params := #[], body := #[stmt0] }] }
      located := #[{ func := Program.mainName, stmtLine := 1, stmt := stmt0, span }] }
  match dupGlobalInfo.validate with
  | .ok _ =>
    throw <| IO.userError "testProgramInfoValidation should reject duplicate globals"
  | .error err =>
    assertTrue "duplicate globals error mentions duplicate" (err.contains "duplicate")

def testDebugCoreFlow : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 5,
      let y := 7,
      let z := add x y
    }
  ]
  let bpLine := info.locationToSourceLine { func := Program.mainName, stmtLine := 2 }
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "core launch" <| ImpLab.launchFromProgramInfo store0 info false #[bpLine]
  assertEq "core launch stopReason" launch.stopReason "breakpoint"
  assertEq "core launch line" launch.line bpLine
  let sessionId := launch.sessionId
  let vars1 ← expectCore "core vars" <| ImpLab.variables store1 sessionId 1
  assertTrue "core vars contain x binding"
    (vars1.variables.any fun v => v.name == "x" && v.value == "5")
  let (store2, cont) ← expectCore "core continue" <| ImpLab.continueExecution store1 sessionId
  assertEq "core continue terminated" cont.terminated true
  assertEq "core continue stopReason" cont.stopReason "terminated"
  let (store3, disconnected) := ImpLab.disconnect store2 sessionId
  assertEq "core disconnect" disconnected true
  let pauseAfterDisconnect := ImpLab.pause store3 sessionId
  match pauseAfterDisconnect with
  | .ok _ =>
    throw <| IO.userError "core pause after disconnect should fail"
  | .error _ =>
    pure ()

def testDebugCoreHeapScope : IO Unit := do
  let info : ProgramInfo := imp%[
    global counter := 0,
    def main() := {
      let one := 1,
      set counter := one,
      let out := get counter
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "core heap launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let scopes0 ← expectCore "core heap scopes" <| ImpLab.scopes store1 sessionId 0
  assertEq "core heap scope count" scopes0.scopes.size 2
  let topLocalsScope := (scopes0.scopes[0]?.map (·.name)).getD ""
  let topHeapScope := (scopes0.scopes[1]?.map (·.name)).getD ""
  assertEq "core heap scope name locals" topLocalsScope "locals"
  assertEq "core heap scope name heap" topHeapScope "heap"
  let heapBefore ← expectCore "core heap vars before set" <| ImpLab.variables store1 sessionId 2
  assertTrue "core heap vars include initial counter"
    (heapBefore.variables.any fun v => v.name == "counter" && v.value == "0")
  let (store2, _) ← expectCore "core heap step one" <| ImpLab.stepIn store1 sessionId
  let (store3, _) ← expectCore "core heap step set" <| ImpLab.stepIn store2 sessionId
  let heapAfter ← expectCore "core heap vars after set" <| ImpLab.variables store3 sessionId 2
  assertTrue "core heap vars include updated counter"
    (heapAfter.variables.any fun v => v.name == "counter" && v.value == "1")

def testDebugCoreHeapScopeInCallee : IO Unit := do
  let info : ProgramInfo := imp%[
    global counter := 9,
    def id(x) := {
      return x
    },
    def main() := {
      let one := 1,
      let out := call id(one)
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "core heap callee launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "core heap callee step1" <| ImpLab.stepIn store1 sessionId
  let (store3, _) ← expectCore "core heap callee enter" <| ImpLab.stepIn store2 sessionId
  let scopesTop ← expectCore "core heap callee scopes top" <| ImpLab.scopes store3 sessionId 0
  let heapRef := (scopesTop.scopes[1]?.map (·.variablesReference)).getD 0
  let heapVars ← expectCore "core heap callee vars" <| ImpLab.variables store3 sessionId heapRef
  assertTrue "core heap callee vars include counter"
    (heapVars.variables.any fun v => v.name == "counter" && v.value == "9")

def testDebugCoreSetVariableHeap : IO Unit := do
  let info : ProgramInfo := imp%[
    global counter := 0,
    def main() := {
      let one := 1
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "setVariable heap launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, setHeap) ← expectCore "setVariable heap counter" <| ImpLab.setVariable store1 sessionId 2 "counter" "7"
  assertEq "setVariable heap response value" setHeap.value "7"
  assertEq "setVariable heap response variablesReference" setHeap.variablesReference 0
  let heapVars ← expectCore "setVariable heap variables" <| ImpLab.variables store2 sessionId 2
  assertTrue "setVariable heap updated counter"
    (heapVars.variables.any fun v => v.name == "counter" && v.value == "7")
  let setMissingHeap := ImpLab.setVariable store2 sessionId 2 "missing" "1"
  match setMissingHeap with
  | .ok _ =>
    throw <| IO.userError "setVariable should reject unknown heap variable names"
  | .error err =>
    assertTrue "setVariable unknown heap variable error" (err.contains "Unknown variable")

def testDebugCoreStackFrames : IO Unit := do
  let info : ProgramInfo := imp%[
    def addMul(x, y) := {
      let sum := add x y,
      let out := mul sum y,
      return out
    },
    def main() := {
      let a := 2,
      let b := 3,
      let z := call addMul(a, b)
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "stack frames launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  assertEq "stack frames launch stopReason" launch.stopReason "entry"
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "stack frames next 1" <| ImpLab.next store1 sessionId
  let (store3, _) ← expectCore "stack frames next 2" <| ImpLab.next store2 sessionId
  let (store4, _) ← expectCore "stack frames stepIn call" <| ImpLab.stepIn store3 sessionId
  let stack ← expectCore "stack frames stackTrace" <| ImpLab.stackTrace store4 sessionId
  assertEq "stack frames total" stack.totalFrames 2
  let top := stack.stackFrames[0]?.getD default
  let caller := stack.stackFrames[1]?.getD default
  assertTrue "stack frames top is callee" (top.name.contains "addMul")
  assertTrue "stack frames caller is main" (caller.name.contains "main")
  let scopesTop ← expectCore "stack frames scopes top" <| ImpLab.scopes store4 sessionId 0
  assertEq "stack frames top scope count" scopesTop.scopes.size 2
  let varsTop ← expectCore "stack frames vars top" <| ImpLab.variables store4 sessionId 1
  assertTrue "stack frames top vars include x"
    (varsTop.variables.any fun v => v.name == "x" && v.value == "2")
  let heapTop ← expectCore "stack frames vars heap top" <| ImpLab.variables store4 sessionId 2
  assertEq "stack frames heap empty without globals" heapTop.variables.size 0
  let varsCaller ← expectCore "stack frames vars caller" <| ImpLab.variables store4 sessionId 3
  assertTrue "stack frames caller vars include a"
    (varsCaller.variables.any fun v => v.name == "a" && v.value == "2")

def testDebugCoreEvaluateAndSetVariable : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 5,
      let y := 3,
      let z := add x y
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "evaluate launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "evaluate step to initialize x" <| ImpLab.next store1 sessionId
  let evalX ← expectCore "evaluate x" <| ImpLab.evaluate store2 sessionId "x"
  assertEq "evaluate variable result" evalX.result "5"
  let evalExpr ← expectCore "evaluate infix expression" <| ImpLab.evaluate store2 sessionId "x + 2"
  assertEq "evaluate infix result" evalExpr.result "7"
  let (store3, setVar) ← expectCore "setVariable x" <| ImpLab.setVariable store2 sessionId 1 "x" "10"
  assertEq "setVariable response value" setVar.value "10"
  assertEq "setVariable response variablesReference" setVar.variablesReference 0
  let evalAfterSet ← expectCore "evaluate after setVariable" <| ImpLab.evaluate store3 sessionId "x * 2"
  assertEq "evaluate after setVariable result" evalAfterSet.result "20"
  let vars ← expectCore "variables after setVariable" <| ImpLab.variables store3 sessionId 1
  assertTrue "setVariable updates locals view"
    (vars.variables.any fun v => v.name == "x" && v.value == "10")
  let setMissing := ImpLab.setVariable store3 sessionId 1 "missing" "1"
  match setMissing with
  | .ok _ =>
    throw <| IO.userError "setVariable should reject unknown variable names"
  | .error err =>
    assertTrue "setVariable unknown variable error" (err.contains "Unknown variable")

def testDebugCoreEvaluateAndSetVariableAcrossFrames : IO Unit := do
  let info : ProgramInfo := imp%[
    def inner(x) := {
      let one := 1,
      let out := add x one,
      return out
    },
    def outer(x) := {
      let outerLocal := 10,
      let y := call inner(x),
      let out := add y outerLocal,
      return out
    },
    def main() := {
      let x := 2,
      let out := call outer(x)
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "evaluate frames launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "evaluate frames main step 1" <| ImpLab.stepIn store1 sessionId
  let (store3, _) ← expectCore "evaluate frames enter outer" <| ImpLab.stepIn store2 sessionId
  let (store4, _) ← expectCore "evaluate frames initialize outerLocal" <| ImpLab.stepIn store3 sessionId
  let (store5, _) ← expectCore "evaluate frames enter inner" <| ImpLab.stepIn store4 sessionId
  let evalInnerX ← expectCore "evaluate frames inner x" <| ImpLab.evaluate store5 sessionId "x" 0
  assertEq "evaluate frames inner x value" evalInnerX.result "2"
  let evalOuterLocal ← expectCore "evaluate frames outer local" <| ImpLab.evaluate store5 sessionId "outerLocal" 1
  assertEq "evaluate frames outer local value" evalOuterLocal.result "10"
  let evalMissingInInner := ImpLab.evaluate store5 sessionId "outerLocal" 0
  match evalMissingInInner with
  | .ok _ =>
    throw <| IO.userError "evaluate should fail when reading caller-local from callee frame"
  | .error err =>
    assertTrue "evaluate callee missing caller-local" (err.contains "Unknown variable")
  let (store6, setCallerLocal) ←
    expectCore "evaluate frames set caller local" <| ImpLab.setVariable store5 sessionId 3 "outerLocal" "20"
  assertEq "evaluate frames set caller local value" setCallerLocal.value "20"
  assertEq "evaluate frames set caller local variablesReference" setCallerLocal.variablesReference 0
  let evalOuterLocalAfter ← expectCore "evaluate frames caller local after set" <| ImpLab.evaluate store6 sessionId "outerLocal" 1
  assertEq "evaluate frames caller local after set value" evalOuterLocalAfter.result "20"
  let evalInnerXAfter ← expectCore "evaluate frames inner x after caller set" <| ImpLab.evaluate store6 sessionId "x" 0
  assertEq "evaluate frames inner x unchanged" evalInnerXAfter.result "2"

def testDebugCoreEvaluateAndSetVariableNegativePaths : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 5,
      let y := 3
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "evaluate negative launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, _) ← expectCore "evaluate negative initialize x" <| ImpLab.stepIn store1 sessionId
  let evalEmpty := ImpLab.evaluate store2 sessionId ""
  match evalEmpty with
  | .ok _ =>
    throw <| IO.userError "evaluate should reject empty expressions"
  | .error err =>
    assertTrue "evaluate empty expression error" (err.contains "non-empty")
  let evalUnknownVar := ImpLab.evaluate store2 sessionId "missing"
  match evalUnknownVar with
  | .ok _ =>
    throw <| IO.userError "evaluate should reject unknown variables"
  | .error err =>
    assertTrue "evaluate unknown variable error" (err.contains "Unknown variable")
  let evalBadFrame := ImpLab.evaluate store2 sessionId "x" 99
  match evalBadFrame with
  | .ok _ =>
    throw <| IO.userError "evaluate should reject unknown frame ids"
  | .error err =>
    assertTrue "evaluate unknown frame id error" (err.contains "Unknown stack frame id")
  let setMissingRef := ImpLab.setVariable store2 sessionId 0 "x" "10"
  match setMissingRef with
  | .ok _ =>
    throw <| IO.userError "setVariable should require variablesReference > 0"
  | .error err =>
    assertTrue "setVariable missing variablesReference error" (err.contains "variablesReference")
  let setBadFrame := ImpLab.setVariable store2 sessionId 100 "x" "10"
  match setBadFrame with
  | .ok _ =>
    throw <| IO.userError "setVariable should reject unknown frame ids"
  | .error err =>
    assertTrue "setVariable unknown frame id error" (err.contains "Unknown stack frame id")
  let setBadExpr := ImpLab.setVariable store2 sessionId 1 "x" "missing + 1"
  match setBadExpr with
  | .ok _ =>
    throw <| IO.userError "setVariable should reject invalid value expressions"
  | .error err =>
    assertTrue "setVariable invalid value expression error" (err.contains "Unknown variable")

def testDebugCoreExceptionBreakpointsAndInfo : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 10,
      let y := 0,
      let z := div x y
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "exception launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, setExceptions) ←
    expectCore "exception enable exception breakpoints" <| ImpLab.setExceptionBreakpoints store1 sessionId #["runtime"]
  assertEq "exception breakpoints enabled" setExceptions.enabled true
  let (store3, _) ← expectCore "exception step x" <| ImpLab.stepIn store2 sessionId
  let (store4, _) ← expectCore "exception step y" <| ImpLab.stepIn store3 sessionId
  let (store5, exceptionStop) ← expectCore "exception stop at div" <| ImpLab.stepIn store4 sessionId
  assertEq "exception stop reason" exceptionStop.stopReason "exception"
  assertEq "exception stop terminated false" exceptionStop.terminated false
  assertTrue "exception stop description mentions divide by zero"
    ((exceptionStop.description?.map (·.contains "division by zero")).getD false)
  let infoResp ← expectCore "exception info available" <| ImpLab.exceptionInfo store5 sessionId
  assertEq "exception info id" infoResp.exceptionId "divByZero"
  assertTrue "exception info description mentions divide by zero"
    ((infoResp.description?.map (·.contains "division by zero")).getD false)
  let (store6, setExceptionsOff) ←
    expectCore "exception disable breakpoints" <| ImpLab.setExceptionBreakpoints store5 sessionId #[]
  assertEq "exception breakpoints disabled" setExceptionsOff.enabled false
  let nextAfterDisable := ImpLab.stepIn store6 sessionId
  match nextAfterDisable with
  | .ok _ =>
    throw <| IO.userError "stepIn should fail with debug error when exception breakpoints are disabled"
  | .error err =>
    assertTrue "disabled exception breakpoints returns debug error"
      (err.contains "Debug operation failed")

def testDebugCoreExceptionStopKeepsFailureLocationAfterContinue : IO Unit := do
  let info : ProgramInfo := imp%[
    def fail(x) := {
      let zero := 0,
      let out := div x zero,
      return out
    },
    def main() := {
      let seed := 8,
      let out := call fail(seed)
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "exception location launch" <| ImpLab.launchFromProgramInfo store0 info true #[]
  let sessionId := launch.sessionId
  let (store2, _) ←
    expectCore "exception location enable exception breakpoints" <| ImpLab.setExceptionBreakpoints store1 sessionId #["runtime"]
  let (store3, stop) ← expectCore "exception location continue to failure" <| ImpLab.continueExecution store2 sessionId
  assertEq "exception location stop reason" stop.stopReason "exception"
  let stack ← expectCore "exception location stack after continue" <| ImpLab.stackTrace store3 sessionId
  assertEq "exception location stack depth" stack.totalFrames 2
  assertTrue "exception location top frame is failing callee"
    ((stack.stackFrames[0]?.map (·.name.contains "fail")).getD false)

def testDebugCoreTerminatedGuards : IO Unit := do
  let info : ProgramInfo := imp%[
    def main() := {
      let x := 1
    }
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "terminated guards launch" <| ImpLab.launchFromProgramInfo store0 info false #[]
  assertEq "terminated guards launch terminated" launch.terminated true
  let sessionId := launch.sessionId
  let nextAfterTerminated := ImpLab.next store1 sessionId
  let setBpAfterTerminated := ImpLab.setBreakpoints store1 sessionId #[1]
  match nextAfterTerminated with
  | .ok _ =>
    throw <| IO.userError "next should fail on terminated session"
  | .error err =>
    assertTrue "next terminated error mentions state" (err.contains "terminated")
  match setBpAfterTerminated with
  | .ok _ =>
    throw <| IO.userError "setBreakpoints should fail on terminated session"
  | .error err =>
    assertTrue "setBreakpoints terminated error mentions state" (err.contains "terminated")

def testDebugCoreRejectsInvalidProgramInfo : IO Unit := do
  let stmt := Stmt.letConst "x" 1
  let span : StmtSpan := { startLine := 1, startColumn := 0, endLine := 1, endColumn := 10 }
  let invalid : ProgramInfo :=
    { program := { functions := #[{ name := "helper", params := #[], body := #[stmt] }] }
      located := #[{ func := "helper", stmtLine := 1, stmt, span }] }
  let launch := ImpLab.launchFromProgramInfo (store := {}) invalid true #[]
  match launch with
  | .ok _ =>
    throw <| IO.userError "launch should fail with invalid ProgramInfo"
  | .error err =>
    assertTrue "invalid program info launch error mentions main" (err.contains "main")

def testResolveCandidateDeclNames : IO Unit := do
  let unqualified := ImpLab.candidateDeclNames `mainProgram (moduleName? := some `Main)
  assertEq "candidate names include local module and examples"
    unqualified
    #[`mainProgram, `Main.mainProgram, `ImpLab.Lang.Examples.mainProgram]
  let dedup := ImpLab.candidateDeclNames `mainProgram (moduleName? := some `ImpLab.Lang.Examples)
  assertEq "candidate names are deduplicated"
    dedup
    #[`mainProgram, `ImpLab.Lang.Examples.mainProgram]
  let qualified := ImpLab.candidateDeclNames `Main.mainProgram (moduleName? := some `Main)
  assertEq "qualified names stay unchanged" qualified #[`Main.mainProgram]

def runCoreTests : IO Unit := do
  testRunProgram
  testUnboundVariable
  testGlobalGetSetProgram
  testUndeclaredGlobalAccess
  testGlobalHeapPersistsAcrossCall
  testRunFunctionCallProgram
  testUnknownFunctionCall
  testArityMismatch
  testMissingReturn
  testHistoryCursorHelpers
  testTraceShape
  testExplorerNavigation
  testWidgetProps
  testWidgetSessionProjectionAfterStep
  testWidgetHeapProjection
  testStepBackAfterTermination
  testWidgetInitProps
  testWidgetSessionViewJsonRoundtrip
  testDebugSessionContinueAndBreakpoints
  testDebugSessionStepBack
  testDebugCoreStepInOut
  testDebugCoreNextStepsOverCall
  testDslProgram
  testDslNegativeLiteral
  testDslFunctionCall
  testDslGlobals
  testDslProgramInfo
  testProgramInfoValidation
  testDebugCoreFlow
  testDebugCoreHeapScope
  testDebugCoreHeapScopeInCallee
  testDebugCoreSetVariableHeap
  testDebugCoreStackFrames
  testDebugCoreEvaluateAndSetVariable
  testDebugCoreEvaluateAndSetVariableAcrossFrames
  testDebugCoreEvaluateAndSetVariableNegativePaths
  testDebugCoreExceptionBreakpointsAndInfo
  testDebugCoreExceptionStopKeepsFailureLocationAfterContinue
  testDebugCoreTerminatedGuards
  testDebugCoreRejectsInvalidProgramInfo
  testResolveCandidateDeclNames

end ImpLab.Tests
