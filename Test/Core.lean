/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Test.Util

open ImpLab
open Lean

namespace ImpLab.Tests

private def mkProgram (mainBody : Array Stmt) (helpers : Array FuncDef := #[]) : Program :=
  { functions := #[{ name := Program.mainName, params := #[], body := mainBody }] ++ helpers }

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
  assertEq "stack frames top scope count" scopesTop.scopes.size 1
  let varsTop ← expectCore "stack frames vars top" <| ImpLab.variables store4 sessionId 1
  assertTrue "stack frames top vars include x"
    (varsTop.variables.any fun v => v.name == "x" && v.value == "2")
  let varsCaller ← expectCore "stack frames vars caller" <| ImpLab.variables store4 sessionId 2
  assertTrue "stack frames caller vars include a"
    (varsCaller.variables.any fun v => v.name == "a" && v.value == "2")

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
  testRunFunctionCallProgram
  testUnknownFunctionCall
  testArityMismatch
  testMissingReturn
  testHistoryCursorHelpers
  testTraceShape
  testExplorerNavigation
  testWidgetProps
  testWidgetSessionProjectionAfterStep
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
  testDslProgramInfo
  testProgramInfoValidation
  testDebugCoreFlow
  testDebugCoreStackFrames
  testDebugCoreTerminatedGuards
  testDebugCoreRejectsInvalidProgramInfo
  testResolveCandidateDeclNames

end ImpLab.Tests
