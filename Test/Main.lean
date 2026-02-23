/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Dap

open Dap
open Lean

namespace Dap.Tests

def assertEq [BEq α] [ToString α] (label : String) (actual expected : α) : IO Unit := do
  if actual == expected then
    pure ()
  else
    throw <| IO.userError s!"{label}: expected `{expected}`, got `{actual}`"

def assertSomeEq [BEq α] [ToString α] (label : String) (actual : Option α) (expected : α) : IO Unit := do
  match actual with
  | some value => assertEq label value expected
  | none => throw <| IO.userError s!"{label}: expected `some {expected}`, got `none`"

def assertTrue (label : String) (condition : Bool) : IO Unit := do
  if condition then
    pure ()
  else
    throw <| IO.userError s!"{label}: expected condition to hold"

def testRunProgram : IO Unit := do
  let program : Program :=
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
  let program : Program :=
    #[
      Stmt.letBin "z" .add "x" "y"
    ]
  match run program with
  | .ok _ =>
    throw <| IO.userError "testUnboundVariable failed: expected an error"
  | .error err =>
    assertEq "unbound variable error" err (.unboundVar "x")

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
  let program : Program :=
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
  let program : Program :=
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
  let program : Program :=
    #[
      Stmt.letConst "x" 2,
      Stmt.letConst "y" 8,
      Stmt.letBin "z" .div "y" "x"
    ]
  match traceWidgetProps program with
  | .error err =>
    throw <| IO.userError s!"testWidgetProps failed: {err}"
  | .ok props =>
    assertEq "widget program length" props.program.size program.size
    assertEq "widget states length" props.states.size (program.size + 1)
    assertEq "widget last pc" ((props.states[props.states.size - 1]?.map (·.pc)).getD 0) program.size

def testDebugSessionContinueAndBreakpoints : IO Unit := do
  let program : Program :=
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
    let session := session.setBreakpoints #[3]
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
  let program : Program :=
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

def testDslProgram : IO Unit := do
  let program : Program := dap%[
    let x := 6,
    let y := 7,
    let z := add x y
  ]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testDslProgram failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl result" (ctx.lookup? "z") 13

def testDslNegativeLiteral : IO Unit := do
  let program : Program := dap%[
    let x := -6,
    let y := 2,
    let z := add x y
  ]
  match run program with
  | .error err =>
    throw <| IO.userError s!"testDslNegativeLiteral failed: {err}"
  | .ok ctx =>
    assertSomeEq "dsl negative literal result" (ctx.lookup? "z") (-4)

def testDslProgramInfo : IO Unit := do
  let info : ProgramInfo := dap%[
    let a := 1,
    let b := 2,
    let c := mul a b
  ]
  assertEq "programInfo size" info.program.size 3
  assertEq "programInfo located size" info.located.size 3
  let line0 := (info.located[0]?.map (·.span.startLine)).getD 0
  assertSomeEq "line maps to first stmt" (info.lineToStmtIdx? line0) 0
  assertTrue "statement spans have valid line range"
    (info.located.all fun located => located.span.startLine ≤ located.span.endLine)
  let firstStmtLine := (ProgramInfo.sourceLineToStmtLine? info.stmtSpans line0).getD 0
  assertEq "source->stmt line mapping" firstStmtLine 1
  let firstSourceLine := ProgramInfo.stmtLineToSourceLine info.stmtSpans 1
  assertEq "stmt->source line mapping" firstSourceLine line0

def testProgramInfoValidation : IO Unit := do
  let stmt0 := Stmt.letConst "x" 1
  let stmt1 := Stmt.letConst "y" 2
  let span : StmtSpan := { startLine := 1, startColumn := 0, endLine := 1, endColumn := 10 }
  let badInfo : ProgramInfo :=
    { program := #[stmt0, stmt1]
      located := #[{ stmt := stmt0, span }] }
  match badInfo.validate with
  | .ok _ =>
    throw <| IO.userError "testProgramInfoValidation should reject mismatched ProgramInfo"
  | .error err =>
    assertTrue "programInfo mismatch error mentions located size" (err.contains "located")
  let compatInfo : ProgramInfo := { program := #[stmt0, stmt1], located := #[] }
  match compatInfo.validate with
  | .ok _ => pure ()
  | .error err =>
    throw <| IO.userError s!"ProgramInfo compatibility mode should validate: {err}"

private def expectCore (label : String) (result : Except String α) : IO α := do
  match result with
  | .ok value =>
    pure value
  | .error err =>
    throw <| IO.userError s!"{label}: {err}"

def testDebugCoreFlow : IO Unit := do
  let program : Program := dap%[
    let x := 5,
    let y := 7,
    let z := add x y
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "core launch" <| Dap.launchFromProgram store0 program false #[2]
  assertEq "core launch stopReason" launch.stopReason "breakpoint"
  assertEq "core launch line" launch.line 2
  let sessionId := launch.sessionId
  let vars1 ← expectCore "core vars" <| Dap.variables store1 sessionId 1
  assertTrue "core vars contain x binding"
    (vars1.variables.any fun v => v.name == "x" && v.value == "5")
  let (store2, cont) ← expectCore "core continue" <| Dap.continueExecution store1 sessionId
  assertEq "core continue terminated" cont.terminated true
  assertEq "core continue stopReason" cont.stopReason "terminated"
  let (store3, disconnected) := Dap.disconnect store2 sessionId
  assertEq "core disconnect" disconnected true
  let pauseAfterDisconnect := Dap.pause store3 sessionId
  match pauseAfterDisconnect with
  | .ok _ =>
    throw <| IO.userError "core pause after disconnect should fail"
  | .error _ =>
    pure ()

def testDebugCoreTerminatedGuards : IO Unit := do
  let program : Program := dap%[
    let x := 1
  ]
  let store0 : SessionStore := {}
  let (store1, launch) ← expectCore "terminated guards launch" <| Dap.launchFromProgram store0 program false #[]
  assertEq "terminated guards launch terminated" launch.terminated true
  let sessionId := launch.sessionId
  let nextAfterTerminated := Dap.next store1 sessionId
  let setBpAfterTerminated := Dap.setBreakpoints store1 sessionId #[1]
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

def testDebugCoreRejectsIncompatibleStmtSpans : IO Unit := do
  let program : Program :=
    #[
      Stmt.letConst "x" 1,
      Stmt.letConst "y" 2
    ]
  let stmtSpans : Array StmtSpan :=
    #[{ startLine := 1, startColumn := 0, endLine := 1, endColumn := 10 }]
  let launch := Dap.launchFromProgram (store := {}) program true #[] stmtSpans
  match launch with
  | .ok _ =>
    throw <| IO.userError "launch should fail with incompatible statement spans"
  | .error err =>
    assertTrue "incompatible span error mentions statement spans" (err.contains "statement spans")

private def encodeDapRequest (seq : Nat) (command : String) (arguments : Json := Json.mkObj []) : String :=
  let payload := Json.mkObj
    [ ("seq", toJson seq),
      ("type", toJson "request"),
      ("command", toJson command),
      ("arguments", arguments) ]
  let data := Lean.Json.compress payload
  s!"Content-Length: {String.utf8ByteSize data}\r\n\r\n{data}"

private def runToyDapPayload (name : String) (stdinPayload : String) : IO String := do
  let inputPath := s!".dap/{name}.stdin"
  IO.FS.createDirAll ".dap"
  IO.FS.writeFile inputPath stdinPayload
  let out ← IO.Process.output
    { cmd := "bash"
      args := #["-lc", s!".lake/build/bin/toydap < {inputPath}"] }
  try
    IO.FS.removeFile inputPath
  catch _ =>
    pure ()
  assertEq s!"toydap ({name}) exits cleanly" out.exitCode 0
  pure out.stdout

private def appearsBefore (s first second : String) : Bool :=
  match s.splitOn first with
  | [] =>
    false
  | _ :: tail =>
    let afterFirst := String.intercalate first tail
    afterFirst.contains second

def testToyDapProtocolSanity : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| Json.mkObj
          [ ("entryPoint", toJson "mainProgram"),
            ("stopOnEntry", toJson true) ],
        encodeDapRequest 3 "threads",
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.sanity" stdinPayload
  assertTrue "initialize response present"
    (stdout.contains "\"request_seq\":1" && stdout.contains "\"command\":\"initialize\"")
  assertTrue "initialized event present"
    (stdout.contains "\"event\":\"initialized\"")
  assertTrue "launch response present"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"launch\"")
  assertTrue "threads response present"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"threads\"")
  assertTrue "next response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"next\"")
  assertTrue "disconnect response present"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"disconnect\"")

def testToyDapBreakpointProtocol : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setBreakpoints" <| Json.mkObj
          [ ("breakpoints", Json.arr #[Json.mkObj [("line", toJson (3 : Nat))]]) ],
        encodeDapRequest 3 "launch" <| Json.mkObj
          [ ("entryPoint", toJson "mainProgram"),
            ("stopOnEntry", toJson false) ],
        encodeDapRequest 4 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.breakpoint" stdinPayload
  assertTrue "setBreakpoints response present"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"setBreakpoints\"")
  assertTrue "pre-launch breakpoint stays pending"
    (stdout.contains "\"verified\":false")
  assertTrue "launch response present in breakpoint flow"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"launch\"")
  if stdout.contains "\"event\":\"stopped\"" && stdout.contains "\"reason\":\"breakpoint\"" then
    pure ()
  else
    throw <| IO.userError s!"breakpoint stop reason missing in output: {stdout}"

def testToyDapContinueEventOrder : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| Json.mkObj
          [ ("entryPoint", toJson "mainProgram"),
            ("stopOnEntry", toJson true) ],
        encodeDapRequest 3 "continue",
        encodeDapRequest 4 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.continue" stdinPayload
  let continuedMarker := "\"event\":\"continued\""
  let continueRespMarker := "\"request_seq\":3"
  assertTrue "continued event is emitted" (stdout.contains continuedMarker)
  assertTrue "continue response is emitted" (stdout.contains continueRespMarker)
  let hasStopOrTerm :=
    stdout.contains "\"event\":\"stopped\"" || stdout.contains "\"event\":\"terminated\""
  assertTrue "post-continue stop/terminate event is emitted" hasStopOrTerm
  assertTrue "continued precedes continue response"
    (appearsBefore stdout continuedMarker continueRespMarker)
  assertTrue "stop/terminate follows continue response"
    (appearsBefore stdout continueRespMarker "\"event\":\"stopped\"" ||
      appearsBefore stdout continueRespMarker "\"event\":\"terminated\"")

end Dap.Tests

def main : IO Unit := do
  Dap.Tests.testRunProgram
  Dap.Tests.testUnboundVariable
  Dap.Tests.testHistoryCursorHelpers
  Dap.Tests.testTraceShape
  Dap.Tests.testExplorerNavigation
  Dap.Tests.testWidgetProps
  Dap.Tests.testDebugSessionContinueAndBreakpoints
  Dap.Tests.testDebugSessionStepBack
  Dap.Tests.testDslProgram
  Dap.Tests.testDslNegativeLiteral
  Dap.Tests.testDslProgramInfo
  Dap.Tests.testProgramInfoValidation
  Dap.Tests.testDebugCoreFlow
  Dap.Tests.testDebugCoreTerminatedGuards
  Dap.Tests.testDebugCoreRejectsIncompatibleStmtSpans
  Dap.Tests.testToyDapProtocolSanity
  Dap.Tests.testToyDapBreakpointProtocol
  Dap.Tests.testToyDapContinueEventOrder
  IO.println "All tests passed."
