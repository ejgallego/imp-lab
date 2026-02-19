import Dap

open Dap

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

end Dap.Tests

def main : IO Unit := do
  Dap.Tests.testRunProgram
  Dap.Tests.testUnboundVariable
  Dap.Tests.testTraceShape
  Dap.Tests.testExplorerNavigation
  Dap.Tests.testWidgetProps
  IO.println "All tests passed."
