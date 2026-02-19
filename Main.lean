import Dap
import Dap.Examples

open Dap

private def renderBindings (ctx : Context) : String :=
  let parts := ctx.bindings.toList.map (fun (name, value) => s!"{name} = {value}")
  String.intercalate ", " parts

def main : IO Unit := do
  match Examples.sampleFinalContext with
  | .error err =>
    IO.eprintln s!"Evaluation failed: {err}"
  | .ok ctx =>
    IO.println s!"Final context (pc={ctx.pc}): {renderBindings ctx}"
