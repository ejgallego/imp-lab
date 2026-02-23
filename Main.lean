/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Dap
import examples.Main

open Dap

private def renderBindings (ctx : Context) : String :=
  let parts := ctx.bindings.toList.map (fun (name, value) => s!"{name} = {value}")
  String.intercalate ", " parts

def main : IO Unit := do
  match Dap.Lang.Examples.sampleFinalContext with
  | .error err =>
    IO.eprintln s!"Evaluation failed: {err}"
  | .ok ctx =>
    IO.println s!"Final context (pc={ctx.pc}): {renderBindings ctx}"
