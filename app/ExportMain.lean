/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import ImpLab.Debugger.DAP.Export

def main (args : List String) : IO Unit := do
  ImpLab.Export.run args
