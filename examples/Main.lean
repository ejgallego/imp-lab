/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Dap.Widget.Server
import Dap.Lang.Dsl
import Dap.DAP.Server

namespace Dap.Lang.Examples

open Dap

def sampleProgramInfo : ProgramInfo := dap%[
  let x := 6,
  let y := 3,
  let z := 4,
  let sum := add x y,
  let prod := mul sum y,
  let quot := div prod x
]

def mainProgram : Program :=
  sampleProgramInfo

def mainProgramInfo : ProgramInfo :=
  sampleProgramInfo

def sampleProgram : Program :=
  mainProgram

def sampleFinalContext : Except EvalError Context :=
  run mainProgram

def sampleTraceProps : TraceWidgetProps :=
  match traceWidgetProps mainProgram with
  | .ok props => props
  | .error _ => default

end Dap.Lang.Examples

#widget Dap.traceExplorerWidget with Dap.Lang.Examples.sampleTraceProps
