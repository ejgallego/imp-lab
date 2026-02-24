/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import ImpLab.Debugger.Widget.UI
import ImpLab.Lang.Dsl
import ImpLab.Debugger.Widget.Server

namespace ImpLab.Lang.Examples

open ImpLab

def mainProgram : ProgramInfo := imp%[
  global lastOut := 0,
  global uh := 1,
  def bump(x) := {
    let one := 1,
    let out := add x one,
    return out
  },
  def scaleAndShift(x, factor) := {
    let scaled := mul x factor,
    let den := get uh,
    let mayfail := div scaled den,
    let shift := 2,
    let out := add scaled shift,
    return out
  },
  def main() := {
    let seed := 5,
    let factor := 3,
    let bumped := call bump(seed),
    let out := call scaleAndShift(bumped, factor),
    set lastOut := out,
    let heapOut := get lastOut
  }
]

#eval run mainProgram

def sampleTraceProps : TraceWidgetInitProps :=
  { programInfo := mainProgram }

def sampleTracePropsJson : Lean.Json :=
  Lean.toJson sampleTraceProps

end ImpLab.Lang.Examples

#widget ImpLab.traceExplorerWidget with ImpLab.Lang.Examples.sampleTracePropsJson
