import Dap.Widget
import Dap.Surface
import Dap.Server

namespace Dap.Examples

open Dap

def sampleProgramInfo : ProgramInfo := dapInfo%[
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

end Dap.Examples

#widget Dap.traceExplorerWidget with Dap.Examples.sampleTraceProps
