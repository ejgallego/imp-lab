import Dap.Widget

namespace Dap.Examples

open Dap

def sampleProgram : Program :=
  #[
    Stmt.letConst "x" 6,
    Stmt.letConst "y" 3,
    Stmt.letBin "sum" .add "x" "y",
    Stmt.letBin "prod" .mul "sum" "y",
    Stmt.letBin "quot" .div "prod" "x"
  ]

def sampleFinalContext : Except EvalError Context :=
  run sampleProgram

def sampleTraceProps : TraceWidgetProps :=
  match traceWidgetProps sampleProgram with
  | .ok props => props
  | .error _ => default

end Dap.Examples

#widget Dap.traceExplorerWidget with Dap.Examples.sampleTraceProps
