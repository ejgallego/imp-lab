# dap

Lean 4 toy project for:
- an arithmetic let-normal-form interpreter,
- step-by-step execution traces,
- an interactive infoview widget to inspect state and move backward/forward.

## Project layout

- `Dap/Syntax.lean`: core AST (`Program` is a list of `let` statements).
- `Dap/Eval.lean`: environment, small-step semantics, and full program runner.
- `Dap/Trace.lean`: execution trace and navigation API (`Explorer`).
- `Dap/Widget.lean`: widget props + `traceExplorerWidget`.
- `Dap/Examples.lean`: sample program and precomputed widget props.
- `Test/Main.lean`: executable tests.
- `DAP_PLAN.md`: roadmap to expose this runtime over DAP.

## Build and run

```bash
lake build
lake exe dap
lake exe dap-tests
```

## Widget usage

In a Lean file:

```lean
import Dap.Examples

#widget Dap.traceExplorerWidget with Dap.Examples.sampleTraceProps
```

Place the cursor on the `#widget` command in the infoview to interact with:
- `Back` / `Forward` navigation over recorded states,
- highlighted current instruction (`pc`),
- environment bindings at each step.
