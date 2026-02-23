# dap

Lean 4 toy project for:
- an arithmetic let-normal-form interpreter,
- step-by-step execution traces,
- an interactive infoview widget to inspect state and move backward/forward,
- a DAP-style debug service exposed as Lean RPC methods,
- a standalone DAP adapter binary (`toydap`) that speaks DAP over stdio,
- a side-loadable VS Code debug client in `client/`.

## Project layout

- `Dap/Lang/Ast.lean`: core AST (`Program` is a list of `let` statements).
- `Dap/Lang/Dsl.lean`: DSL syntax/macros (`dap%[...]`) + infotree metadata.
- `Dap/Lang/Eval.lean`: environment, small-step semantics, and full program runner.
- `Dap/Lang/History.lean`: shared cursor/history navigation helpers.
- `Dap/Lang/Trace.lean`: execution trace and navigation API (`Explorer`).
- `Dap/Lang/Examples.lean`: sample program and precomputed widget props.
- `Dap/DAP/Session.lean`: pure debugger session model (breakpoints, continue, next, stepBack).
- `Dap/DAP/Core.lean`: session store + DAP-shaped pure core operations.
- `Dap/DAP/Server.lean`: Lean server RPC endpoints implementing DAP-like operations.
- `Dap/DAP/Stdio.lean`: standalone DAP adapter implementation (native DAP protocol over stdio).
- `Dap/Widget/Types.lean`: pure widget data model + trace-to-widget projection helpers.
- `Dap/Widget/Server.lean`: widget props + `traceExplorerWidget`.
- `Dap/DAP/Export.lean`: `dap-export` declaration loader/export logic.
- `Test/Main.lean`: executable tests.
- `client/`: VS Code extension scaffold for side-loading (`lean-toy-dap` debug type).
- `DAP_PLAN.md`: roadmap to expose this runtime over DAP.

## Build and run

```bash
lake build
lake exe dap
lake exe toydap
lake exe dap-export --help
lake exe dap-tests
```

## DSL syntax and coloring

The toy language now has Lean syntax categories and one term elaborator:

- `dap%[...] : Dap.ProgramInfo` (coerces to `Dap.Program` when needed)

The DSL uses statements of the shape:

```lean
let v := N
let v := add v1 v2
let v := sub v1 v2
let v := mul v1 v2
let v := div v1 v2
```

Example:

```lean
def p : Dap.Program := dap%[
  let x := 10,
  let y := 2,
  let z := div x y
]
```

Keywords/operators (`let`, `add`, `sub`, `mul`, `div`) and literals/idents are tokenized through Lean's parser, so they receive syntax highlighting in editors.

`dap%[...]` preserves per-statement source spans in `ProgramInfo.located`.

## Execution model

The debugger now follows a stepper-first design:
- the interpreter exposes single-step execution (`step`) as the semantic foundation,
- the debug engine drives execution step-by-step and keeps history for reverse stepping (`stepBack`).

This keeps interpreter semantics explicit and is easier to teach as the base language grows.

Trace utilities (`Dap/Lang/Trace.lean`) are still available for visualization and analysis.
If there is user demand for a full trace-first debug mode, we would welcome a PR adding that setup.

## Lean RPC debug methods

The following RPC methods are registered in `Dap.Server`:

- `Dap.Server.dapInitialize`
- `Dap.Server.dapLaunch`
- `Dap.Server.dapLaunchMain`
- `Dap.Server.dapSetBreakpoints`
- `Dap.Server.dapThreads`
- `Dap.Server.dapStackTrace`
- `Dap.Server.dapScopes`
- `Dap.Server.dapVariables`
- `Dap.Server.dapNext`
- `Dap.Server.dapStepBack`
- `Dap.Server.dapContinue`
- `Dap.Server.dapPause`
- `Dap.Server.dapDisconnect`

They are designed to be called over Lean's `$/lean/rpc/call` transport.

`dapLaunchMain` is the preferred entry for the VS Code prototype flow:
- open a Lean file,
- define `mainProgram : Dap.Program` (or `Dap.ProgramInfo`),
- launch `lean-toy-dap` with `source = ${file}` and `entryPoint = "mainProgram"`.

`entryPoint` resolves declarations dynamically (unqualified names also try `Dap.Lang.Examples.<name>`). The declaration may be either `Dap.Program` or `Dap.ProgramInfo`.

## Infotree metadata

When elaborating `dap%[...]`, the elaborator stores custom infotree nodes carrying the statement-location payload.

Use `Dap.getProgramSyntaxInfo?` to decode these custom nodes from `Elab.Info` entries when building source-aware tooling.

## Widget usage

In a Lean file:

```lean
import Dap.Lang.Examples

#widget Dap.traceExplorerWidget with Dap.Lang.Examples.sampleTraceProps
```

Place the cursor on the `#widget` command in the infoview to interact with:
- `Back` / `Forward` navigation over recorded states,
- highlighted current instruction (`pc`),
- environment bindings at each step.

## VS Code side-load client

The `client/` folder contains an extension that exposes debug type `lean-toy-dap` and launches the standalone `toydap` executable.

See `client/README.md` for build/sideload steps and launch configuration details.
`toydap` can consume either `Program` JSON or `ProgramInfo` JSON (`located` spans) for source-line-aware breakpoint/stack-trace mapping.

## ProgramInfo export helper

Use `dap-export` to generate source-aware JSON from a Lean declaration:

```bash
lake exe dap-export --decl Dap.Lang.Examples.mainProgramInfo --out .dap/programInfo.generated.json
```

`--decl` also accepts `Dap.Program` declarations; these are exported as `ProgramInfo` with empty `located` spans.
Default is `--decl mainProgram`.
For unqualified names, resolution tries `<name>` and then `Dap.Lang.Examples.<name>`.

The repository `.vscode/launch.json` includes a config that runs this command as a `preLaunchTask`.
