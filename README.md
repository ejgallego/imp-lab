# dap

Lean 4 toy project for:
- an arithmetic let-normal-form interpreter,
- step-by-step execution traces,
- an interactive infoview widget to inspect state and move backward/forward,
- a DAP-style debug service exposed as Lean RPC methods,
- a standalone DAP adapter binary (`toydap`) that speaks DAP over stdio,
- a side-loadable VS Code debug client in `client/`.

## Project layout

- `Dap/Syntax.lean`: core AST (`Program` is a list of `let` statements).
- `Dap/Surface.lean`: DSL syntax/macros (`dap%[...]`, `dapInfo%[...]`) + infotree metadata.
- `Dap/Eval.lean`: environment, small-step semantics, and full program runner.
- `Dap/Trace.lean`: execution trace and navigation API (`Explorer`).
- `Dap/DebugModel.lean`: pure debugger session model (breakpoints, continue, next, stepBack).
- `Dap/Server.lean`: Lean server RPC endpoints implementing DAP-like operations.
- `Dap/ToyDap.lean`: standalone DAP adapter implementation (native DAP protocol over stdio).
- `Dap/Widget.lean`: widget props + `traceExplorerWidget`.
- `Dap/Examples.lean`: sample program and precomputed widget props.
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

The toy language now has Lean syntax categories and term elaborators:

- `dap%[...] : Dap.Program`
- `dapInfo%[...] : Dap.ProgramInfo`

Both forms use statements of the shape:

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

`dapInfo%[...]` preserves per-statement source spans in `ProgramInfo.located`.

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
- define `mainProgram : Dap.Program`,
- launch `lean-toy-dap` with `source = ${file}` and `entryPoint = "mainProgram"`.

## Infotree metadata

When elaborating `dap%[...]` or `dapInfo%[...]`, the elaborator stores custom infotree nodes carrying the statement-location payload.

Use `Dap.getProgramSyntaxInfo?` to decode these custom nodes from `Elab.Info` entries when building source-aware tooling.

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

## VS Code side-load client

The `client/` folder contains an extension that exposes debug type `lean-toy-dap` and launches the standalone `toydap` executable.

See `client/README.md` for build/sideload steps and launch configuration details.
`toydap` can consume either `Program` JSON or `ProgramInfo` JSON (`located` spans) for source-line-aware breakpoint/stack-trace mapping.

## ProgramInfo export helper

Use `dap-export` to generate source-aware JSON from a Lean declaration:

```bash
lake exe dap-export --decl Dap.Examples.mainProgramInfo --out .dap/programInfo.generated.json
```

The repository `.vscode/launch.json` includes a config that runs this command as a `preLaunchTask`.
