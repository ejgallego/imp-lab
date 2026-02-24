# Debugger

This document contains debugger-specific architecture, launch flow, and protocol details.

## Components

- Core semantics:
  - `ImpLab/Debugger/Core.lean`
  - `ImpLab/Debugger/Session.lean`
- StdIO DAP transport:
  - `ImpLab/Debugger/DAP/Stdio.lean`
  - `app/ToyDap.lean`
- Lean widget transport:
  - `ImpLab/Debugger/Widget/Server.lean`
  - `ImpLab/Debugger/Widget/UI.lean`
  - `ImpLab/Debugger/Widget/Types.lean`
- ProgramInfo export:
  - `ImpLab/Debugger/DAP/Export.lean`
  - `app/ExportMain.lean`

## Architecture guardrails

- Put debugger behavior in `ImpLab/Debugger/Core.lean` first.
- Keep transport files as adapters only.
- Treat `ProgramInfo` as canonical across launch/debug/export.
- Keep source mapping coherent (`func` + `stmtLine` <-> source line).
- Preserve DAP payload shape stability and lifecycle ordering.

## VS Code quick start

1. Build Lean artifacts:

```bash
lake build
```

2. Install client dependencies (first time only):

```bash
cd client && npm i
```

3. Start the extension dev host:

```bash
code client
```

4. In that VS Code window, press `F5` and run one of:
- `Run ImpLab Toy DAP Extension (watch)`
- `Run ImpLab Toy DAP Extension (compile once)`

5. In the Extension Development Host:
- open this repository,
- open `examples/Main.lean`,
- run `ImpLab Toy DAP (auto-export ProgramInfo)` from `.vscode/launch.json`.

## VS Code launch flow

1. Run `ImpLab Toy DAP (auto-export ProgramInfo)` from `.vscode/launch.json`.
2. `preLaunchTask` runs `dap-export` to write `.dap/programInfo.generated.json`.
3. Extension launches `toydap` (`.lake/build/bin/toydap` by default).
4. If `programInfo` is missing inline, extension loads `.dap/programInfo.generated.json`.

Minimal launch config:

```json
{
  "name": "ImpLab Toy DAP",
  "type": "lean-toy-dap",
  "request": "launch",
  "source": "${file}",
  "stopOnEntry": true
}
```

## Launch contract

- `programInfo` is required at launch.
- Launch fails if neither inline `programInfo` nor valid `.dap/programInfo.generated.json` is available.
- `source` is optional and affects source display in stack frames.
- `toydapPath` and `toydapArgs` are optional adapter controls.

## Supported DAP requests

Currently handled by `toydap`:

- `initialize`
- `launch`
- `setBreakpoints`
- `setExceptionBreakpoints`
- `configurationDone`
- `threads`
- `stackTrace`
- `scopes`
- `variables`
- `evaluate`
- `setVariable`
- `exceptionInfo`
- `next`
- `stepIn`
- `stepOut`
- `stepBack`
- `continue`
- `pause`
- `disconnect`
- `terminate`

## Current caveats

- `evaluate` is intentionally minimal:
  - Supports variable lookup, integer literals, and simple binary forms (`x + 1`, `add x y`).
  - More complex expression parsing is deferred.
- Exception breakpoint filters currently act as an enable/disable toggle:
  - Non-empty `filters` enables exception stops.
  - Empty `filters` disables exception stops.
  - Filter-specific behavior is not implemented yet.
- Source introspection requests (`loadedSources`, `source`) remain deferred until we need stronger virtual/generated source workflows.

Export examples:

```bash
lake exe dap-export --decl ImpLab.Lang.Examples.mainProgram --out .dap/programInfo.generated.json
lake exe dap-export --decl MyProject.Debugger.lesson1Program --out .dap/programInfo.generated.json
```

`--decl` must resolve to an `ImpLab.ProgramInfo` declaration.

## Execution model

The interpreter uses explicit call frames:
- each frame has function name, local environment, and program counter,
- runtime state also includes a persistent global heap initialized from top-level `global` declarations,
- `call` pushes a frame,
- `return` pops and assigns into caller destination,
- `get` reads a heap cell into locals and `set` updates a heap cell,
- stepping (`step`) is the semantic foundation for runtime and debugger behavior.

## Scopes and variables

- `scopes` returns two scopes per stack frame:
  - `locals`: frame-local environment
  - `heap`: global mutable variables
- `variables` resolves references for either locals or heap using core-level logic in `ImpLab/Debugger/Core.lean`.

## Widget RPC methods

Registered in `ImpLab.Debugger.Widget.Server`:
- `ImpLab.Debugger.Widget.Server.widgetLaunch`
- `ImpLab.Debugger.Widget.Server.widgetStepIn`
- `ImpLab.Debugger.Widget.Server.widgetStepBack`
- `ImpLab.Debugger.Widget.Server.widgetContinue`
- `ImpLab.Debugger.Widget.Server.widgetDisconnect`

`widgetLaunch` accepts `programInfo` plus optional `stopOnEntry` and `breakpoints`.

## Lifecycle and behavior checks

- Event order must remain coherent (`initialized`, `stopped`, `continued`, `terminated`).
- Breakpoint verification and stack-frame/source mapping must remain function-aware.
- `Program` must include `main()` and may include top-level global declarations.

## Roadmap

Current debugger priorities and milestones:
- `docs/debugger-roadmap.md`

## Tests

```bash
lake build
lake exe dap-tests
cd client && npm run compile
```

Key tests:
- `Test/Core.lean`: core behavior and debugger semantics.
- `Test/Transport.lean`: stdio framing, request wiring, lifecycle and breakpoint sanity.
