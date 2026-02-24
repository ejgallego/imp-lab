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
- ProgramInfo loading/export:
  - `ImpLab/Debugger/DAP/ProgramInfoLoader.lean`
  - `ImpLab/Debugger/DAP/Export.lean`
  - `app/ExportMain.lean`

## Architecture guardrails

- Put debugger behavior in `ImpLab/Debugger/Core.lean` first.
- Keep transport files as adapters only.
- Treat `ProgramInfo` as canonical across launch/debug/export.
- Keep source mapping coherent (`func` + `stmtLine` <-> source line).
- Preserve DAP payload shape stability and lifecycle ordering.

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
- `source` is optional and affects source display in stack frames.
- `toydapPath` and `toydapArgs` are optional adapter controls.

Export example:

```bash
lake exe dap-export --decl ImpLab.Lang.Examples.mainProgram --out .dap/programInfo.generated.json
```

`--decl` must resolve to an `ImpLab.ProgramInfo` declaration.

## Lifecycle and behavior checks

- Event order must remain coherent (`initialized`, `stopped`, `continued`, `terminated`).
- Breakpoint verification and stack-frame/source mapping must remain function-aware.
- `Program` stays function-only with required `main()`.

## Tests

```bash
lake build
lake exe dap-tests
cd client && npm run compile
```

Key tests:

- `Test/Core.lean`: core behavior and debugger semantics.
- `Test/Transport.lean`: stdio framing, request wiring, lifecycle and breakpoint sanity.
