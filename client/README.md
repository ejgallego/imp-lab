# ImpLab Toy DAP Client (VS Code)

This extension starts the standalone `toydap` debug adapter binary built from this repository.

## Prerequisites

- Build `toydap`:
  ```bash
  lake build toydap
  ```
- By default, the extension looks for:
  - `${workspaceFolder}/.lake/build/bin/toydap`, then
  - `toydap` in `PATH`.

## Build and sideload

```bash
cd client
npm install
npm run compile
```

Then package/sideload as you usually do (for example `vsce package` and install VSIX).

## Launch configuration

Use debug type `lean-toy-dap`.

`source`:
- Optional source path shown in stack traces.

Launch payload:
- `programInfo`: `ImpLab.ProgramInfo` JSON payload.
- If omitted, the extension tries `${workspaceFolder}/.dap/programInfo.generated.json`.
- The extension does not auto-load `client/programInfo.sample.json`; that file is only a reference shape.

Adapter executable:
- `toydapPath` (optional): explicit path to the `toydap` binary.
- `toydapArgs` (optional): extra command-line arguments.

Example:

```json
{
  "name": "ImpLab Toy DAP",
  "type": "lean-toy-dap",
  "request": "launch",
  "source": "${file}",
  "toydapPath": "${workspaceFolder}/.lake/build/bin/toydap",
  "programInfo": {
    "...": "ImpLab.ProgramInfo JSON"
  },
  "stopOnEntry": true
}
```

For JSON payload shape, see `client/programInfo.sample.json`.
You can also generate `ProgramInfo` JSON via:

```bash
lake exe dap-export --decl ImpLab.Lang.Examples.mainProgram --out .dap/programInfo.generated.json
```

## Supported DAP requests

Handled by `toydap`:
- `initialize`
- `launch`
- `setBreakpoints`
- `configurationDone`
- `threads`
- `stackTrace`
- `scopes`
- `variables`
- `next`
- `stepIn`
- `stepOut`
- `stepBack`
- `continue`
- `pause`
- `disconnect`

## Notes

- Breakpoints are line-based and resolved through function-aware `ProgramInfo` locations.
- `setBreakpoints` accepts both `breakpoints[*].line` and legacy `lines[]` payloads.
- Variables are exposed as a `locals` scope per selected stack frame.
