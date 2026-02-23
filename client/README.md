# Lean Toy DAP Client (VS Code)

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

Then package/sideload as you usually do (e.g. `vsce package` and install VSIX).

## Launch configuration

Use debug type `lean-toy-dap`.

`source`:
- Optional source path shown in stack traces.

Entry point:
- If neither `program` nor `programFile` are provided, `entryPoint` defaults to `mainProgramInfo`.
- `entryPoint` is resolved as a Lean declaration name (supports both `Dap.ProgramInfo` and `Dap.Program`; unqualified names also try `Main.<name>` and `Dap.Lang.Examples.<name>`).

Adapter executable:
- `toydapPath` (optional): explicit path to the `toydap` binary.
- `toydapArgs` (optional): extra command-line arguments.

Example:

```json
{
  "name": "Lean Toy DAP",
  "type": "lean-toy-dap",
  "request": "launch",
  "source": "${file}",
  "toydapPath": "${workspaceFolder}/.lake/build/bin/toydap",
  "entryPoint": "mainProgramInfo",
  "stopOnEntry": true
}
```

Program launch mode:
- `program` (inline JSON array), or
- `programFile` (path to JSON file containing either that array or a full `ProgramInfo` object), or
- `programInfo` / `programInfoFile` (`ProgramInfo` payload with source spans).

A ready-to-run sample is included at `client/program.sample.json`.
For source-line-accurate breakpoints/stack traces, use `client/programInfo.sample.json`.
You can also auto-generate `ProgramInfo` JSON via `lake exe dap-export` (see repository `README.md`).

## Program JSON format

Each statement is:
- `{"dest":"x","rhs":{"const":{"value":6}}}` or
- `{"dest":"z","rhs":{"bin":{"op":"add","lhs":"x","rhs":"y"}}}`

Operator values: `add`, `sub`, `mul`, `div`.

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
- `stepBack`
- `continue`
- `pause`
- `disconnect`

## Notes

- Breakpoints are interpreted as statement lines in the toy program (1-based).
- Variables scope is currently a single `locals` scope.
