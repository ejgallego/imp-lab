# imp-lab

Lean 4 toy debugger project with:
- a small function-based toy language,
- a pure debugger core,
- a standalone DAP adapter (`toydap`),
- a VS Code side-load client (`client/`),
- Lean widget RPC endpoints for infoview demos.

## Beginner guide (no DAP background needed)

This project has two ways to use the debugger:
1. VS Code mode (primary): debug from VS Code using the `lean-toy-dap` extension.
2. Lean widget mode: debug inside Lean infoview via `#widget`.

### VS Code quick start (recommended)

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

4. In that VS Code window, press `F5` and choose:
- `Run ImpLab Toy DAP Extension (watch)` (recommended), or
- `Run ImpLab Toy DAP Extension (compile once)`.

5. In the Extension Development Host that opens:
- open this repository,
- open `examples/Main.lean`,
- run debug config `ImpLab Toy DAP (auto-export ProgramInfo)` from `.vscode/launch.json`.
- if you need to create/edit one manually, see [VS Code launch process and config](#vs-code-launch-process-and-config).

The extension launches `toydap` automatically (default path: `${workspaceFolder}/.lake/build/bin/toydap`).

By default, it will debug the program named `mainProgram`; see [VS Code launch process and config](#vs-code-launch-process-and-config) for how to customize this.

### Lean widget quick start

In a Lean file:

```lean
import Lean
import ImpLab.Debugger.Widget.UI
import ImpLab.Debugger.Widget.Server
import ImpLab.Debugger.Widget.Types
import ImpLab.Lang.Dsl

open ImpLab

def mainProgram : ProgramInfo := dap%[
  def inc(x) := {
    let one := 1,
    let out := add x one,
    return out
  },
  def main() := {
    let seed := 5,
    let out := call inc(seed)
  }
]

def mainProps : TraceWidgetInitProps :=
  { programInfo := mainProgram, stopOnEntry := true }

#widget ImpLab.traceExplorerWidget with Lean.toJson mainProps
```

The widget launches a live debugger session and shows grouped function code, current function/pc/source location, call stack, and locals while stepping.

### Toy language reference

The language has one term elaborator:
- `dap%[...] : ImpLab.ProgramInfo`

`dap%[...]` accepts only function definitions and must include `main()` as entrypoint.

Statements:

```lean
let v := N
let v := add v1 v2
let v := sub v1 v2
let v := mul v1 v2
let v := div v1 v2
let v := call f(a, b, ...)
return v
```

Example:

```lean
def p : ImpLab.ProgramInfo := dap%[
  def addMul(x, y) := {
    let s := add x y,
    let z := mul s y,
    return z
  },
  def main() := {
    let a := 2,
    let b := 5,
    let out := call addMul(a, b)
  }
]
```

`ProgramInfo.located` stores source locations with function context (`func`, `stmtLine`, `span`), which powers function-aware breakpoints and stack traces.

## Internals and advanced configuration

### VS Code launch process and config

A launch config is a VS Code debug profile (JSON in `launch.json`) that tells VS Code:
- which debugger type to run (`lean-toy-dap`),
- how to launch it (`request: launch`),
- which inputs to pass (`programInfo`, `source`, `stopOnEntry`, etc.).

Launch flow in this repository:
1. You run `ImpLab Toy DAP (auto-export ProgramInfo)` from `.vscode/launch.json`.
2. Its `preLaunchTask` runs `dap-export` and writes `.dap/programInfo.generated.json`.
3. The extension launches `toydap`.
4. If `programInfo` is not inline in launch JSON, the extension auto-loads `.dap/programInfo.generated.json`.

Minimal config (customize as needed):

```json
{
  "name": "ImpLab Toy DAP",
  "type": "lean-toy-dap",
  "request": "launch",
  "source": "${file}",
  "stopOnEntry": true
}
```

Notes:
- `programInfo` is required to launch.
- With the provided auto-export launch setup, you usually do not need to set `programInfo` manually.
- Launch fails if neither inline `programInfo` nor valid `.dap/programInfo.generated.json` is available.
- `source` is optional and controls displayed source path in stack frames.
- `toydapPath` (optional, `string`): explicit adapter binary path.
- If `toydapPath` is omitted, the extension tries `${workspaceFolder}/.lake/build/bin/toydap`, then `toydap` from `PATH`.
- `toydapArgs` (optional, `string[]`): extra arguments passed to the `toydap` process.

Manual export (advanced/internal): generate source-aware JSON from a Lean declaration:

```bash
lake exe dap-export --decl ImpLab.Lang.Examples.mainProgram --out .dap/programInfo.generated.json
```

Using a different declaration than `mainProgram`:

```bash
lake exe dap-export --decl MyProject.Debugger.lesson1Program --out .dap/programInfo.generated.json
```

Then launch normally from VS Code; the extension will pick up the newly generated `.dap/programInfo.generated.json`.

`--decl` must resolve to an `ImpLab.ProgramInfo` declaration.

`toydap` CLI arguments (`lake exe toydap --help`):
- No CLI flags are currently supported.
- `toydap` runs as a stdio DAP server and expects DAP messages on stdin.

### VS Code side-load client

The `client/` extension launches `toydap`.

See `client/README.md` for packaging/sideload options and full launch details.

### Execution model

The interpreter uses explicit call frames:
- each frame has function name, local environment, and program counter,
- `call` pushes a frame,
- `return` pops and assigns into caller destination,
- stepping (`step`) is the semantic foundation for runtime and debugger behavior.

### Lean RPC widget methods

Registered in `ImpLab.Debugger.Widget.Server`:
- `ImpLab.Debugger.Widget.Server.widgetLaunch`
- `ImpLab.Debugger.Widget.Server.widgetStepIn`
- `ImpLab.Debugger.Widget.Server.widgetStepBack`
- `ImpLab.Debugger.Widget.Server.widgetContinue`
- `ImpLab.Debugger.Widget.Server.widgetDisconnect`

`widgetLaunch` accepts `programInfo` plus optional `stopOnEntry` and `breakpoints`.

### Project layout

- `app/` executables:
  - `app/ToyDap.lean`: stdio DAP adapter entrypoint (`lake exe toydap`).
  - `app/ExportMain.lean`: `ProgramInfo` export CLI (`lake exe dap-export`).
  - These files are intentional thin entrypoints; logic lives under `ImpLab/*`.
- `ImpLab/Lang/Ast.lean`: core AST (`Program` is a list of functions, entrypoint is `main`).
- `ImpLab/Lang/Dsl.lean`: DSL syntax/macros (`dap%[...]`) + infotree metadata.
- `ImpLab/Lang/Eval.lean`: environment, call-stack semantics, small-step transition, and full runner.
- `ImpLab/Lang/History.lean`: shared cursor/history navigation helpers.
- `ImpLab/Lang/Trace.lean`: execution trace and navigation API (`Explorer`).
- `examples/Main.lean`: sample program and widget launch props.
- `ImpLab/Debugger/Session.lean`: pure debugger session model (breakpoints, continue, next, stepIn, stepOut, stepBack).
- `ImpLab/Debugger/Core.lean`: session store + DAP-shaped pure core operations.
- `ImpLab/Debugger/Widget/Server.lean`: Lean server RPC endpoints implementing DAP-like operations.
- `ImpLab/Debugger/DAP/Stdio.lean`: standalone DAP adapter implementation (native DAP protocol over stdio).
- `ImpLab/Debugger/Widget/Types.lean`: widget launch/session view models and session-to-widget projection helpers.
- `ImpLab/Debugger/Widget/UI.lean`: `traceExplorerWidget` module UI.
- `ImpLab/Debugger/DAP/Export.lean`: `dap-export` declaration loader/export logic.
- `Test/Core.lean`: core/runtime/debugger tests.
- `Test/Transport.lean`: DAP stdio transport lifecycle/framing tests.
- `Test/Main.lean`: test runner executable.
- `client/`: VS Code extension scaffold for side-loading (`lean-toy-dap` debug type).

### Dev commands

```bash
lake build
lake exe toydap
lake exe dap-export --help
lake exe dap-tests
```
