# imp-lab

ImpLab is a Lean playground to showcase Lean's capabilities for programming language modeling, debugging, and education.

## What is in this repo

- `ImpLab/Lang/*`: toy language AST, DSL, evaluator, traces.
- `ImpLab/Debugger/*`: debugger core, DAP transport, widget transport.
- `examples/Main.lean`: default `mainProgram` fixture.
- `client/`: VS Code extension for the standalone `toydap` adapter.

## Quick start

```bash
lake build
lake exe dap-tests
lake exe toydap
```

For VS Code client:

```bash
cd client
npm install
npm run compile
```

## Minimal Lean example

```lean
import Lean
import ImpLab.Debugger.Widget.UI
import ImpLab.Debugger.Widget.Server
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

def props : TraceWidgetInitProps :=
  { programInfo := mainProgram, stopOnEntry := true }

#widget ImpLab.traceExplorerWidget with Lean.toJson props
```

## Documentation

- Debugger architecture, DAP flow, launch configuration, and transport details:
  - `docs/debugger.md`
- VS Code extension details:
  - `client/README.md`

## Naming notes

- The project is `imp-lab` / `ImpLab`.
- DAP protocol-specific names intentionally remain where appropriate (`toydap`, `dap-export`, `dap-tests`, `lean-toy-dap`, `.dap/`).
