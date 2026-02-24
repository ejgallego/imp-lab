# imp-lab

ImpLab is a Lean playground for programming language modeling and teaching-oriented tooling.

Today the core feature is the debugger, available in two modes: as a [Debug Adapter Protocol (DAP)](https://microsoft.github.io/debug-adapter-protocol/) server (`toydap`) for editor integration, and as an in-editor Lean UI built with [ProofWidgets](https://github.com/leanprover-community/ProofWidgets4).

## Run the debugger

### 1) Build everything once

```bash
lake build
cd client && npm install && npm run compile
```

### 2) Run in VS Code (DAP mode)

1. Open the extension project:

```bash
code client
```

2. In that VS Code window, press `F5` and run one of:
- `Run ImpLab Toy DAP Extension (watch)`
- `Run ImpLab Toy DAP Extension (compile once)`

3. In the Extension Development Host window:
- open this repository,
- open `examples/Main.lean`,
- run debug config `ImpLab Toy DAP (auto-export ProgramInfo)`.

Notes:
- The launch config auto-generates `.dap/programInfo.generated.json` using `dap-export`.
- The adapter binary is `toydap`.

### 3) Run in Lean (ProofWidgets mode)

1. Open `examples/Main.lean`.
2. Ensure the Lean infoview is active.
3. Evaluate the widget declaration at the end of the file:

```lean
#widget ImpLab.traceExplorerWidget with ImpLab.Lang.Examples.sampleTracePropsJson
```

This launches a debugger session directly in infoview.

## Language

ImpLab includes a small imperative language with:

- integer literals and local variables,
- arithmetic operations (`add`, `sub`, `mul`, `div`),
- function calls with parameters,
- source-aware program metadata used by the debugger.

Programs are written with:

- `imp%[...] : ImpLab.ProgramInfo`

Example:

```lean
def sample : ImpLab.ProgramInfo := imp%[
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
```

Language reference:
- `docs/language.md`

## Additional links

- Debugger architecture and launch contract: `docs/debugger.md`
- VS Code extension details: `client/README.md`
- Agent instructions (global): `AGENTS.md`
- Agent instructions (debugger-local): `ImpLab/Debugger/AGENTS.md`
