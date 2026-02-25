# imp-lab

ImpLab is a Lean playground for programming language modeling and teaching-oriented tooling.

Today the core feature is the debugger, available in two modes:

- as a [Debug Adapter Protocol (DAP)](https://microsoft.github.io/debug-adapter-protocol/) server (`toydap`) for editor integration, and,
- as an in-editor Lean UI built with [ProofWidgets](https://github.com/leanprover-community/ProofWidgets4).

ImpLab was built using [Lean](https://lean-lang.org/) and [OpenAI's Codex](https://openai.com/codex/)

**Note**: ImpLab is not production ready! It is designed to demonstrate and explore Lean’s capabilities. The code is a prototype and has not undergone the rigorous review and testing required for production use.

## Run the Imp debugger

### 1) Build the project

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
- open the folder at the root of this repository,
- open `examples/Main.lean`,
- run debug config `ImpLab Toy DAP (auto-export ProgramInfo)`.

Notes:
- The launch config should auto-generate the debugger meta-data `.dap/programInfo.generated.json` using `dap-export` automatically.
- The adapter binary is `toydap`.

### 3) Run in Lean (ProofWidgets mode, less involved, less features)

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
- top-level mutable globals (`global g := N`) stored in a heap,
- arithmetic operations (`add`, `sub`, `mul`, `div`),
- heap operations (`get g`, `set g := v`),
- function calls with parameters,
- source-aware program metadata used by the debugger.

Programs are written with:

- `imp%[...] : ImpLab.ProgramInfo`

Example:

```lean
def sample : ImpLab.ProgramInfo := imp%[
  global counter := 0,
  def inc(x) := {
    let one := 1,
    let out := add x one,
    return out
  },
  def main() := {
    let seed := 5,
    let out := call inc(seed),
    set counter := out,
    let latest := get counter
  }
]
```

`get` and `set` on undeclared globals fail with a runtime error.

Language reference:
- `docs/language.md`

## Additional links

- Debugger architecture and launch contract: `docs/debugger.md`
- Debugger roadmap (active priorities): `docs/debugger-roadmap.md`
- VS Code extension details: `client/README.md`
- Agent instructions (global): `AGENTS.md`
- Agent instructions (debugger-local): `ImpLab/Debugger/AGENTS.md`
