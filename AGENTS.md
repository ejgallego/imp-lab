# AGENTS

## Scope and priorities
- Educational Lean 4 toy language + debugger.
- Optimize for clarity and maintainability, not performance.
- Avoid compatibility shims during refactors; prefer direct clean structure.

## Main surfaces
- Runtime: `ImpLab/Lang/*.lean`
- Debugger source of truth: `ImpLab/Debugger/Core.lean`
- Session semantics: `ImpLab/Debugger/Session.lean`
- Lean RPC transport: `ImpLab/Debugger/Widget/Server.lean`
- StdIO DAP transport: `ImpLab/Debugger/DAP/Stdio.lean` + `app/ToyDap.lean`
- VS Code client: `client/`

## Build/test commands
- `lake build`
- `lake exe dap-tests`
- `lake exe toydap`
- `lake exe dap-export --help`
- `cd client && npm run compile`

## Architecture guardrails
- Put new debugger behavior in `ImpLab/Debugger/Core.lean` first, then wire transports.
- Keep transport files as adapters only; avoid protocol/state duplication.
- Treat `ProgramInfo` as canonical across launch/debug/export flows.
- `Program` remains function-only (`functions : Array FuncDef`) with required `main`.
- Keep source mapping coherent (function + statement line <-> source line).

## Language and API conventions
- `dap%[...]` is the only DSL elaborator and must produce `ProgramInfo`.
- `dap%[...]` accepts functions only and must include `main()` (zero params).
- Keep `mainProgram` as default fixture entrypoint in `examples/Main.lean`.
- Prefer `initialize` over `builtin_initialize` in project code.
- Preserve stable DAP JSON payload shapes.

## Testing split
- Core behavior tests: `ImpLab/Debugger/Core.lean` APIs.
- Transport tests: framing/serialization + request-to-core wiring.
- DAP sanity tests: lifecycle ordering + at least one breakpoint hit path.

## Review checklist
- Is behavior duplicated in `Server.lean`/`Stdio.lean` that belongs in core?
- Any hardcoded entrypoint/decl list that should be generalized?
- Any duplicated decode/source-mapping logic that can drift?
- Are stack/breakpoint lines mapped correctly for all functions via `ProgramInfo`?
- Are lifecycle events ordered correctly (`initialized`, `stopped`, `continued`, `terminated`)?
