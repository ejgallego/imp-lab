# AGENTS

## Project scope
Educational Lean 4 toy language + debugger.
Primary goal: clarity for students reading the code, not runtime performance.
Assume small programs; optimize for simple, explicit, maintainable implementations.
Do not add compatibility layers/shims during reorganizations; prefer a direct clean structure.
Primary surfaces:
- Lean language runtime (`Dap/Lang/*.lean`)
- Shared pure debugger API/core (`Dap/DAP/Core.lean`)
- Lean RPC debug service (`Dap/DAP/Server.lean`)
- Standalone DAP adapter (`Dap/DAP/Stdio.lean`, `ToyDap.lean`)
- VS Code client extension (`client/`)
- Widget demo (`Dap/Widget/Types.lean`, `Dap/Widget/Server.lean`)

## Build and test
- `lake build`
- `lake exe dap-tests`
- `lake exe toydap`
- `lake exe dap-export --help`
- `cd client && npm run compile`

Run these after meaningful changes touching Lean or VS Code codepaths.

## Architecture guardrails
- Keep execution semantics in `Dap/Lang/Eval.lean` and `Dap/DAP/Session.lean`.
- Keep debugger/session semantics in `Dap/DAP/Core.lean` (single source of truth).
- Treat `ProgramInfo` as the canonical program representation across debugger flows.
- Keep `Program` support as compatibility/input convenience (`Program -> ProgramInfo` with empty spans).
- Treat transports as adapters only:
  - Lean RPC adapter logic in `Dap/DAP/Server.lean`
  - StdIO DAP adapter logic in `Dap/DAP/Stdio.lean`
- Implement new debugger behavior in `Dap/DAP/Core.lean` first, then wire transports.
- Avoid duplicating protocol/state logic across adapters.
- Keep source mapping consistent (statement index <-> source lines) and clearly document line-base assumptions.

## Coding conventions
- Prefer simplicity and readability over performance-oriented complexity.
- Prefer small, total helpers over large request handlers.
- Preserve stable JSON shapes for DAP-facing payloads.
- Keep sample/demo declarations in `Dap/Lang/Examples.lean` as canonical fixtures.
- When adding new launch modes, update both docs and tests.
- Keep `mainProgram` as the default user entrypoint.
- Prefer `ProgramInfo`-first APIs and flows; avoid adding new `Program`-only paths.
- Use `dap%[...]` as the single DSL elaborator; it should produce `ProgramInfo`.

## Testing split
- Core functional tests should target `Dap/DAP/Core.lean` APIs directly.
- Transport tests should focus on framing/serialization and request-to-core wiring.
- For DAP protocol sanity tests, cover lifecycle ordering and at least one breakpoint hit path.

## Review checklist
- Any behavior duplicated between `Dap/DAP/Server.lean` and `Dap/DAP/Stdio.lean` that belongs in `Dap/DAP/Core.lean`?
- Any hardcoded declaration/entrypoint list that should be generalized?
- Any duplicate decode/source-mapping logic that can drift from syntax/data definitions?
- Are breakpoints/stack lines correctly mapped for both plain `Program` and `ProgramInfo`?
- Are lifecycle events (`initialized`, `stopped`, `continued`, `terminated`) emitted in valid order?
