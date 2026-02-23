# AGENTS

## Project scope
Educational Lean 4 toy language + debugger.
Primary surfaces:
- Lean runtime + debug model (`Dap/*.lean`)
- Shared pure debugger API/core (`Dap/DebugCore.lean`)
- Lean RPC debug service (`Dap/Server.lean`)
- Standalone DAP adapter (`Dap/ToyDap.lean`, `ToyDap.lean`)
- VS Code client extension (`client/`)
- Widget demo (`Dap/Widget.lean`)

## Build and test
- `lake build`
- `lake exe dap-tests`
- `lake exe toydap`
- `lake exe dap-export --help`
- `cd client && npm run compile`

Run these after meaningful changes touching Lean or VS Code codepaths.

## Architecture guardrails
- Keep execution semantics in `Dap/Eval.lean` and `Dap/DebugModel.lean`.
- Keep debugger/session semantics in `Dap/DebugCore.lean` (single source of truth).
- Treat transports as adapters only:
  - Lean RPC adapter logic in `Dap/Server.lean`
  - StdIO DAP adapter logic in `Dap/ToyDap.lean`
- Implement new debugger behavior in `DebugCore` first, then wire transports.
- Avoid duplicating protocol/state logic across adapters.
- Keep source mapping consistent (statement index <-> source lines) and clearly document line-base assumptions.

## Coding conventions
- Prefer small, total helpers over large request handlers.
- Preserve stable JSON shapes for DAP-facing payloads.
- Keep sample/demo declarations in `Dap/Examples.lean` as canonical fixtures.
- When adding new launch modes, update both docs and tests.
- Keep `mainProgram` as the default user entrypoint.
- Prefer `programInfo` / `programInfoFile` launch flow for robust source mapping.

## Testing split
- Core functional tests should target `Dap/DebugCore.lean` APIs directly.
- Transport tests should focus on framing/serialization and request-to-core wiring.
- For DAP protocol sanity tests, cover lifecycle ordering and at least one breakpoint hit path.

## Review checklist
- Any behavior duplicated between `Dap/Server.lean` and `Dap/ToyDap.lean` that belongs in `Dap/DebugCore.lean`?
- Any hardcoded declaration/entrypoint list that should be generalized?
- Any duplicate decode/source-mapping logic that can drift from syntax/data definitions?
- Are breakpoints/stack lines correctly mapped for both plain `Program` and `ProgramInfo`?
- Are lifecycle events (`initialized`, `stopped`, `continued`, `terminated`) emitted in valid order?
