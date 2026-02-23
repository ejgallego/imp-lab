# AGENTS

## Project scope
Educational Lean 4 toy language + debugger.
Primary surfaces:
- Lean runtime + debug model (`Dap/*.lean`)
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
- Treat transports as adapters only:
  - Lean RPC adapter logic in `Dap/Server.lean`
  - StdIO DAP adapter logic in `Dap/ToyDap.lean`
- Avoid duplicating protocol/state logic across adapters; prefer shared helpers in Lean when possible.
- Keep source mapping consistent (statement index <-> source lines) and clearly document line-base assumptions.

## Coding conventions
- Prefer small, total helpers over large request handlers.
- Preserve stable JSON shapes for DAP-facing payloads.
- Keep sample/demo declarations in `Dap/Examples.lean` as canonical fixtures.
- When adding new launch modes, update both docs and tests.

## Review checklist
- Any dead path between `client/src/extension.ts` and adapter implementation?
- Any hardcoded declaration/entrypoint list that should be generalized?
- Any duplicate decode logic that can drift from syntax/data definitions?
- Are breakpoints/stack lines correctly mapped for both plain `Program` and `ProgramInfo`?
- Are lifecycle events (`initialized`, `stopped`, `continued`, `terminated`) emitted in valid order?
