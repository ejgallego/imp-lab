# Debugger Roadmap

This file tracks active debugger work and near-term milestones.

Stable architecture guardrails and review policy live in:
- `ImpLab/Debugger/AGENTS.md`

## Active priorities

1. Remove remaining duplicated behavior between `ImpLab/Debugger/Widget/Server.lean` and `ImpLab/Debugger/DAP/Stdio.lean` by lifting semantics to `ImpLab/Debugger/Core.lean`.
2. Expand transport tests for lifecycle edge cases (invalid ordering, repeated terminate/disconnect).
3. Strengthen multi-function source mapping checks for stack and breakpoint rendering.

## Open work queue

- Audit both transports for duplicate request validation and decode helpers.
- Add/adjust transport tests for lifecycle edge cases (invalid ordering, repeated terminate/disconnect).
- Verify breakpoint and stack location mapping in multi-function traces.
- Keep `client/README.md`, `docs/debugger.md`, and `README.md` aligned with current launch contract.
- Keep executable roots (`app/ToyDap.lean`, `app/ExportMain.lean`) as thin wrappers.

## Recently completed

- Namespace and layout rebrand to `ImpLab` with debugger split under `ImpLab/Debugger/{DAP,Widget}`.
- Documentation split by scope:
  - `README.md` for user-facing onboarding.
  - `docs/debugger.md` for debugger operations.
  - `docs/language.md` for language reference.
  - `ImpLab/Debugger/AGENTS.md` for stable debugger guardrails.
- DSL elaborator renamed to `imp%[...]`.
