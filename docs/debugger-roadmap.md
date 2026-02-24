# Debugger Roadmap

This file tracks active debugger work.

Stable architecture guardrails and review policy live in:
- `ImpLab/Debugger/AGENTS.md`

## Active priorities

1. Remove remaining duplicated behavior between `ImpLab/Debugger/Widget/Server.lean` and `ImpLab/Debugger/DAP/Stdio.lean` by lifting semantics to `ImpLab/Debugger/Core.lean`.
Context: both adapters still own overlapping request/validation/update patterns.
2. Expand transport tests for lifecycle edge cases (invalid ordering, repeated terminate/disconnect).
Context: current tests cover main happy paths plus selected ordering checks, but not the full invalid-ordering matrix.
3. Strengthen multi-function source mapping checks for stack and breakpoint rendering.
Context: mapping works for core scenarios, but we still need broader multi-function and cross-step coverage.

## Open work queue

- Transport parity audit:
  - Inventory duplicated validation/dispatch helpers in `ImpLab/Debugger/DAP/Stdio.lean` and `ImpLab/Debugger/Widget/Server.lean`.
  - Propose core-level helpers in `ImpLab/Debugger/Core.lean` for shared semantics.
- Lifecycle ordering test expansion (`Test/Transport.lean`):
  - Add invalid-ordering cases (commands before `launch`, repeated `disconnect`, control after termination).
  - Add repeated terminal-event guards (no duplicate terminal transitions).
- Source mapping matrix (`Test/Core.lean` + `Test/Transport.lean`):
  - Cover breakpoints and stack traces across multiple functions/call depths.
  - Confirm mapping stability across `stepIn`, `next`, `stepOut`, and `stepBack` transitions.
