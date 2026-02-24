# Debugger Roadmap

This file tracks active debugger work.

Stable architecture guardrails and review policy live in:
- `ImpLab/Debugger/AGENTS.md`

## Active priorities

1. Build a deterministic DAP showcase scenario pack for demos.
Context: feature-complete demos need scripted examples that exercise each capability reliably.
2. Keep fixture auto-launch/export wiring stable for `examples/Main.lean`.
Context: demo/debug sessions should always use a coherent `source` + generated `ProgramInfo` pair.
3. Add an optional launch/task declaration variable (default `ImpLab.Lang.Examples.mainProgram`).
Context: preserve the stable default fixture while enabling explicit alternate entrypoints without config drift.
4. Expand expression support in `evaluate` while preserving deterministic errors.
Context: current parser intentionally supports a compact expression subset.

## New features

- `evaluate`:
  - Support `context` values (`repl`, `hover`, `watch`) with stable response payloads.
  - Evaluate against selected frame state and return readable result strings.
- `setVariable`:
  - Update either locals or heap depending on scope reference.
  - Return scalar value metadata (`variablesReference = 0`) to avoid duplicate expandable trees in clients.
- Breakpoint richness:
  - Conditional breakpoints.
  - Hit-count breakpoints.
  - Logpoints.
- Exceptions as debug stops:
  - `setExceptionBreakpoints`.
  - `exceptionInfo`.
  - `stopped` with `reason = "exception"` when runtime errors occur.
- Function breakpoints:
  - `setFunctionBreakpoints` resolved through `ProgramInfo` function names.
- Source introspection:
  - Status: deferred until we have stronger virtual/generated source use-cases.
  - `loadedSources` request.
  - `source` request for virtual/generated source text.
- Stepping extensions:
  - `stepInTargets`.
  - Optional `restartFrame`.
- Time-travel jumps:
  - `gotoTargets`.
  - `goto` backed by the trace/history cursor model.
- Data breakpoints:
  - Stop when selected locals change.

## Robustness

- Transport parity audit:
  - Inventory duplicated validation/dispatch helpers in `ImpLab/Debugger/DAP/Stdio.lean` and `ImpLab/Debugger/Widget/Server.lean`.
  - Lift shared semantics into `ImpLab/Debugger/Core.lean`.
- Lifecycle ordering matrix (`Test/Transport.lean`):
  - Add invalid-ordering cases (commands before `launch`, repeated `disconnect`, control after termination).
  - Add repeated terminal-event guards (no duplicate terminal transitions).
- Source mapping matrix (`Test/Core.lean` + `Test/Transport.lean`):
  - Cover breakpoints and stack traces across multiple functions/call depths.
  - Confirm mapping stability across `stepIn`, `next`, `stepOut`, and `stepBack` transitions.
- DAP payload and event invariants:
  - Keep response/error shapes stable.
  - Keep lifecycle event order coherent (`initialized`, `stopped`, `continued`, `terminated`).

## Demo readiness

- DAP showcase scenario pack:
  - Add small programs that deterministically trigger each implemented DAP feature.
  - Keep `examples/Main.lean` as default fixture while adding focused demo fixtures.
- Demo runbook:
  - Document a canonical walk-through sequence for VS Code.
  - Include expected visible outcomes for each step.
- Adapter demo narration:
  - Emit useful `output` events during key transitions.
- Docs synchronization:
  - Keep `docs/debugger.md`, `client/README.md`, and this roadmap aligned with actual support.
