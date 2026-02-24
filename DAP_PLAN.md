# DAP Plan

Canonical project guardrails, architecture rules, and validation commands live in `AGENTS.md`.
This file is only for current DAP-facing priorities and open work.

## Active priorities
1. Remove any remaining duplicated behavior between `ImpLab/Debugger/Widget/Server.lean` and `ImpLab/Debugger/DAP/Stdio.lean` by lifting semantics to `ImpLab/Debugger/Core.lean`.
2. Keep line/function source mapping explicit and centralized so stack/breakpoint rendering stays consistent.
3. Preserve strict DAP lifecycle ordering and stable payload shapes for editor compatibility.
4. Keep docs/examples aligned with `ProgramInfo`-only launch/export flows and `app/` entrypoint layout.

## Open work queue
- Audit both transports for duplicate request validation and decode helpers.
- Add/adjust transport tests for lifecycle ordering edge cases (invalid ordering, repeated terminate/disconnect).
- Verify breakpoint and stack location mapping in multi-function traces.
- Keep `client/README.md` and root `README.md` consistent with current launch input contract.
- Keep executable roots (`app/ToyDap.lean`, `app/ExportMain.lean`) as thin wrappers only.

## Milestones
1. Transport parity audit complete (no semantic drift from core).
2. Source-mapping checks expanded for multi-frame scenarios.
3. Lifecycle sanity suite covers error ordering paths.
4. Docs trimmed to a single non-overlapping story (`AGENTS.md` rules, `DAP_PLAN.md` priorities, `README.md` usage).
