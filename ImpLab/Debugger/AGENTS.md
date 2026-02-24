# Debugger AGENTS

## Scope
Applies to work under `ImpLab/Debugger/*`, `app/ToyDap.lean`, `app/ExportMain.lean`, and debugger-focused tests.

## Source of truth
- Debugger behavior: `ImpLab/Debugger/Core.lean`
- Session semantics: `ImpLab/Debugger/Session.lean`
- DAP transport: `ImpLab/Debugger/DAP/Stdio.lean`
- Widget transport: `ImpLab/Debugger/Widget/Server.lean`

## Rules
- Put new debugger behavior in `Core.lean` first, then wire transports.
- Keep transport files as adapters only; avoid protocol/state duplication.
- Treat `ProgramInfo` as canonical across launch/debug/export flows.
- Keep source mapping coherent (function + statement line <-> source line).
- Preserve stable DAP JSON payload shapes.

## Testing split
- Core behavior tests: `Test/Core.lean`.
- Transport tests: `Test/Transport.lean`.
- DAP sanity must include lifecycle ordering and at least one breakpoint hit path.

## Review checklist
- Is behavior duplicated in `Widget/Server.lean` or `DAP/Stdio.lean` that belongs in core?
- Any hardcoded entrypoint/decl list that should be generalized?
- Any duplicated decode/source-mapping logic that can drift?
- Are stack and breakpoint lines mapped correctly for all functions via `ProgramInfo`?
- Are lifecycle events ordered correctly (`initialized`, `stopped`, `continued`, `terminated`)?
