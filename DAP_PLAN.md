# DAP Integration Plan

## Goal

Expose the toy interpreter (`Dap`) as a debug target so external editors can debug programs through the Debug Adapter Protocol (DAP).

## Current status

Implemented pieces:
- Deterministic syntax and evaluator.
- Small-step transition (`step`) with explicit program counter.
- Full execution traces and random access by step index.
- Widget-based time-travel visualization in Lean infoview.

These give us a stable execution core; the DAP adapter can be added on top without changing language semantics.

## Proposed architecture

1. `Runtime` layer (already present):
   - `Syntax`, `Eval`, `Trace`.
   - Pure functions, no transport/protocol concerns.

2. `DebugModel` layer (next):
   - Thread/session model (single-thread for now).
   - Breakpoint representation over statement indices.
   - Debug state machine (`Initialized`, `Running`, `Stopped`, `Terminated`).

3. `DAP Adapter` layer (next):
   - JSON-RPC message loop over stdio.
   - Request handlers: `initialize`, `launch`, `setBreakpoints`, `threads`,
     `stackTrace`, `scopes`, `variables`, `continue`, `next`, `stepBack`, `disconnect`.
   - Event emission: `initialized`, `stopped`, `continued`, `terminated`.

4. `Editor Integration` layer (later):
   - VS Code launch config.
   - Optional bridge between infoview widget state and DAP session state.

## Main technical challenges

1. Source mapping and breakpoint precision
   - DAP breakpoints are file/line based, while runtime currently steps by statement index.
   - Need a stable mapping `line -> stmt index` and the reverse mapping for stack/stop locations.
   - Challenge: preserving mapping under edits and parse failures.

2. Reverse execution semantics
   - DAP `stepBack` is optional but desired.
   - We already store trace snapshots; challenge is managing memory for larger programs.
   - Need configurable trace retention strategy (full snapshots vs checkpoints + replay).

3. Variables/scopes projection
   - DAP expects scope/variable references and lazy expansion.
   - Runtime stores a flat environment map.
   - Need stable variable handles and deterministic ordering for UI diff stability.

4. Protocol state machine correctness
   - DAP clients are strict about request ordering and events.
   - Need robust lifecycle transitions and graceful error reporting for invalid requests.

5. Concurrency and cancellation
   - Even with a single-threaded interpreter, requests can overlap in client behavior.
   - Need request serialization and cancellation strategy (especially around launch/continue).

6. Compatibility with Lean tooling
   - Widget RPC and DAP adapter both touch debug state.
   - Need one shared source of truth to avoid split-brain state.

## Incremental milestones

1. Minimal adapter (launch + next + continue + variables).
2. Breakpoints and stopped events.
3. Reverse stepping (`stepBack`) backed by `ExecutionTrace`.
4. Stack frames/scopes polish and better error UX.
5. Integration tests against the VS Code DAP test harness.

## Testing strategy

- Unit tests for `DebugModel` state transitions.
- Golden tests for JSON request/response bodies.
- Scenario tests:
  - launch -> stop at entry -> step -> inspect vars -> continue -> terminate
  - breakpoints hit order
  - reverse stepping consistency with forward trace
  - invalid requests in each adapter state
