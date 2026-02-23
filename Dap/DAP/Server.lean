/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Debugger.Core
import Dap.DAP.Launch

open Lean Lean.Server

namespace Dap.Server

builtin_initialize dapSessionStoreRef : IO.Ref SessionStore ←
  IO.mkRef { nextId := 1, sessions := {} }

structure InitializeParams where
  deriving Inhabited, Repr, FromJson, ToJson

structure InitializeResponse where
  supportsConfigurationDoneRequest : Bool := true
  supportsStepBack : Bool := true
  supportsRestartRequest : Bool := false
  deriving Inhabited, Repr, FromJson, ToJson

structure LaunchParams where
  programInfo? : Option ProgramInfo := none
  program? : Option Program := none
  stopOnEntry : Bool := true
  breakpoints : Array Nat := #[]
  deriving Inhabited, Repr, FromJson, ToJson

structure LaunchMainParams where
  entryPoint : String := "mainProgramInfo"
  line : Nat := 0
  character : Nat := 0
  stopOnEntry : Bool := true
  breakpoints : Array Nat := #[]
  deriving Inhabited, Repr, FromJson, ToJson

abbrev LaunchResponse := Dap.LaunchResponse
abbrev BreakpointView := Dap.BreakpointView

structure SetBreakpointsParams where
  sessionId : Nat
  breakpoints : Array Nat := #[]
  deriving Inhabited, Repr, FromJson, ToJson

abbrev SetBreakpointsResponse := Dap.SetBreakpointsResponse
abbrev ThreadView := Dap.ThreadView
abbrev ThreadsResponse := Dap.ThreadsResponse

structure SessionParams where
  sessionId : Nat
  deriving Inhabited, Repr, FromJson, ToJson

abbrev ControlResponse := Dap.ControlResponse
abbrev StackFrameView := Dap.StackFrameView

structure StackTraceParams where
  sessionId : Nat
  startFrame : Nat := 0
  levels : Nat := 20
  deriving Inhabited, Repr, FromJson, ToJson

abbrev StackTraceResponse := Dap.StackTraceResponse

structure ScopesParams where
  sessionId : Nat
  frameId : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

abbrev ScopeView := Dap.ScopeView
abbrev ScopesResponse := Dap.ScopesResponse

structure VariablesParams where
  sessionId : Nat
  variablesReference : Nat
  deriving Inhabited, Repr, FromJson, ToJson

abbrev VariableView := Dap.VariableView
abbrev VariablesResponse := Dap.VariablesResponse

structure DisconnectResponse where
  disconnected : Bool
  deriving Inhabited, Repr, FromJson, ToJson

private def mkInvalidParams (message : String) : RequestError :=
  RequestError.invalidParams message

private def runCoreResult (result : Except String α) : RequestM α :=
  match result with
  | .ok value =>
    pure value
  | .error err =>
    throw <| mkInvalidParams err

private def updateStore (store : SessionStore) : IO Unit :=
  dapSessionStoreRef.set store

private def launchFromProgram
    (program : Program)
    (stopOnEntry : Bool)
    (breakpoints : Array Nat)
    (stmtSpans : Array StmtSpan := #[]) : RequestM LaunchResponse := do
  let store ← dapSessionStoreRef.get
  let (store, response) ← runCoreResult <|
    Dap.launchFromProgram store program stopOnEntry breakpoints stmtSpans
  updateStore store
  pure response

@[server_rpc_method]
def dapInitialize (_params : InitializeParams) : RequestM (RequestTask InitializeResponse) :=
  RequestM.pureTask do
    pure {}

@[server_rpc_method]
def dapLaunch (params : LaunchParams) : RequestM (RequestTask LaunchResponse) :=
  RequestM.pureTask do
    let launchProgram : Except String (Program × Array StmtSpan) := do
      match params.programInfo?, params.program? with
      | some info, _ =>
        let info ← info.validate
        pure (info.program, info.stmtSpans)
      | none, some program =>
        pure (program, #[])
      | none, none =>
        throw "Invalid launch payload: expected `programInfo` (preferred) or `program`."
    let (program, stmtSpans) ← runCoreResult launchProgram
    launchFromProgram program params.stopOnEntry params.breakpoints stmtSpans

@[server_rpc_method]
def dapLaunchMain (params : LaunchMainParams) : RequestM (RequestTask LaunchResponse) := do
  let lspPos : Lsp.Position := { line := params.line, character := params.character }
  let doc ← RequestM.readDoc
  RequestM.withWaitFindSnapAtPos lspPos fun snap => do
    let launchProgramResult ←
      RequestM.runCoreM snap do
        let env ← getEnv
        pure <|
          Dap.resolveLaunchProgramFromEnv
            env
            params.entryPoint
            (moduleName? := some doc.meta.mod)
    let launchProgram ← runCoreResult launchProgramResult
    launchFromProgram
      launchProgram.program
      params.stopOnEntry
      params.breakpoints
      launchProgram.stmtSpans

@[server_rpc_method]
def dapSetBreakpoints (params : SetBreakpointsParams) :
    RequestM (RequestTask SetBreakpointsResponse) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, response) ← runCoreResult <| Dap.setBreakpoints store params.sessionId params.breakpoints
    updateStore store
    pure response

@[server_rpc_method]
def dapThreads (_params : SessionParams) : RequestM (RequestTask ThreadsResponse) :=
  RequestM.pureTask do
    pure <| Dap.threads (← dapSessionStoreRef.get)

@[server_rpc_method]
def dapNext (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, response) ← runCoreResult <| Dap.next store params.sessionId
    updateStore store
    pure response

@[server_rpc_method]
def dapStepBack (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, response) ← runCoreResult <| Dap.stepBack store params.sessionId
    updateStore store
    pure response

@[server_rpc_method]
def dapContinue (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, response) ← runCoreResult <| Dap.continueExecution store params.sessionId
    updateStore store
    pure response

@[server_rpc_method]
def dapPause (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    runCoreResult <| Dap.pause (← dapSessionStoreRef.get) params.sessionId

@[server_rpc_method]
def dapStackTrace (params : StackTraceParams) : RequestM (RequestTask StackTraceResponse) :=
  RequestM.pureTask do
    runCoreResult <| Dap.stackTrace (← dapSessionStoreRef.get) params.sessionId params.startFrame params.levels

@[server_rpc_method]
def dapScopes (params : ScopesParams) : RequestM (RequestTask ScopesResponse) :=
  RequestM.pureTask do
    runCoreResult <| Dap.scopes (← dapSessionStoreRef.get) params.sessionId params.frameId

@[server_rpc_method]
def dapVariables (params : VariablesParams) : RequestM (RequestTask VariablesResponse) :=
  RequestM.pureTask do
    runCoreResult <| Dap.variables (← dapSessionStoreRef.get) params.sessionId params.variablesReference

@[server_rpc_method]
def dapDisconnect (params : SessionParams) : RequestM (RequestTask DisconnectResponse) :=
  RequestM.pureTask do
    let (store, disconnected) := Dap.disconnect (← dapSessionStoreRef.get) params.sessionId
    updateStore store
    pure { disconnected }

end Dap.Server
