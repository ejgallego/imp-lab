/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Debugger.Core
import ImpLab.Debugger.Widget.Types

open Lean Lean.Server

namespace ImpLab.Debugger.Widget.Server

initialize dapSessionStoreRef : IO.Ref SessionStore ←
  IO.mkRef { nextId := 1, sessions := {} }

structure DisconnectResponse where
  disconnected : Bool
  deriving Inhabited, Repr, FromJson, ToJson

abbrev WidgetLaunchParams := ImpLab.TraceWidgetInitProps

structure WidgetControlParams where
  sessionId : Nat
  deriving Inhabited, Repr, FromJson, ToJson

abbrev WidgetSessionView := ImpLab.TraceWidgetSessionView

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

private def widgetView (sessionId : Nat) (stopReason : String := "entry") : RequestM WidgetSessionView := do
  let data ← runCoreResult <| ImpLab.inspectSession (← dapSessionStoreRef.get) sessionId
  pure <| ImpLab.TraceWidgetSessionView.ofSessionData sessionId data stopReason

@[server_rpc_method]
def widgetLaunch (params : WidgetLaunchParams) : RequestM (RequestTask WidgetSessionView) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, launch) ← runCoreResult <|
      ImpLab.launchFromProgramInfo store params.programInfo params.stopOnEntry params.breakpoints
    updateStore store
    widgetView launch.sessionId launch.stopReason

@[server_rpc_method]
def widgetStepIn (params : WidgetControlParams) : RequestM (RequestTask WidgetSessionView) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, control) ← runCoreResult <| ImpLab.stepIn store params.sessionId
    updateStore store
    widgetView params.sessionId control.stopReason

@[server_rpc_method]
def widgetStepBack (params : WidgetControlParams) : RequestM (RequestTask WidgetSessionView) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, control) ← runCoreResult <| ImpLab.stepBack store params.sessionId
    updateStore store
    widgetView params.sessionId control.stopReason

@[server_rpc_method]
def widgetContinue (params : WidgetControlParams) : RequestM (RequestTask WidgetSessionView) :=
  RequestM.pureTask do
    let store ← dapSessionStoreRef.get
    let (store, control) ← runCoreResult <| ImpLab.continueExecution store params.sessionId
    updateStore store
    widgetView params.sessionId control.stopReason

@[server_rpc_method]
def widgetDisconnect (params : WidgetControlParams) : RequestM (RequestTask DisconnectResponse) :=
  RequestM.pureTask do
    let (store, disconnected) := ImpLab.disconnect (← dapSessionStoreRef.get) params.sessionId
    updateStore store
    pure { disconnected }

end ImpLab.Debugger.Widget.Server
