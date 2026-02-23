/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.DAP.Core

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
  program : Program
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

private def parseDeclName? (raw : String) : Option Name :=
  let parts := raw.trimAscii.toString.splitOn "." |>.filter (· != "")
  match parts with
  | [] => none
  | _ =>
    some <| parts.foldl Name.str Name.anonymous

private def isUnqualifiedName (n : Name) : Bool :=
  match n with
  | .str .anonymous _ => true
  | .num .anonymous _ => true
  | _ => false

private def candidateEntryNames (moduleName entryName : Name) : Array Name :=
  if isUnqualifiedName entryName then
    #[entryName, moduleName ++ entryName]
  else
    #[entryName]

private def stringLit? (e : Expr) : Option String :=
  match e with
  | .lit (.strVal s) => some s
  | _ => none

private def natLit? (e : Expr) : Option Nat :=
  match e with
  | .lit (.natVal n) => some n
  | _ => none

private def decodeBinOpExpr? (e : Expr) : Lean.MetaM (Option BinOp) := do
  let e ← Lean.Meta.whnf e
  match e.getAppFn with
  | .const ``Dap.BinOp.add _ => pure (some .add)
  | .const ``Dap.BinOp.sub _ => pure (some .sub)
  | .const ``Dap.BinOp.mul _ => pure (some .mul)
  | .const ``Dap.BinOp.div _ => pure (some .div)
  | _ => pure none

private def decodeIntExpr? (e : Expr) : Lean.MetaM (Option Int) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Int.ofNat _ =>
    if h : args.size = 1 then
      pure <| (natLit? args[0]).map Int.ofNat
    else
      pure none
  | .const ``Int.negSucc _ =>
    if h : args.size = 1 then
      pure <| (natLit? args[0]).map Int.negSucc
    else
      pure none
  | _ => pure none

private def decodeRhsExpr? (e : Expr) : Lean.MetaM (Option Rhs) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Dap.Rhs.const _ =>
    if h : args.size = 1 then
      return (← decodeIntExpr? args[0]).map Rhs.const
    else
      pure none
  | .const ``Dap.Rhs.bin _ =>
    if h : args.size = 3 then
      let op? ← decodeBinOpExpr? args[0]
      let lhs? := stringLit? args[1]
      let rhs? := stringLit? args[2]
      pure <| do
        let op ← op?
        let lhs ← lhs?
        let rhs ← rhs?
        pure (Rhs.bin op lhs rhs)
    else
      pure none
  | _ => pure none

private def decodeStmtExpr? (e : Expr) : Lean.MetaM (Option Stmt) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Dap.Stmt.mk _ =>
    if h : args.size = 2 then
      let dest? := stringLit? args[0]
      let rhs? ← decodeRhsExpr? args[1]
      pure <| do
        let dest ← dest?
        let rhs ← rhs?
        pure { dest, rhs }
    else
      pure none
  | _ => pure none

private def decodeStmtSpanExpr? (e : Expr) : Lean.MetaM (Option StmtSpan) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Dap.StmtSpan.mk _ =>
    if h : args.size = 4 then
      let startLine? := natLit? args[0]
      let startColumn? := natLit? args[1]
      let endLine? := natLit? args[2]
      let endColumn? := natLit? args[3]
      pure <| do
        let startLine ← startLine?
        let startColumn ← startColumn?
        let endLine ← endLine?
        let endColumn ← endColumn?
        pure { startLine, startColumn, endLine, endColumn }
    else
      pure none
  | _ => pure none

private def decodeLocatedStmtExpr? (e : Expr) : Lean.MetaM (Option LocatedStmt) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Dap.LocatedStmt.mk _ =>
    if h : args.size = 2 then
      let stmt? ← decodeStmtExpr? args[0]
      let span? ← decodeStmtSpanExpr? args[1]
      pure <| do
        let stmt ← stmt?
        let span ← span?
        pure { stmt, span }
    else
      pure none
  | _ => pure none

private def decodeStmtListExprAux? (fuel : Nat) (e : Expr) : Lean.MetaM (Option (List Stmt)) := do
  if fuel = 0 then
    pure none
  else
    let e ← Lean.Meta.whnf e
    let args := e.getAppArgs
    match e.getAppFn with
    | .const ``List.nil _ =>
      pure (some [])
    | .const ``List.cons _ =>
      if h : args.size = 3 then
        let head? ← decodeStmtExpr? args[1]
        let tail? ← decodeStmtListExprAux? (fuel - 1) args[2]
        pure <| do
          let head ← head?
          let tail ← tail?
          pure (head :: tail)
      else
        pure none
    | _ =>
      pure none

private def decodeStmtListExpr? (e : Expr) : Lean.MetaM (Option (List Stmt)) :=
  decodeStmtListExprAux? 100000 e

private def decodeLocatedStmtListExprAux? (fuel : Nat) (e : Expr) :
    Lean.MetaM (Option (List LocatedStmt)) := do
  if fuel = 0 then
    pure none
  else
    let e ← Lean.Meta.whnf e
    let args := e.getAppArgs
    match e.getAppFn with
    | .const ``List.nil _ =>
      pure (some [])
    | .const ``List.cons _ =>
      if h : args.size = 3 then
        let head? ← decodeLocatedStmtExpr? args[1]
        let tail? ← decodeLocatedStmtListExprAux? (fuel - 1) args[2]
        pure <| do
          let head ← head?
          let tail ← tail?
          pure (head :: tail)
      else
        pure none
    | _ =>
      pure none

private def decodeLocatedStmtListExpr? (e : Expr) : Lean.MetaM (Option (List LocatedStmt)) :=
  decodeLocatedStmtListExprAux? 100000 e

private def decodeProgramExpr? (e : Expr) : Lean.MetaM (Option Program) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Array.mk _ =>
    if h : args.size = 2 then
      return (← decodeStmtListExpr? args[1]).map List.toArray
    else
      pure none
  | .const ``List.toArray _ =>
    if h : args.size = 2 then
      return (← decodeStmtListExpr? args[1]).map List.toArray
    else
      pure none
  | _ =>
    pure none

private def decodeLocatedStmtArrayExpr? (e : Expr) : Lean.MetaM (Option (Array LocatedStmt)) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Array.mk _ =>
    if h : args.size = 2 then
      return (← decodeLocatedStmtListExpr? args[1]).map List.toArray
    else
      pure none
  | .const ``List.toArray _ =>
    if h : args.size = 2 then
      return (← decodeLocatedStmtListExpr? args[1]).map List.toArray
    else
      pure none
  | _ =>
    pure none

private def decodeProgramInfoExpr? (e : Expr) : Lean.MetaM (Option ProgramInfo) := do
  let e ← Lean.Meta.whnf e
  let args := e.getAppArgs
  match e.getAppFn with
  | .const ``Dap.ProgramInfo.mk _ =>
    if h : args.size = 2 then
      let program? ← decodeProgramExpr? args[0]
      let located? ← decodeLocatedStmtArrayExpr? args[1]
      pure <| do
        let program ← program?
        let located ← located?
        pure { program, located }
    else
      pure none
  | _ =>
    pure none

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
  RequestM.pureTask <| launchFromProgram params.program params.stopOnEntry params.breakpoints

@[server_rpc_method]
def dapLaunchMain (params : LaunchMainParams) : RequestM (RequestTask LaunchResponse) := do
  let declName ←
    match parseDeclName? params.entryPoint with
    | some declName => pure declName
    | none => throw <| mkInvalidParams s!"Invalid entry point name: '{params.entryPoint}'"
  let lspPos : Lsp.Position := { line := params.line, character := params.character }
  let doc ← RequestM.readDoc
  let candidates := candidateEntryNames doc.meta.mod declName
  RequestM.withWaitFindSnapAtPos lspPos fun snap => do
    let resolvedName? ←
      RequestM.runCoreM snap do
        let env ← getEnv
        pure <| candidates.find? env.contains
    let resolvedName ←
      match resolvedName? with
      | some name => pure name
      | none =>
        let attempted := String.intercalate ", " <| candidates.toList.map (fun n => s!"'{n}'")
        throw <| mkInvalidParams
          s!"Could not resolve entry point '{params.entryPoint}'. Tried: {attempted}"
    let programInfo? ←
      RequestM.runTermElabM snap do
        decodeProgramInfoExpr? (mkConst resolvedName)
    match programInfo? with
    | some rawInfo =>
      let info ←
        match rawInfo.validate with
        | .ok info => pure info
        | .error err => throw <| mkInvalidParams err
      launchFromProgram info.program params.stopOnEntry params.breakpoints info.stmtSpans
    | none =>
      let program ←
        match ← RequestM.runTermElabM snap do
          decodeProgramExpr? (mkConst resolvedName)
        with
        | some program => pure program
        | none =>
          throw <| mkInvalidParams
            s!"Could not decode '{resolvedName}' as Dap.ProgramInfo or Dap.Program at {lspPos}"
      launchFromProgram program params.stopOnEntry params.breakpoints

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
