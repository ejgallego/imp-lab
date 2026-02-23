import Lean
import Dap.DebugModel

open Lean Lean.Server

namespace Dap.Server

structure SessionStore where
  nextId : Nat := 1
  sessions : Std.HashMap Nat DebugSession := {}
  deriving Inhabited

builtin_initialize dapSessionStoreRef : IO.Ref SessionStore ←
  IO.mkRef { nextId := 1, sessions := {} }

private def allocateSession (session : DebugSession) : IO Nat := do
  dapSessionStoreRef.modifyGet fun store =>
    let sessionId := store.nextId
    let sessions := store.sessions.insert sessionId session
    (sessionId, { nextId := sessionId + 1, sessions })

private def findSession? (sessionId : Nat) : IO (Option DebugSession) := do
  return (← dapSessionStoreRef.get).sessions.get? sessionId

private def updateSession (sessionId : Nat) (session : DebugSession) : IO Unit := do
  dapSessionStoreRef.modify fun store =>
    { store with sessions := store.sessions.insert sessionId session }

private def eraseSession (sessionId : Nat) : IO Bool := do
  dapSessionStoreRef.modifyGet fun store =>
    let existed := (store.sessions.get? sessionId).isSome
    (existed, { store with sessions := store.sessions.erase sessionId })

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
  entryPoint : String := "mainProgram"
  line : Nat := 0
  character : Nat := 0
  stopOnEntry : Bool := true
  breakpoints : Array Nat := #[]
  deriving Inhabited, Repr, FromJson, ToJson

structure LaunchResponse where
  sessionId : Nat
  threadId : Nat
  line : Nat
  stopReason : String
  terminated : Bool
  deriving Inhabited, Repr, FromJson, ToJson

structure BreakpointView where
  line : Nat
  verified : Bool
  message? : Option String := none
  deriving Inhabited, Repr, FromJson, ToJson

structure SetBreakpointsParams where
  sessionId : Nat
  breakpoints : Array Nat := #[]
  deriving Inhabited, Repr, FromJson, ToJson

structure SetBreakpointsResponse where
  breakpoints : Array BreakpointView
  deriving Inhabited, Repr, FromJson, ToJson

structure ThreadView where
  id : Nat
  name : String
  deriving Inhabited, Repr, FromJson, ToJson

structure ThreadsResponse where
  threads : Array ThreadView
  deriving Inhabited, Repr, FromJson, ToJson

structure SessionParams where
  sessionId : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure ControlResponse where
  line : Nat
  stopReason : String
  terminated : Bool
  deriving Inhabited, Repr, FromJson, ToJson

structure StackFrameView where
  id : Nat
  name : String
  line : Nat
  column : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure StackTraceParams where
  sessionId : Nat
  startFrame : Nat := 0
  levels : Nat := 20
  deriving Inhabited, Repr, FromJson, ToJson

structure StackTraceResponse where
  stackFrames : Array StackFrameView
  totalFrames : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure ScopesParams where
  sessionId : Nat
  frameId : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

structure ScopeView where
  name : String
  variablesReference : Nat
  expensive : Bool := false
  deriving Inhabited, Repr, FromJson, ToJson

structure ScopesResponse where
  scopes : Array ScopeView
  deriving Inhabited, Repr, FromJson, ToJson

structure VariablesParams where
  sessionId : Nat
  variablesReference : Nat
  deriving Inhabited, Repr, FromJson, ToJson

structure VariableView where
  name : String
  value : String
  variablesReference : Nat := 0
  deriving Inhabited, Repr, FromJson, ToJson

structure VariablesResponse where
  variables : Array VariableView
  deriving Inhabited, Repr, FromJson, ToJson

structure DisconnectResponse where
  disconnected : Bool
  deriving Inhabited, Repr, FromJson, ToJson

private def mkInvalidParams (message : String) : RequestError :=
  RequestError.invalidParams message

private def getSessionOrThrow (sessionId : Nat) : RequestM DebugSession := do
  match ← findSession? sessionId with
  | some session => pure session
  | none => throw <| mkInvalidParams s!"Unknown DAP session id: {sessionId}"

private def mkControlResponse (session : DebugSession) (reason : StopReason) : ControlResponse :=
  { line := session.currentLine
    stopReason := toString reason
    terminated := session.atEnd || reason = .terminated }

private def currentFrameName (session : DebugSession) : String :=
  let pc := session.currentPc
  match session.trace.program[pc]? with
  | some stmt => toString stmt
  | none => "<terminated>"

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

private def launchFromProgram (program : Program) (stopOnEntry : Bool) (breakpoints : Array Nat) :
    RequestM LaunchResponse := do
  let session ←
    match DebugSession.fromProgram program with
    | .ok session => pure session
    | .error err => throw <| mkInvalidParams s!"Launch failed: {err}"
  let session := session.setBreakpoints breakpoints
  let (session, stopReason) := session.initialStop stopOnEntry
  let sessionId ← allocateSession session
  pure
    { sessionId
      threadId := 1
      line := session.currentLine
      stopReason := toString stopReason
      terminated := session.atEnd || stopReason = .terminated }

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
    let program ←
      match ← RequestM.runTermElabM snap do
        decodeProgramExpr? (mkConst resolvedName)
      with
      | some program => pure program
      | none =>
        throw <| mkInvalidParams
          s!"Could not decode '{resolvedName}' as Dap.Program at {lspPos}"
    launchFromProgram program params.stopOnEntry params.breakpoints

@[server_rpc_method]
def dapSetBreakpoints (params : SetBreakpointsParams) :
    RequestM (RequestTask SetBreakpointsResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    let programSize := session.trace.program.size
    let normalized := DebugSession.normalizeBreakpoints programSize params.breakpoints
    let session := { session with breakpoints := normalized }
    updateSession params.sessionId session
    let views :=
      params.breakpoints.map fun line =>
        let verified := DebugSession.isValidBreakpointLine programSize line
        let message? :=
          if verified then
            none
          else
            some s!"Line {line} is outside the valid range 1..{programSize}"
        { line, verified, message? : BreakpointView }
    pure { breakpoints := views }

@[server_rpc_method]
def dapThreads (_params : SessionParams) : RequestM (RequestTask ThreadsResponse) :=
  RequestM.pureTask do
    pure { threads := #[{ id := 1, name := "main" }] }

@[server_rpc_method]
def dapNext (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    let (session, reason) := session.next
    updateSession params.sessionId session
    pure (mkControlResponse session reason)

@[server_rpc_method]
def dapStepBack (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    let (session, reason) := session.stepBack
    updateSession params.sessionId session
    pure (mkControlResponse session reason)

@[server_rpc_method]
def dapContinue (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    let (session, reason) := session.continueExecution
    updateSession params.sessionId session
    pure (mkControlResponse session reason)

@[server_rpc_method]
def dapPause (params : SessionParams) : RequestM (RequestTask ControlResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    pure (mkControlResponse session .pause)

@[server_rpc_method]
def dapStackTrace (params : StackTraceParams) : RequestM (RequestTask StackTraceResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    let fullFrames :=
      #[{
        id := 0
        name := currentFrameName session
        line := session.currentLine
        column := 1
      : StackFrameView }]
    let frames :=
      if params.startFrame > 0 || params.levels = 0 then
        #[]
      else
        fullFrames
    pure
      { stackFrames := frames
        totalFrames := fullFrames.size }

@[server_rpc_method]
def dapScopes (params : ScopesParams) : RequestM (RequestTask ScopesResponse) :=
  RequestM.pureTask do
    let _ ← getSessionOrThrow params.sessionId
    if params.frameId = 0 then
      pure { scopes := #[{ name := "locals", variablesReference := 1 }] }
    else
      pure { scopes := #[] }

@[server_rpc_method]
def dapVariables (params : VariablesParams) : RequestM (RequestTask VariablesResponse) :=
  RequestM.pureTask do
    let session ← getSessionOrThrow params.sessionId
    if params.variablesReference != 1 then
      pure { variables := #[] }
    else
      let variables :=
        session.bindings.map fun (name, value) =>
          { name, value := toString value : VariableView }
      pure { variables }

@[server_rpc_method]
def dapDisconnect (params : SessionParams) : RequestM (RequestTask DisconnectResponse) :=
  RequestM.pureTask do
    let disconnected ← eraseSession params.sessionId
    pure { disconnected }

end Dap.Server
