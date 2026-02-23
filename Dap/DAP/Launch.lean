/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Lang.Ast
import Dap.DAP.Resolve

open Lean

namespace Dap

/-- Launch payload normalized to the runtime program plus optional source spans. -/
structure LaunchProgram where
  program : Program
  stmtSpans : Array StmtSpan := #[]

namespace LaunchProgram

def ofProgramInfo (programInfo : ProgramInfo) : LaunchProgram :=
  { program := programInfo.program, stmtSpans := programInfo.stmtSpans }

def ofProgram (program : Program) : LaunchProgram :=
  { program, stmtSpans := #[] }

end LaunchProgram

def decodeProgramJson (json : Json) : Except String Program :=
  match fromJson? json with
  | .ok program => pure program
  | .error err => throw s!"Invalid 'program' payload: {err}"

def decodeProgramInfoJson (json : Json) : Except String ProgramInfo :=
  match (fromJson? json : Except String ProgramInfo) with
  | .ok programInfo =>
    match programInfo.validate with
    | .ok info => pure info
    | .error err => throw err
  | .error err => throw s!"Invalid 'programInfo' payload: {err}"

def decodeLaunchProgramJson (json : Json) : Except String LaunchProgram :=
  match decodeProgramInfoJson json with
  | .ok programInfo =>
    pure (LaunchProgram.ofProgramInfo programInfo)
  | .error _ =>
    match decodeProgramJson json with
    | .ok program =>
      pure (LaunchProgram.ofProgram program)
    | .error err =>
      throw s!"Invalid program payload: {err}"

private unsafe def evalLaunchProgramFromDecl
    (env : Environment) (opts : Options) (decl : Name) : Except String LaunchProgram := do
  match env.evalConstCheck ProgramInfo opts ``Dap.ProgramInfo decl with
  | .ok rawProgramInfo =>
    let programInfo ← rawProgramInfo.validate
    pure (LaunchProgram.ofProgramInfo programInfo)
  | .error infoErr =>
    match env.evalConstCheck Program opts ``Dap.Program decl with
    | .ok program =>
      pure (LaunchProgram.ofProgram program)
    | .error programErr =>
      throw s!"Declaration '{decl}' is neither Dap.ProgramInfo nor Dap.Program.\nProgramInfo error: {infoErr}\nProgram error: {programErr}"

def resolveLaunchProgramFromEnv
    (env : Environment)
    (entryPoint : String)
    (moduleName? : Option Name := none)
    (opts : Options := {}) : Except String LaunchProgram := do
  let declName ←
    match Dap.parseDeclName? entryPoint with
    | some n => pure n
    | none => throw s!"Invalid entryPoint '{entryPoint}'"
  let candidates := Dap.candidateDeclNames declName (moduleName? := moduleName?)
  let resolved ←
    match Dap.resolveFirstDecl? env candidates with
    | some n => pure n
    | none =>
      let attempted := Dap.renderCandidateDecls candidates
      throw s!"Could not resolve entryPoint '{entryPoint}'. Tried: {attempted}"
  match unsafe evalLaunchProgramFromDecl env opts resolved with
  | .ok launchProgram => pure launchProgram
  | .error err => throw err

end Dap
