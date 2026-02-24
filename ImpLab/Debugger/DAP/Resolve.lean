/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean

open Lean

namespace ImpLab

def parseDeclName? (raw : String) : Option Name :=
  let parts := raw.trimAscii.toString.splitOn "." |>.filter (· != "")
  match parts with
  | [] => none
  | _ =>
    some <| parts.foldl Name.str Name.anonymous

def isUnqualifiedName (n : Name) : Bool :=
  match n with
  | .str .anonymous _ => true
  | .num .anonymous _ => true
  | _ => false

private def pushIfMissing (names : Array Name) (name : Name) : Array Name :=
  if names.contains name then names else names.push name

def candidateDeclNames
    (decl : Name)
    (moduleName? : Option Name := none)
    (includeExamples : Bool := true) : Array Name :=
  if isUnqualifiedName decl then
    let names := #[decl]
    let names :=
      match moduleName? with
      | some moduleName =>
        if moduleName == .anonymous then
          names
        else
          pushIfMissing names (moduleName ++ decl)
      | none =>
        names
    if includeExamples then
      pushIfMissing names (`ImpLab.Lang.Examples ++ decl)
    else
      names
  else
    #[decl]

def resolveFirstDecl? (env : Environment) (candidates : Array Name) : Option Name :=
  candidates.find? env.contains

def renderCandidateDecls (candidates : Array Name) : String :=
  String.intercalate ", " <| candidates.toList.map (fun n => s!"'{n}'")

def importProjectEnv : IO Environment := do
  let candidates : Array (Array Name) :=
    #[#[`Main, `ImpLab], #[`Main], #[`ImpLab]]
  let rec go (idx : Nat) : IO Environment := do
    if h : idx < candidates.size then
      let modules := candidates[idx]
      let imports : Array Import := modules.map (fun module => { module })
      try
        importModules imports {}
      catch _ =>
        go (idx + 1)
    else
      throw <| IO.userError "Could not import project modules (`Main` or `ImpLab`) to resolve declaration"
  go 0

end ImpLab
