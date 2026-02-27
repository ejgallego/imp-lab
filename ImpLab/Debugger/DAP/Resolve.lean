/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean

open Lean

namespace ImpLab

def parseName? (raw : String) : Option Name :=
  let parts := raw.trimAscii.toString.splitOn "." |>.filter (· != "")
  match parts with
  | [] => none
  | _ =>
    some <| parts.foldl Name.str Name.anonymous

def parseDeclName? (raw : String) : Option Name :=
  parseName? raw

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

private def importFirstAvailableEnv
    (candidates : Array (Array Name))
    (errMsg : String) : IO Environment := do
  let rec go (idx : Nat) : IO Environment := do
    if h : idx < candidates.size then
      let modules := candidates[idx]
      let imports : Array Import := modules.map (fun module => { module })
      try
        importModules imports {}
      catch _ =>
        go (idx + 1)
    else
      throw <| IO.userError errMsg
  go 0

def importProjectEnv : IO Environment := do
  importFirstAvailableEnv
    #[#[`Main, `ImpLab], #[`Main], #[`ImpLab]]
    "Could not import project modules (`Main` or `ImpLab`) to resolve declaration"

def importEnvForModule (moduleName : Name) : IO Environment := do
  let candidates : Array (Array Name) :=
    if moduleName == `ImpLab then
      #[#[`ImpLab]]
    else if moduleName == `Main then
      #[#[`Main, `ImpLab], #[`Main], #[`ImpLab]]
    else
      #[#[moduleName, `ImpLab], #[moduleName]]
  importFirstAvailableEnv
    candidates
    s!"Could not import module '{moduleName}' to resolve declaration"

end ImpLab
