/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Lang.Ast
import ImpLab.Debugger.DAP.Resolve
import examples.Main

open Lean

namespace ImpLab.Export

structure CliOptions where
  decl : String := "mainProgram"
  out : System.FilePath := System.FilePath.mk ".dap/programInfo.generated.json"
  pretty : Bool := true
  deriving Inhabited

def usage : String := String.intercalate "\n"
  [ "Usage: lake exe dap-export [--decl <name>] [--out <path>] [--compact]",
    "",
    "Export a DAP payload from a Lean declaration.",
    "",
    "--decl must point to a ImpLab.ProgramInfo declaration.",
    "",
    "Default: --decl mainProgram",
    "Name resolution for unqualified names tries:",
    "  1) <name>",
    "  2) Main.<name>",
    "  3) ImpLab.Lang.Examples.<name>" ]

private def parseArgs : CliOptions → List String → Except String CliOptions
  | opts, [] =>
    pure opts
  | opts, "--decl" :: value :: rest =>
    parseArgs { opts with decl := value } rest
  | opts, "--out" :: value :: rest =>
    parseArgs { opts with out := System.FilePath.mk value } rest
  | opts, "--compact" :: rest =>
    parseArgs { opts with pretty := false } rest
  | opts, "--pretty" :: rest =>
    parseArgs { opts with pretty := true } rest
  | _, "--decl" :: [] =>
    throw "Missing value for --decl"
  | _, "--out" :: [] =>
    throw "Missing value for --out"
  | _, arg :: _ =>
    throw s!"Unknown argument '{arg}'"

private def normalizeDeclName (raw : String) : String :=
  raw.trimAscii.toString

private unsafe def evalProgramInfo
    (env : Environment) (opts : Options) (decl : Name) : Except String ProgramInfo := do
  match env.evalConstCheck ProgramInfo opts ``ImpLab.ProgramInfo decl with
  | .ok info =>
    info.validate
  | .error infoErr =>
    throw s!"Declaration '{decl}' is not ImpLab.ProgramInfo.\nProgramInfo error: {infoErr}"

private def loadProgramInfoFromDecl (rawDecl : String) : IO ProgramInfo := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot [System.FilePath.mk ".lake/build/lib/lean"]
  let declName ←
    match ImpLab.parseDeclName? rawDecl with
    | some n => pure n
    | none => throw <| IO.userError s!"Invalid declaration name '{rawDecl}'"
  let env ← ImpLab.importProjectEnv
  let opts : Options := {}
  let candidates := ImpLab.candidateDeclNames declName (moduleName? := some `Main)
  let resolved? := ImpLab.resolveFirstDecl? env candidates
  let resolved ←
    match resolved? with
    | some n => pure n
    | none =>
      let attempted := ImpLab.renderCandidateDecls candidates
      throw <| IO.userError s!"Could not resolve declaration '{rawDecl}'. Tried: {attempted}"
  match unsafe evalProgramInfo env opts resolved with
  | .ok info => pure info
  | .error err => throw <| IO.userError err

private def renderJson (programInfo : ProgramInfo) (pretty : Bool) : String :=
  let json := toJson programInfo
  if pretty then
    json.pretty
  else
    json.compress

private def writeJsonFile (output : System.FilePath) (content : String) : IO Unit := do
  if let some parent := output.parent then
    IO.FS.createDirAll parent
  IO.FS.writeFile output content

def run (args : List String) : IO Unit := do
  if args.contains "--help" || args.contains "-h" then
    IO.println usage
    return
  let opts ←
    match parseArgs default args with
    | .ok opts => pure opts
    | .error err => throw <| IO.userError err
  let programInfo ← loadProgramInfoFromDecl opts.decl
  let content := renderJson programInfo opts.pretty
  writeJsonFile opts.out content
  IO.println s!"Wrote {opts.out} from {normalizeDeclName opts.decl}"

end ImpLab.Export
