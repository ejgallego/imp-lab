/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import Dap.Lang.Ast
import examples.Main

open Lean

namespace Dap.Export

structure CliOptions where
  decl : String := "mainProgramInfo"
  out : System.FilePath := System.FilePath.mk ".dap/programInfo.generated.json"
  pretty : Bool := true
  deriving Inhabited

def usage : String := String.intercalate "\n"
  [ "Usage: lake exe dap-export [--decl <name>] [--out <path>] [--compact]",
    "",
    "Export a DAP payload from a Lean declaration.",
    "",
    "--decl may point to either:",
    "  - Dap.ProgramInfo (preferred, preserves spans), or",
    "  - Dap.Program (exported as ProgramInfo with empty `located`).",
    "",
    "Default: --decl mainProgramInfo",
    "Name resolution for unqualified names tries:",
    "  1) <name>",
    "  2) Main.<name>",
    "  3) Dap.Lang.Examples.<name>" ]

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

private def candidateDeclNames (decl : Name) : Array Name :=
  if isUnqualifiedName decl then
    #[decl, `Main ++ decl, `Dap.Lang.Examples ++ decl]
  else
    #[decl]

private def importProjectEnv : IO Environment := do
  let candidates : Array (Array Name) :=
    #[#[`Main, `Dap], #[`Main], #[`Dap]]
  let rec go (idx : Nat) : IO Environment := do
    if h : idx < candidates.size then
      let modules := candidates[idx]
      let imports : Array Import := modules.map (fun module => { module })
      try
        importModules imports {}
      catch _ =>
        go (idx + 1)
    else
      throw <| IO.userError "Could not import project modules (`Main` or `Dap`) to resolve declaration"
  go 0

private unsafe def evalProgramInfoOrProgram
    (env : Environment) (opts : Options) (decl : Name) : Except String ProgramInfo := do
  match env.evalConstCheck ProgramInfo opts ``Dap.ProgramInfo decl with
  | .ok info =>
    info.validate
  | .error infoErr =>
    match env.evalConstCheck Program opts ``Dap.Program decl with
    | .ok program =>
      pure { program, located := #[] }
    | .error programErr =>
      throw s!"Declaration '{decl}' is neither Dap.ProgramInfo nor Dap.Program.\nProgramInfo error: {infoErr}\nProgram error: {programErr}"

private def loadProgramInfoFromDecl (rawDecl : String) : IO ProgramInfo := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot [System.FilePath.mk ".lake/build/lib/lean"]
  let declName ←
    match parseDeclName? rawDecl with
    | some n => pure n
    | none => throw <| IO.userError s!"Invalid declaration name '{rawDecl}'"
  let env ← importProjectEnv
  let opts : Options := {}
  let candidates := candidateDeclNames declName
  let resolved? := candidates.find? env.contains
  let resolved ←
    match resolved? with
    | some n => pure n
    | none =>
      let attempted := String.intercalate ", " <| candidates.toList.map (fun n => s!"'{n}'")
      throw <| IO.userError s!"Could not resolve declaration '{rawDecl}'. Tried: {attempted}"
  match unsafe evalProgramInfoOrProgram env opts resolved with
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

end Dap.Export
