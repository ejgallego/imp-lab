import Dap.Examples

open Lean

namespace Dap.Export

structure CliOptions where
  decl : String := "Dap.Examples.mainProgramInfo"
  out : System.FilePath := System.FilePath.mk ".dap/programInfo.generated.json"
  pretty : Bool := true
  deriving Inhabited

def usage : String := String.intercalate "\n"
  [ "Usage: lake exe dap-export [--decl <name>] [--out <path>] [--compact]",
    "",
    "Export a source-aware DAP program payload (`Dap.ProgramInfo`) to JSON.",
    "",
    "Supported --decl values:",
    "  Dap.Examples.mainProgramInfo (default)",
    "  Dap.Examples.sampleProgramInfo",
    "  mainProgramInfo",
    "  sampleProgramInfo" ]

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

private def resolveProgramInfo (rawDecl : String) : Except String ProgramInfo := do
  let decl := normalizeDeclName rawDecl
  match decl with
  | "Dap.Examples.mainProgramInfo" | "mainProgramInfo" =>
    pure Dap.Examples.mainProgramInfo
  | "Dap.Examples.sampleProgramInfo" | "sampleProgramInfo" =>
    pure Dap.Examples.sampleProgramInfo
  | _ =>
    throw s!"Unsupported --decl '{rawDecl}'.\n\n{usage}"

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
  let programInfo ←
    match resolveProgramInfo opts.decl with
    | .ok info => pure info
    | .error err => throw <| IO.userError err
  let content := renderJson programInfo opts.pretty
  writeJsonFile opts.out content
  IO.println s!"Wrote {opts.out} from {normalizeDeclName opts.decl}"

end Dap.Export
