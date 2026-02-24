/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean

open Lean

namespace ImpLab

/-- Variable names in the toy language. -/
abbrev Var := String

/-- Function names in the toy language. -/
abbrev FuncName := String

/-- Global variable names in the toy language. -/
abbrev GlobalName := String

/-- Arithmetic operators supported by the language. -/
inductive BinOp where
  | add
  | sub
  | mul
  | div
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

instance : ToString BinOp where
  toString
    | .add => "add"
    | .sub => "sub"
    | .mul => "mul"
    | .div => "div"

/-- Right-hand side of a let statement. -/
inductive Rhs where
  | const (value : Int)
  | bin (op : BinOp) (lhs rhs : Var)
  | call (fn : FuncName) (args : Array Var)
  | get (name : GlobalName)
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

instance : ToString Rhs where
  toString
    | .const value => s!"{value}"
    | .bin op lhs rhs => s!"{op} {lhs} {rhs}"
    | .call fn args =>
      s!"call {fn}({String.intercalate ", " args.toList})"
    | .get name =>
      s!"get {name}"

/--
A program statement:
`let dest := rhs` or `return value`.
-/
inductive Stmt where
  | assign (dest : Var) (rhs : Rhs)
  | set (name : GlobalName) (value : Var)
  | return_ (value : Var)
  deriving Repr, BEq, DecidableEq, Inhabited, ToExpr

namespace Stmt

def letConst (dest : Var) (value : Int) : Stmt :=
  .assign dest (.const value)

def letBin (dest : Var) (op : BinOp) (lhs rhs : Var) : Stmt :=
  .assign dest (.bin op lhs rhs)

def letCall (dest : Var) (fn : FuncName) (args : Array Var) : Stmt :=
  .assign dest (.call fn args)

def letGet (dest : Var) (name : GlobalName) : Stmt :=
  .assign dest (.get name)

def setGlobal (name : GlobalName) (value : Var) : Stmt :=
  .set name value

def ret (value : Var) : Stmt :=
  .return_ value

end Stmt

instance : ToString Stmt where
  toString
    | .assign dest rhs => s!"let {dest} := {rhs}"
    | .set name value => s!"set {name} := {value}"
    | .return_ value => s!"return {value}"

instance : ToJson Stmt where
  toJson
    | .assign dest rhs =>
      Json.mkObj [("dest", toJson dest), ("rhs", toJson rhs)]
    | .set name value =>
      Json.mkObj [("set", Json.mkObj [("name", toJson name), ("value", toJson value)])]
    | .return_ value =>
      Json.mkObj [("return", toJson value)]

instance : FromJson Stmt where
  fromJson? json := do
    if let some returnJson := (json.getObjVal? "return").toOption then
      return .return_ (← fromJson? returnJson)
    else if let some setJson := (json.getObjVal? "set").toOption then
      return .set
        (← setJson.getObjValAs? GlobalName "name")
        (← setJson.getObjValAs? Var "value")
    else
      return .assign
        (← json.getObjValAs? Var "dest")
        (← json.getObjValAs? Rhs "rhs")

/-- Top-level mutable global declaration with initial value. -/
structure GlobalDecl where
  name : GlobalName
  init : Int
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

/-- Function declaration in the toy language. -/
structure FuncDef where
  name : FuncName
  params : Array Var := #[]
  body : Array Stmt := #[]
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

instance : ToString FuncDef where
  toString fn :=
    s!"def {fn.name}({String.intercalate ", " fn.params.toList})"

/-- Program is a list of global declarations and functions. Entry point function is `main`. -/
structure Program where
  globals : Array GlobalDecl := #[]
  functions : Array FuncDef := #[]
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

namespace Program

def mainName : FuncName := "main"

def findGlobal? (program : Program) (name : GlobalName) : Option GlobalDecl :=
  program.globals.find? (fun g => g.name = name)

def isDeclaredGlobal (program : Program) (name : GlobalName) : Bool :=
  (program.findGlobal? name).isSome

def findFunction? (program : Program) (name : FuncName) : Option FuncDef :=
  program.functions.find? (fun fn => fn.name = name)

def mainFunction? (program : Program) : Option FuncDef :=
  program.findFunction? mainName

def mainBody? (program : Program) : Option (Array Stmt) :=
  (program.mainFunction?).map (·.body)

def bodyOf? (program : Program) (name : FuncName) : Option (Array Stmt) :=
  (findFunction? program name).map (·.body)

def bodySizeOf (program : Program) (name : FuncName) : Nat :=
  (bodyOf? program name).map (·.size) |>.getD 0

def stmtAt? (program : Program) (name : FuncName) (pc : Nat) : Option Stmt :=
  (bodyOf? program name).bind (·[pc]?)

def size (program : Program) : Nat :=
  (program.mainBody?).map (·.size) |>.getD 0

def render (program : Program) : Array String :=
  (program.mainBody?).map (fun body => body.map toString) |>.getD #[]

def totalStmtCount (program : Program) : Nat :=
  program.functions.foldl (init := 0) fun acc fn => acc + fn.body.size

def defaultFuel (program : Program) : Nat :=
  (max program.totalStmtCount 1) * 64 + 1

def hasMain (program : Program) : Bool :=
  (program.mainFunction?).isSome

def mainHasNoParams (program : Program) : Bool :=
  match program.mainFunction? with
  | some fn => fn.params.isEmpty
  | none => false

def hasDuplicateGlobalNames (program : Program) : Bool :=
  (program.globals.foldl (init := ((#[] : Array GlobalName), false)) fun (seen, dup) global =>
    if dup then
      (seen, true)
    else if seen.contains global.name then
      (seen, true)
    else
      (seen.push global.name, false)).2

end Program

/-- Source span for one statement in the DSL program literal. -/
structure StmtSpan where
  startLine : Nat
  startColumn : Nat
  endLine : Nat
  endColumn : Nat
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

/-- Function-local statement location (1-based statement line). -/
structure StmtLocation where
  func : FuncName
  stmtLine : Nat
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

/-- Statement plus its function and source span in the originating Lean file. -/
structure LocatedStmt where
  func : FuncName
  stmtLine : Nat
  stmt : Stmt
  span : StmtSpan
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

/--
Program enriched with statement source locations.
Useful for DAP/source mapping where runtime steps must map back to source ranges.
-/
structure ProgramInfo where
  program : Program
  located : Array LocatedStmt
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

instance : Coe ProgramInfo Program where
  coe info := info.program

namespace ProgramInfo

def hasCompatibleLocations (info : ProgramInfo) : Bool :=
  info.located.size = info.program.totalStmtCount

def validate (info : ProgramInfo) : Except String ProgramInfo := do
  if !info.program.hasMain then
    throw "Invalid ProgramInfo: missing required entry function `main`."
  if !info.program.mainHasNoParams then
    throw "Invalid ProgramInfo: `main` must have zero parameters."
  if info.program.hasDuplicateGlobalNames then
    throw "Invalid ProgramInfo: duplicate global names are not allowed."
  if !info.hasCompatibleLocations then
    throw <|
      s!"Invalid ProgramInfo: program has {info.program.totalStmtCount} statements but `located` has {info.located.size}."
  pure info

def sourceLineToLocation? (info : ProgramInfo) (line : Nat) : Option StmtLocation :=
  let rec go (idx : Nat) : Option StmtLocation :=
    if h : idx < info.located.size then
      let located := info.located[idx]
      if located.span.startLine ≤ line && line ≤ located.span.endLine then
        some { func := located.func, stmtLine := located.stmtLine }
      else
        go (idx + 1)
    else
      none
  go 0

def locationToSourceLine? (info : ProgramInfo) (loc : StmtLocation) : Option Nat := do
  let found := info.located.find? (fun located => located.func = loc.func && located.stmtLine = loc.stmtLine)
  found.map (·.span.startLine)

def locationToSourceLine (info : ProgramInfo) (loc : StmtLocation) : Nat :=
  (locationToSourceLine? info loc).getD loc.stmtLine

end ProgramInfo

end ImpLab
