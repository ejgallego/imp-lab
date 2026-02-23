import Lean

open Lean

namespace Dap

/-- Variable names in the toy language. -/
abbrev Var := String

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
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

instance : ToString Rhs where
  toString
    | .const value => s!"{value}"
    | .bin op lhs rhs => s!"{op} {lhs} {rhs}"

/--
A program statement in let-normal form:
`let dest := constant` or `let dest := op lhs rhs`.
-/
structure Stmt where
  dest : Var
  rhs : Rhs
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

namespace Stmt

def letConst (dest : Var) (value : Int) : Stmt :=
  { dest, rhs := .const value }

def letBin (dest : Var) (op : BinOp) (lhs rhs : Var) : Stmt :=
  { dest, rhs := .bin op lhs rhs }

end Stmt

instance : ToString Stmt where
  toString stmt := s!"let {stmt.dest} := {stmt.rhs}"

/-- A program is a sequence of let statements. -/
abbrev Program := Array Stmt

/-- Source span for one statement in the DSL program literal. -/
structure StmtSpan where
  startLine : Nat
  startColumn : Nat
  endLine : Nat
  endColumn : Nat
  deriving Repr, BEq, DecidableEq, Inhabited, FromJson, ToJson, ToExpr

/-- Statement plus its source span in the originating Lean file. -/
structure LocatedStmt where
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

def Program.render (program : Program) : Array String :=
  program.map toString

namespace ProgramInfo

def stmtSpans (info : ProgramInfo) : Array StmtSpan :=
  info.located.map (·.span)

def lineToStmtIdx? (info : ProgramInfo) (line : Nat) : Option Nat :=
  let rec go (idx : Nat) : Option Nat :=
    if h : idx < info.located.size then
      let located := info.located[idx]
      if located.span.startLine ≤ line && line ≤ located.span.endLine then
        some idx
      else
        go (idx + 1)
    else
      none
  go 0

end ProgramInfo

end Dap
