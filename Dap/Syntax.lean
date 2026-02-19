namespace Dap

/-- Variable names in the toy language. -/
abbrev Var := String

/-- Arithmetic operators supported by the language. -/
inductive BinOp where
  | add
  | sub
  | mul
  | div
  deriving Repr, BEq, DecidableEq, Inhabited

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
  deriving Repr, BEq, DecidableEq, Inhabited

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
  deriving Repr, BEq, DecidableEq, Inhabited

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

def Program.render (program : Program) : Array String :=
  program.map toString

end Dap
