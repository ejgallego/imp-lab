/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import ImpLab

namespace ImpLab.Tests

def assertEq [BEq α] [ToString α] (label : String) (actual expected : α) : IO Unit := do
  if actual == expected then
    pure ()
  else
    throw <| IO.userError s!"{label}: expected `{expected}`, got `{actual}`"

def assertSomeEq [BEq α] [ToString α] (label : String) (actual : Option α) (expected : α) : IO Unit := do
  match actual with
  | some value => assertEq label value expected
  | none => throw <| IO.userError s!"{label}: expected `some {expected}`, got `none`"

def assertTrue (label : String) (condition : Bool) : IO Unit := do
  if condition then
    pure ()
  else
    throw <| IO.userError s!"{label}: expected condition to hold"

def expectCore (label : String) (result : Except String α) : IO α := do
  match result with
  | .ok value =>
    pure value
  | .error err =>
    throw <| IO.userError s!"{label}: {err}"

end ImpLab.Tests
