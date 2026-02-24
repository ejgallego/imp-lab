/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

namespace ImpLab.History

def maxCursor (items : Array α) : Nat :=
  items.size - 1

def normalizeCursor (items : Array α) (cursor : Nat) : Nat :=
  min cursor (maxCursor items)

def current? (items : Array α) (cursor : Nat) : Option α :=
  items[normalizeCursor items cursor]?

def hasPrev (_items : Array α) (cursor : Nat) : Bool :=
  cursor > 0

def hasNext (items : Array α) (cursor : Nat) : Bool :=
  let cursor := normalizeCursor items cursor
  cursor + 1 < items.size

def backCursor (items : Array α) (cursor : Nat) : Nat :=
  let cursor := normalizeCursor items cursor
  if hasPrev items cursor then
    cursor - 1
  else
    cursor

def forwardCursor (items : Array α) (cursor : Nat) : Nat :=
  if hasNext items cursor then
    normalizeCursor items cursor + 1
  else
    normalizeCursor items cursor

def jumpCursor (items : Array α) (cursor : Nat) : Nat :=
  normalizeCursor items cursor

end ImpLab.History
