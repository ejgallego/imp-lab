/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean
import ImpLab.Lang.Ast

open Lean

namespace ImpLab

def decodeProgramInfoJson (json : Json) : Except String ProgramInfo :=
  match (fromJson? json : Except String ProgramInfo) with
  | .ok programInfo =>
    programInfo.validate
  | .error err =>
    throw s!"Invalid 'programInfo' payload: {err}"

end ImpLab
