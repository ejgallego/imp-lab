/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Lean

open Lean

namespace ImpLab

structure DapCapabilities where
  supportsConfigurationDoneRequest : Bool := true
  supportsStepBack : Bool := true
  supportsRestartRequest : Bool := false
  deriving Inhabited, Repr, FromJson, ToJson

def dapCapabilities : DapCapabilities :=
  {}

end ImpLab
