/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/

import Test.Util

open ImpLab
open Lean

namespace ImpLab.Tests

private def encodeDapRequest (seq : Nat) (command : String) (arguments : Json := Json.mkObj []) : String :=
  let payload := Json.mkObj
    [ ("seq", toJson seq),
      ("type", toJson "request"),
      ("command", toJson command),
      ("arguments", arguments) ]
  let data := Lean.Json.compress payload
  s!"Content-Length: {String.utf8ByteSize data}\r\n\r\n{data}"

private def runToyDapPayload (name : String) (stdinPayload : String) : IO String := do
  let inputPath := s!".dap/{name}.stdin"
  IO.FS.createDirAll ".dap"
  IO.FS.writeFile inputPath stdinPayload
  let out ← IO.Process.output
    { cmd := "bash"
      args := #["-lc", s!".lake/build/bin/toydap < {inputPath}"] }
  try
    IO.FS.removeFile inputPath
  catch _ =>
    pure ()
  assertEq s!"toydap ({name}) exits cleanly" out.exitCode 0
  pure out.stdout

private def appearsBefore (s first second : String) : Bool :=
  match s.splitOn first with
  | [] =>
    false
  | _ :: tail =>
    let afterFirst := String.intercalate first tail
    afterFirst.contains second

private def transportProgramInfo : ProgramInfo := imp%[
  def bump(x) := {
    let one := 1,
    let out := add x one,
    return out
  },
  def scaleAndShift(x, factor) := {
    let scaled := mul x factor,
    let shift := 2,
    let out := add scaled shift,
    return out
  },
  def main() := {
    let seed := 5,
    let factor := 3,
    let bumped := call bump(seed),
    let out := call scaleAndShift(bumped, factor)
  }
]

private def launchArgs (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfo", toJson transportProgramInfo),
      ("stopOnEntry", toJson stopOnEntry) ]

private def exceptionProgramInfo : ProgramInfo := imp%[
  def main() := {
    let x := 10,
    let y := 0,
    let z := div x y
  }
]

private def exceptionLaunchArgs (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfo", toJson exceptionProgramInfo),
      ("stopOnEntry", toJson stopOnEntry) ]

private def failingTransportProgramInfo : ProgramInfo := imp%[
  def bump(x) := {
    let one := 1,
    let out := add x one,
    return out
  },
  def scaleAndShift(x, factor) := {
    let scaled := mul x factor,
    let uh := 0,
    let mayfail := div scaled uh,
    let shift := 2,
    let out := add scaled shift,
    return out
  },
  def main() := {
    let seed := 5,
    let factor := 3,
    let bumped := call bump(seed),
    let out := call scaleAndShift(bumped, factor)
  }
]

private def failingLaunchArgs (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfo", toJson failingTransportProgramInfo),
      ("stopOnEntry", toJson stopOnEntry) ]

private def globalProgramInfo : ProgramInfo := imp%[
  global counter := 0,
  def main() := {
    let one := 1
  }
]

private def launchArgsWithGlobals (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfo", toJson globalProgramInfo),
      ("stopOnEntry", toJson stopOnEntry) ]

private def launchArgsFromRef (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfoRef", Json.mkObj
      [ ("module", toJson "examples.Main"),
        ("decl", toJson "Dap.Lang.Examples.mainProgram") ]),
      ("stopOnEntry", toJson stopOnEntry) ]

private def bumpEntryLine : Nat :=
  transportProgramInfo.locationToSourceLine { func := "bump", stmtLine := 1 }

private def mainEntryLine : Nat :=
  transportProgramInfo.locationToSourceLine { func := Program.mainName, stmtLine := 1 }

def testToyDapProtocolSanity : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "threads",
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.sanity" stdinPayload
  assertTrue "initialize response present"
    (stdout.contains "\"request_seq\":1" && stdout.contains "\"command\":\"initialize\"")
  assertTrue "initialized event present"
    (stdout.contains "\"event\":\"initialized\"")
  assertTrue "launch response present"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"launch\"")
  assertTrue "threads response present"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"threads\"")
  assertTrue "next response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"next\"")
  assertTrue "disconnect response present"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"disconnect\"")

def testToyDapLaunchFromProgramInfoRef : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgsFromRef true,
        encodeDapRequest 3 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.launch.programinforef" stdinPayload
  assertTrue "programInfoRef launch response present"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"launch\"")
  if stdout.contains "\"success\":false" then
    throw <| IO.userError s!"programInfoRef launch has an error response: {stdout}"

def testToyDapLaunchFromProgramInfoRefRejectsUnknownModule : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| Json.mkObj
          [ ("programInfoRef", Json.mkObj
            [ ("module", toJson "No.Such.Module"),
              ("decl", toJson "mainProgram") ]),
            ("stopOnEntry", toJson true) ],
        encodeDapRequest 3 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.launch.programinforef.invalid.module" stdinPayload
  assertTrue "invalid module launch request gets an error response"
    (stdout.contains "\"request_seq\":2" &&
      stdout.contains "Could not import module 'No.Such.Module'")

def testToyDapBreakpointProtocol : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setBreakpoints" <| Json.mkObj
          [ ("breakpoints", Json.arr #[Json.mkObj [("line", toJson mainEntryLine)]]) ],
        encodeDapRequest 3 "launch" <| launchArgs false,
        encodeDapRequest 4 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.breakpoint" stdinPayload
  assertTrue "setBreakpoints response present"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"setBreakpoints\"")
  assertTrue "pre-launch breakpoint stays pending"
    (stdout.contains "\"verified\":false")
  assertTrue "launch response present in breakpoint flow"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"launch\"")
  if stdout.contains "\"event\":\"stopped\"" && stdout.contains "\"reason\":\"breakpoint\"" then
    pure ()
  else
    throw <| IO.userError s!"breakpoint stop reason missing in output: {stdout}"

def testToyDapContinueEventOrder : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "continue",
        encodeDapRequest 4 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.continue" stdinPayload
  let continuedMarker := "\"event\":\"continued\""
  let continueRespMarker := "\"request_seq\":3"
  assertTrue "continued event is emitted" (stdout.contains continuedMarker)
  assertTrue "continue response is emitted" (stdout.contains continueRespMarker)
  let hasStopOrTerm :=
    stdout.contains "\"event\":\"stopped\"" || stdout.contains "\"event\":\"terminated\""
  assertTrue "post-continue stop/terminate event is emitted" hasStopOrTerm
  assertTrue "continued precedes continue response"
    (appearsBefore stdout continuedMarker continueRespMarker)
  assertTrue "stop/terminate follows continue response"
    (appearsBefore stdout continueRespMarker "\"event\":\"stopped\"" ||
      appearsBefore stdout continueRespMarker "\"event\":\"terminated\"")

def testToyDapStepInOutProtocol : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "stepIn",
        encodeDapRequest 4 "stepIn",
        encodeDapRequest 5 "stepOut",
        encodeDapRequest 6 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.stepinout" stdinPayload
  assertTrue "stepIn response present"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"stepIn\"")
  assertTrue "second stepIn response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"stepIn\"")
  assertTrue "stepOut response present"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"stepOut\"")
  assertTrue "stepOut emits a stop/terminate event"
    (appearsBefore stdout "\"request_seq\":5" "\"event\":\"stopped\"" ||
      appearsBefore stdout "\"request_seq\":5" "\"event\":\"terminated\"")

def testToyDapScopesExposeHeap : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgsWithGlobals true,
        encodeDapRequest 3 "scopes" <| Json.mkObj [("frameId", toJson (0 : Nat))],
        encodeDapRequest 4 "variables" <| Json.mkObj [("variablesReference", toJson (2 : Nat))],
        encodeDapRequest 5 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.scopes.heap" stdinPayload
  assertTrue "scopes response present"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"scopes\"")
  assertTrue "scopes include locals and heap"
    (stdout.contains "\"name\":\"locals\"" && stdout.contains "\"name\":\"heap\"")
  assertTrue "heap variables response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"variables\"")
  assertTrue "heap variables include declared global"
    (stdout.contains "\"name\":\"counter\"" && stdout.contains "\"value\":\"0\"")

def testToyDapSetVariableHeap : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgsWithGlobals true,
        encodeDapRequest 3 "setVariable" <| Json.mkObj
          [ ("variablesReference", toJson (2 : Nat)),
            ("name", toJson "counter"),
            ("value", toJson "7") ],
        encodeDapRequest 4 "variables" <| Json.mkObj [("variablesReference", toJson (2 : Nat))],
        encodeDapRequest 5 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.setvariable.heap" stdinPayload
  assertTrue "heap setVariable response present"
    (stdout.contains "\"request_seq\":3" && stdout.contains "\"command\":\"setVariable\"")
  assertTrue "heap setVariable reports updated value"
    (stdout.contains "\"value\":\"7\"")
  assertTrue "heap setVariable response is scalar (variablesReference = 0)"
    (appearsBefore stdout "\"request_seq\":3" "\"variablesReference\":0" &&
      appearsBefore stdout "\"variablesReference\":0" "\"request_seq\":4")
  assertTrue "heap variables reflect updated value"
    (stdout.contains "\"request_seq\":4" &&
      stdout.contains "\"name\":\"counter\"" &&
      stdout.contains "\"value\":\"7\"")

def testToyDapNextStepsOverCall : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "next",
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "next",
        encodeDapRequest 6 "stackTrace",
        encodeDapRequest 7 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.next.stepover" stdinPayload
  assertTrue "third next response present"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"next\"")
  assertTrue "stackTrace response present"
    (stdout.contains "\"request_seq\":6" && stdout.contains "\"command\":\"stackTrace\"")
  assertTrue "next over call returns to caller depth"
    (stdout.contains "\"totalFrames\":1")

def testToyDapNextCanStopAtCalleeBreakpoint : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setBreakpoints" <| Json.mkObj
          [ ("breakpoints", Json.arr #[Json.mkObj [("line", toJson bumpEntryLine)]]) ],
        encodeDapRequest 3 "launch" <| launchArgs true,
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "next",
        encodeDapRequest 6 "next",
        encodeDapRequest 7 "stackTrace",
        encodeDapRequest 8 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.next.callee.breakpoint" stdinPayload
  assertTrue "third next response present"
    (stdout.contains "\"request_seq\":6" && stdout.contains "\"command\":\"next\"")
  assertTrue "third next can stop on callee breakpoint"
    (appearsBefore stdout "\"request_seq\":6" "\"reason\":\"breakpoint\"")
  assertTrue "stackTrace response present in callee-breakpoint flow"
    (stdout.contains "\"request_seq\":7" && stdout.contains "\"command\":\"stackTrace\"")
  assertTrue "callee-breakpoint flow has two frames"
    (stdout.contains "\"totalFrames\":2")
  assertTrue "callee-breakpoint flow stops in bump frame"
    (stdout.contains "\"name\":\"bump:")

def testToyDapEvaluateAndSetVariable : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "next",
        encodeDapRequest 4 "evaluate" <| Json.mkObj [("expression", toJson "seed")],
        encodeDapRequest 5 "setVariable" <| Json.mkObj
          [ ("variablesReference", toJson (1 : Nat)),
            ("name", toJson "seed"),
            ("value", toJson "10") ],
        encodeDapRequest 6 "evaluate" <| Json.mkObj [("expression", toJson "seed + 1")],
        encodeDapRequest 7 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.evaluate.setvariable" stdinPayload
  assertTrue "evaluate response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"evaluate\"")
  assertTrue "evaluate reads pre-mutation value"
    (stdout.contains "\"result\":\"5\"")
  assertTrue "setVariable response present"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"setVariable\"")
  assertTrue "setVariable reports updated value"
    (stdout.contains "\"value\":\"10\"")
  assertTrue "locals setVariable response is scalar (variablesReference = 0)"
    (appearsBefore stdout "\"request_seq\":5" "\"variablesReference\":0" &&
      appearsBefore stdout "\"variablesReference\":0" "\"request_seq\":6")
  assertTrue "second evaluate response present"
    (stdout.contains "\"request_seq\":6" && stdout.contains "\"command\":\"evaluate\"")
  assertTrue "second evaluate observes mutation"
    (stdout.contains "\"result\":\"11\"")
  assertTrue "mutation result appears after setVariable response payload"
    (appearsBefore stdout "\"value\":\"10\"" "\"result\":\"11\"")

def testToyDapEvaluateAndSetVariableAcrossFrames : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "next",
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "stepIn",
        encodeDapRequest 6 "evaluate" <| Json.mkObj [("expression", toJson "seed")],
        encodeDapRequest 7 "evaluate" <| Json.mkObj
          [ ("expression", toJson "seed"),
            ("frameId", toJson (1 : Nat)) ],
        encodeDapRequest 8 "setVariable" <| Json.mkObj
          [ ("variablesReference", toJson (3 : Nat)),
            ("name", toJson "seed"),
            ("value", toJson "10") ],
        encodeDapRequest 9 "evaluate" <| Json.mkObj
          [ ("expression", toJson "seed + factor"),
            ("frameId", toJson (1 : Nat)) ],
        encodeDapRequest 10 "evaluate" <| Json.mkObj [("expression", toJson "x")],
        encodeDapRequest 11 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.evaluate.setvariable.frames" stdinPayload
  assertTrue "frame 0 evaluate missing caller variable reports error"
    (stdout.contains "\"request_seq\":6" &&
      stdout.contains "Unknown variable 'seed'")
  assertTrue "frame 1 evaluate sees caller variable"
    (stdout.contains "\"request_seq\":7" &&
      stdout.contains "\"result\":\"5\"")
  assertTrue "setVariable on caller frame response present"
    (stdout.contains "\"request_seq\":8" &&
      stdout.contains "\"command\":\"setVariable\"" &&
      stdout.contains "\"value\":\"10\"")
  assertTrue "setVariable caller-frame response is scalar (variablesReference = 0)"
    (appearsBefore stdout "\"request_seq\":8" "\"variablesReference\":0" &&
      appearsBefore stdout "\"variablesReference\":0" "\"request_seq\":9")
  assertTrue "frame 1 evaluate observes caller mutation"
    (stdout.contains "\"request_seq\":9" &&
      stdout.contains "\"result\":\"13\"")
  assertTrue "frame 0 evaluate still reads callee locals"
    (stdout.contains "\"request_seq\":10" &&
      stdout.contains "\"result\":\"5\"")
  assertTrue "caller mutation appears before frame-1 post-mutation evaluate result"
    (appearsBefore stdout "\"value\":\"10\"" "\"result\":\"13\"")

def testToyDapEvaluateAndSetVariableNegativePaths : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "next",
        encodeDapRequest 4 "evaluate" <| Json.mkObj [],
        encodeDapRequest 5 "evaluate" <| Json.mkObj [("expression", toJson "seed"), ("frameId", toJson (99 : Nat))],
        encodeDapRequest 6 "setVariable" <| Json.mkObj [("name", toJson "seed"), ("value", toJson "10")],
        encodeDapRequest 7 "setVariable" <| Json.mkObj
          [ ("variablesReference", toJson (0 : Nat)),
            ("name", toJson "seed"),
            ("value", toJson "10") ],
        encodeDapRequest 8 "setVariable" <| Json.mkObj
          [ ("variablesReference", toJson (1 : Nat)),
            ("name", toJson "seed"),
            ("value", toJson "missing + 1") ],
        encodeDapRequest 9 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.evaluate.setvariable.negative" stdinPayload
  assertTrue "evaluate missing expression rejects request"
    (stdout.contains "\"request_seq\":4" &&
      stdout.contains "evaluate requires arguments.expression")
  assertTrue "evaluate invalid frame rejects request"
    (stdout.contains "\"request_seq\":5" &&
      stdout.contains "Unknown stack frame id: 99")
  assertTrue "setVariable missing variablesReference rejects request"
    (stdout.contains "\"request_seq\":6" &&
      stdout.contains "setVariable requires arguments.variablesReference")
  assertTrue "setVariable zero variablesReference rejects request"
    (stdout.contains "\"request_seq\":7" &&
      stdout.contains "setVariable requires variablesReference > 0")
  assertTrue "setVariable invalid value expression rejects request"
    (stdout.contains "\"request_seq\":8" &&
      stdout.contains "Unknown variable 'missing'")

def testToyDapExceptionBreakpointsAndInfo : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setExceptionBreakpoints" <| Json.mkObj
          [ ("filters", Json.arr #[toJson "runtime"]) ],
        encodeDapRequest 3 "launch" <| exceptionLaunchArgs true,
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "next",
        encodeDapRequest 6 "next",
        encodeDapRequest 7 "exceptionInfo",
        encodeDapRequest 8 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.exception.info" stdinPayload
  assertTrue "setExceptionBreakpoints response present"
    (stdout.contains "\"request_seq\":2" &&
      stdout.contains "\"command\":\"setExceptionBreakpoints\"")
  assertTrue "failing next still produces a successful response when exception breakpoints are enabled"
    (stdout.contains "\"request_seq\":6" &&
      stdout.contains "\"command\":\"next\"" &&
      stdout.contains "\"success\":true")
  assertTrue "exception stop event emitted after failing step"
    (appearsBefore stdout "\"request_seq\":6" "\"reason\":\"exception\"")
  assertTrue "exception stop includes human-readable text"
    (stdout.contains "\"text\":\"division by zero")
  assertTrue "exceptionInfo response present"
    (stdout.contains "\"request_seq\":7" &&
      stdout.contains "\"command\":\"exceptionInfo\"")
  assertTrue "exceptionInfo includes divByZero id"
    (stdout.contains "\"exceptionId\":\"divByZero\"")
  assertTrue "exceptionInfo includes divide-by-zero description"
    (stdout.contains "\"description\":\"division by zero")

def testToyDapExceptionBreakpointsToggleWithinSession : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setExceptionBreakpoints" <| Json.mkObj
          [ ("filters", Json.arr #[toJson "runtime"]) ],
        encodeDapRequest 3 "launch" <| exceptionLaunchArgs true,
        encodeDapRequest 4 "next",
        encodeDapRequest 5 "next",
        encodeDapRequest 6 "next",
        encodeDapRequest 7 "setExceptionBreakpoints" <| Json.mkObj
          [ ("filters", Json.arr #[]) ],
        encodeDapRequest 8 "next",
        encodeDapRequest 9 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.exception.toggle" stdinPayload
  assertTrue "exception stop appears while breakpoints are enabled"
    (stdout.contains "\"request_seq\":6" &&
      stdout.contains "\"reason\":\"exception\"")
  assertTrue "disable exception breakpoints response present"
    (stdout.contains "\"request_seq\":7" &&
      stdout.contains "\"command\":\"setExceptionBreakpoints\"" &&
      stdout.contains "\"enabled\":false")
  assertTrue "same failing step errors after disabling exception breakpoints"
    (stdout.contains "\"request_seq\":8" &&
      stdout.contains "\"command\":\"next\"" &&
      stdout.contains "\"success\":false" &&
      stdout.contains "Debug operation failed: division by zero")

def testToyDapExceptionStopKeepsFailureLocationAfterContinue : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setExceptionBreakpoints" <| Json.mkObj
          [ ("filters", Json.arr #[toJson "runtime"]) ],
        encodeDapRequest 3 "launch" <| failingLaunchArgs true,
        encodeDapRequest 4 "continue",
        encodeDapRequest 5 "stackTrace",
        encodeDapRequest 6 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.exception.location.continue" stdinPayload
  assertTrue "continue response present in exception-location flow"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"continue\"")
  assertTrue "continue flow emits exception stop"
    (stdout.contains "\"reason\":\"exception\"")
  assertTrue "stackTrace response present in exception-location flow"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"stackTrace\"")
  assertTrue "stackTrace top frame is scaleAndShift at failing division statement"
    (stdout.contains "\"name\":\"scaleAndShift: let mayfail := div scaled uh\"")
  assertTrue "callee frame appears before caller frame in stackTrace payload"
    (appearsBefore stdout "\"name\":\"scaleAndShift:" "\"name\":\"main:")
def testToyDapLaunchWithEntryPointRejected : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| Json.mkObj
          [ ("entryPoint", toJson "mainProgram"),
            ("stopOnEntry", toJson true) ],
        encodeDapRequest 3 "stepIn",
        encodeDapRequest 4 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.launch.entrypoint.rejected" stdinPayload
  assertTrue "entryPoint launch request gets an error response"
    (stdout.contains "\"request_seq\":2" &&
      stdout.contains "launch requires 'programInfo'")
  assertTrue "stepIn after rejected launch reports missing active session"
    (stdout.contains "\"request_seq\":3" &&
      stdout.contains "No default DAP session. Launch first or pass arguments.sessionId.")

def testToyDapLaunchTerminatesOrder : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs false,
        encodeDapRequest 3 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.launch.terminates" stdinPayload
  assertTrue "initialized event emitted before launch response"
    (appearsBefore stdout "\"event\":\"initialized\"" "\"request_seq\":2")
  assertTrue "launch response emitted"
    (stdout.contains "\"request_seq\":2" && stdout.contains "\"command\":\"launch\"")
  assertTrue "terminated event emitted after launch response"
    (appearsBefore stdout "\"request_seq\":2" "\"event\":\"terminated\"")
  assertEq "no stopped event when launch runs to completion"
    (stdout.contains "\"event\":\"stopped\"")
    false

def testToyDapDisconnectCanTargetSessionId : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "launch" <| launchArgs true,
        encodeDapRequest 3 "launch" <| launchArgs true,
        encodeDapRequest 4 "disconnect" <| Json.mkObj [("sessionId", toJson (1 : Nat))],
        encodeDapRequest 5 "next",
        encodeDapRequest 6 "next" <| Json.mkObj [("sessionId", toJson (1 : Nat))],
        encodeDapRequest 7 "disconnect" ]
  let stdout ← runToyDapPayload "toydap.disconnect.sessionid" stdinPayload
  assertTrue "targeted disconnect response present"
    (stdout.contains "\"request_seq\":4" && stdout.contains "\"command\":\"disconnect\"")
  assertTrue "default session still runs after disconnecting another session"
    (stdout.contains "\"request_seq\":5" && stdout.contains "\"command\":\"next\"")
  assertTrue "disconnected session rejects further requests"
    (stdout.contains "\"request_seq\":6" &&
      stdout.contains "Unknown DAP session id: 1")

def runTransportTests : IO Unit := do
  testToyDapProtocolSanity
  testToyDapLaunchFromProgramInfoRef
  testToyDapLaunchFromProgramInfoRefRejectsUnknownModule
  testToyDapBreakpointProtocol
  testToyDapContinueEventOrder
  testToyDapStepInOutProtocol
  testToyDapScopesExposeHeap
  testToyDapSetVariableHeap
  testToyDapNextStepsOverCall
  testToyDapNextCanStopAtCalleeBreakpoint
  testToyDapEvaluateAndSetVariable
  testToyDapEvaluateAndSetVariableAcrossFrames
  testToyDapEvaluateAndSetVariableNegativePaths
  testToyDapExceptionBreakpointsAndInfo
  testToyDapExceptionBreakpointsToggleWithinSession
  testToyDapExceptionStopKeepsFailureLocationAfterContinue
  testToyDapLaunchWithEntryPointRejected
  testToyDapLaunchTerminatesOrder
  testToyDapDisconnectCanTargetSessionId

end ImpLab.Tests
