/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
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

private def launchArgs (stopOnEntry : Bool) : Json :=
  Json.mkObj
    [ ("programInfo", toJson ImpLab.Lang.Examples.mainProgram),
      ("stopOnEntry", toJson stopOnEntry) ]

private def bumpEntryLine : Nat :=
  ImpLab.Lang.Examples.mainProgram.locationToSourceLine { func := "bump", stmtLine := 1 }

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

def testToyDapBreakpointProtocol : IO Unit := do
  let stdinPayload :=
    String.intercalate ""
      [ encodeDapRequest 1 "initialize",
        encodeDapRequest 2 "setBreakpoints" <| Json.mkObj
          [ ("breakpoints", Json.arr #[Json.mkObj [("line", toJson (22 : Nat))]]) ],
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
  testToyDapBreakpointProtocol
  testToyDapContinueEventOrder
  testToyDapStepInOutProtocol
  testToyDapNextStepsOverCall
  testToyDapNextCanStopAtCalleeBreakpoint
  testToyDapLaunchWithEntryPointRejected
  testToyDapLaunchTerminatesOrder
  testToyDapDisconnectCanTargetSessionId

end ImpLab.Tests
