
module DSL

// As all the unit tests for state transitions follow the "Given, When and Then" 
// let's define a DSL to make it easier to write unit tests.

open FsUnit.Xunit
open Xunit
open States
open CommandHandlers
open Chessie.ErrorHandling
open Errors

let private fail msg =
  Assert.True(false, msg)

let Given (state : State) = state

let When command state = (command, state)

let ThenStateShouldBe expectedState (command, state) =
  match evolve state command with
  | Ok((actualState,events),_) ->
      actualState |> should equal expectedState
      events |> Some
  | Bad errs ->
      sprintf "Expected : %A, But Actual : %A" expectedState errs.Head
      |> fail
      None

let WithEvents expectedEvents actualEvents =
  match actualEvents with
  | Some (actualEvents) ->
    actualEvents |> should equal expectedEvents
  | None -> None |> should equal expectedEvents

let ShouldFailWith (expectedError : Error) (command, state) =
  match evolve state command with
  | Bad errs -> errs.Head |> should equal expectedError
  | Ok(r,_) ->
      sprintf "Expected : %A, But Actual : %A" expectedError r 
      |> fail
      