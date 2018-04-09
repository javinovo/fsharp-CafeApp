
module DSL

// As all the unit tests for state transitions follow the "Given, When and Then" 
// let's define a DSL to make it easier to write unit tests.

open FsUnit.Xunit
open States
open CommandHandlers

let Given (state : State) = state

let When command state = (command, state)

let ThenStateShouldBe expectedState (command, state) =
  let (actualState, events) = evolve state command
  actualState |> should equal expectedState
  events

let WithEvents expectedEvents actualEvents =
  actualEvents |> should equal expectedEvents