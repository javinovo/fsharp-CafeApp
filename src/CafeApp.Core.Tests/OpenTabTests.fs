module OpenTabTests

open Domain
open DSL
open Events
open Commands
open States
open System
open Xunit
open Errors
open TestData

[<Fact>]
let ``Can open a new Tab``() =
  Given (ClosedTab None) // Current State
  |> When (OpenTab tab) // Command
  |> ThenStateShouldBe (OpenedTab tab) // New State
  |> WithEvents [TabOpened tab] // Event Emitted

[<Fact>]
let ``Can not open an already opened tab`` () =
  let tab = {Id = Guid.NewGuid(); TableNumber = 1}

  Given (OpenedTab tab)
  |> When (OpenTab tab)
  |> ShouldFailWith TabAlreadyOpened