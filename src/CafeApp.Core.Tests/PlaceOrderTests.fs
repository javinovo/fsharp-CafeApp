module PlaceOrderTests

open Xunit
open DSL
open Domain
open States
open Commands
open Events
open Errors
open TestData

[<Fact>]
let ``Can place only drinks order`` () =
  let order = {order with Drinks = [coke]}
  Given (OpenedTab tab)
  |> When (PlaceOrder order)
  |> ThenStateShouldBe (PlacedOrder order)
  |> WithEvents [OrderPlaced order]

[<Fact>]
let ``Can not place empty order`` () =
  Given (OpenedTab tab)
  |> When (PlaceOrder order)
  |> ShouldFailWith CanNotPlaceEmptyOrder

[<Fact>]
let ``Can not place order with a closed tab`` () =
  let order = {order with Drinks = [coke]}
  Given (ClosedTab None)
  |> When (PlaceOrder order)
  |> ShouldFailWith CanNotOrderWithClosedTab

[<Fact>]
let ``Can not place order multiple times`` () =
  let order = {order with Drinks = [coke]}
  Given (PlacedOrder order)
  |> When (PlaceOrder order)
  |> ShouldFailWith OrderAlreadyPlaced  