module ServeFoodTests

open Domain
open States
open Commands
open Events
open DSL
open Xunit
open TestData
open Errors

[<Fact>]
let ``Can maintain the order in progress state by serving food`` () =
  let order = {order with Foods = [salad;pizza]}
  let orderInProgress = {
    PlacedOrder = order
    ServedFoods = []
    ServedDrinks = []
    PreparedFoods = [salad;pizza]
  }
  let expected = {orderInProgress with ServedFoods = [salad]}

  Given (OrderInProgress orderInProgress)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ThenStateShouldBe (OrderInProgress expected)
  |> WithEvents [FoodServed (salad, order.Tab.Id)]

[<Fact>]
let ``Can serve only prepared food`` () =
  let order = {order with Foods = [salad;pizza]}
  let orderInProgress = {
    PlacedOrder = order
    ServedFoods = []
    ServedDrinks = []
    PreparedFoods = [salad]
  }

  Given (OrderInProgress orderInProgress)
  |> When (ServeFood (pizza, order.Tab.Id))
  |> ShouldFailWith (CanNotServeNonPreparedFood pizza)

[<Fact>]
let ``Can not serve non-ordered food`` () =
  let order = {order with Foods = [salad;]}
  let orderInProgress = {
    PlacedOrder = order
    ServedFoods = []
    ServedDrinks = []
    PreparedFoods = [salad]
  }

  Given (OrderInProgress orderInProgress)
  |> When (ServeFood (pizza, order.Tab.Id))
  |> ShouldFailWith (CanNotServeNonOrderedFood pizza)

[<Fact>]
let ``Can not serve already served food`` () =
  let order = {order with Foods = [salad;pizza]}
  let orderInProgress = {
    PlacedOrder = order
    ServedFoods = [salad]
    ServedDrinks = []
    PreparedFoods = [pizza]
  }

  Given (OrderInProgress orderInProgress)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ShouldFailWith (CanNotServeAlreadyServedFood salad)

[<Fact>]
let ``Can not serve for placed order`` () =
  Given (PlacedOrder order)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ShouldFailWith (CanNotServeNonPreparedFood salad)

[<Fact>]
let ``Can not serve for non placed order`` () =
  Given (OpenedTab tab)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ShouldFailWith CanNotServeForNonPlacedOrder

[<Fact>]
let ``Can not serve for already served order`` () =
  Given (ServedOrder order)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ShouldFailWith OrderAlreadyServed

[<Fact>]
let ``Can not serve with closed tab`` () =
  Given (ClosedTab None)
  |> When (ServeFood (salad, order.Tab.Id))
  |> ShouldFailWith CanNotServeWithClosedTab  
