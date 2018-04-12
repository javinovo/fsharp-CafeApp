module CommandHandlers

open Domain
open States
open Events
open Commands
open Errors
open Chessie.ErrorHandling

(* Command handlers *)

// OpenTab

let private handleOpenTab tab = function
  | ClosedTab _ -> [TabOpened tab] |> ok
  | _ -> TabAlreadyOpened |> fail

// PlaceOrder

let private handlePlaceOrder (order : Order) = function
  | OpenedTab _ -> 
      match List.isEmpty order.Foods && List.isEmpty order.Drinks with
      | true -> fail CanNotPlaceEmptyOrder
      | false -> [OrderPlaced order] |> ok
  | ClosedTab _ -> fail CanNotOrderWithClosedTab
  | _ -> fail OrderAlreadyPlaced

// ServeDrink

let (|NonOrderedDrink|_|) order drink =
  match List.contains drink order.Drinks with
  | false -> Some drink
  | true -> None

let (|ServeDrinkCompletesOrder|_|) order drink =
  match isServingDrinkCompletesOrder order drink with
  | true -> Some drink
  | false -> None

let (|AlreadyServedDrink|_|) ipo drink =
  match List.contains drink ipo.ServedDrinks with
  | true -> Some drink
  | false -> None  
  
let (|ServeDrinkCompletesIPOrder|_|) ipo drink =
  match isServingDrinkCompletesIPOrder ipo drink with
  | true -> Some drink
  | false -> None

let private handleServeDrink drink tabId = function
| PlacedOrder order ->
  let event = DrinkServed (drink,tabId)
  match drink with
  | NonOrderedDrink order _ ->
    CanNotServeNonOrderedDrink drink |> fail
  | ServeDrinkCompletesOrder order _ ->
    let payment = {Tab = order.Tab; Amount = orderAmount order}
    event :: [OrderServed (order, payment)] |> ok
  | _ -> [event] |> ok
| ServedOrder _ -> OrderAlreadyServed |> fail
| OpenedTab _ ->  CanNotServeForNonPlacedOrder |> fail
| ClosedTab _ -> CanNotServeWithClosedTab |> fail
| OrderInProgress ipo ->
  let order = ipo.PlacedOrder
  let drinkServed = DrinkServed (drink, order.Tab.Id)
  match drink with
  | NonOrderedDrink order _ ->
    CanNotServeNonOrderedDrink drink |> fail
  | AlreadyServedDrink ipo _ ->
    CanNotServeAlreadyServedDrink drink |> fail
  | ServeDrinkCompletesIPOrder ipo _ ->
    let payment = {Tab = order.Tab; Amount = orderAmount order}
    drinkServed :: [OrderServed (order, payment)] |> ok
  | _ -> [drinkServed] |> ok

// PrepareFood

let (|NonOrderedFood|_|) order food =
  match List.contains food order.Foods with
  | false -> Some food
  | true -> None

let (|AlreadyPreparedFood|_|) ipo food =
  match List.contains food ipo.PreparedFoods with
  | true -> Some food
  | false -> None

let private handlePrepareFood food tabId = function
| PlacedOrder order ->
  match food with
  | NonOrderedFood order _ -> CanNotPrepareNonOrderedFood food |> fail
  | _ -> [FoodPrepared (food, tabId)] |> ok
| ServedOrder _ -> OrderAlreadyServed |> fail
| OpenedTab _ ->  CanNotPrepareForNonPlacedOrder |> fail
| ClosedTab _ -> CanNotPrepareWithClosedTab |> fail
| OrderInProgress ipo ->
  let order = ipo.PlacedOrder
  match food with
  | NonOrderedFood order _ -> CanNotPrepareNonOrderedFood food |> fail
  | AlreadyPreparedFood ipo _ -> CanNotPrepareAlreadyPreparedFood food |> fail
  | _ -> [FoodPrepared (food, tabId)] |> ok

// ServeFood

let (|UnPreparedFood|_|) ipo food =
  match List.contains food ipo.PreparedFoods with
  | false -> Some food
  | true -> None

let (|AlreadyServedFood|_|) ipo food =
  match List.contains food ipo.ServedFoods with
  | true -> Some food
  | false -> None

let (|ServeFoodCompletesIPOrder|_|) ipo food =
  match isServingFoodCompletesIPOrder ipo food with
  | true -> Some food
  | false -> None
  
let private handleServeFood food tabId = function
| OrderInProgress ipo ->
  let order = ipo.PlacedOrder
  let foodServed = FoodServed (food, tabId)
  match food with
  | NonOrderedFood order _ ->
    CanNotServeNonOrderedFood food |> fail
  | AlreadyServedFood ipo _ ->
    CanNotServeAlreadyServedFood food |> fail
  | UnPreparedFood ipo _ ->
    CanNotServeNonPreparedFood food |> fail
  | ServeFoodCompletesIPOrder ipo _ ->
    let payment = {Tab = order.Tab; Amount = orderAmount order}
    foodServed :: [OrderServed (ipo.PlacedOrder, payment)] |> ok
  | _ -> [foodServed] |> ok
| PlacedOrder _ -> CanNotServeNonPreparedFood food |> fail
| ServedOrder _ -> OrderAlreadyServed |> fail
| OpenedTab _ -> CanNotServeForNonPlacedOrder |> fail
| ClosedTab _ -> CanNotServeWithClosedTab |> fail

// CloseTab

let private handleCloseTab payment = function
| ServedOrder order ->
  let orderAmount = orderAmount order
  if payment.Amount = orderAmount then
    [TabClosed payment] |> ok
  else
    InvalidPayment (orderAmount, payment.Amount) |> fail
| _ -> CanNotPayForNonServedOrder |> fail


// (* Handling logic: evolve *)

let private execute state command =
  match command with
  | OpenTab tab -> handleOpenTab tab state
  | PlaceOrder order -> handlePlaceOrder order state
  | ServeDrink (drink, tabId) -> handleServeDrink drink tabId state
  | PrepareFood (food, tabId) -> handlePrepareFood food tabId state
  | ServeFood (food, tabId) -> handleServeFood food tabId state
  | CloseTab payment -> handleCloseTab payment state

let evolve state command =
  match execute state command with
  | Ok (events, _) ->
    let newState = List.fold States.apply state events
    (newState, events) |> ok
  | Bad err -> Bad err