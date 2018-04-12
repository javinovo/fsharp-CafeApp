module Domain

open System

type Tab = {
  Id : Guid
  TableNumber : int
}

type Item = {
  MenuNumber : int
  Price : decimal
  Name : string
}

type Food = Food of Item

type Drink = Drink of Item

type Payment = {
  Tab : Tab
  Amount : decimal
}

type Order = {
  Foods : Food list
  Drinks : Drink list
  Tab : Tab
}

type InProgressOrder = {
  PlacedOrder : Order
  ServedDrinks : Drink list
  ServedFoods : Food list
  PreparedFoods : Food list
}

let isServingDrinkCompletesOrder order drink =
  List.isEmpty order.Foods && order.Drinks = [drink]

let orderAmount order =
  let foodAmount =
    order.Foods
    |> List.sumBy (fun (Food f) -> f.Price)
  let drinksAmount =
    order.Drinks
    |> List.sumBy (fun (Drink d) -> d.Price)
  foodAmount + drinksAmount

let nonServedFoods ipo =
  List.except ipo.ServedFoods ipo.PlacedOrder.Foods
let nonServedDrinks ipo =
  List.except ipo.ServedDrinks ipo.PlacedOrder.Drinks
let isServingDrinkCompletesIPOrder ipo drink =
  List.isEmpty (nonServedFoods ipo)
  && (nonServedDrinks ipo) = [drink]
let isServingFoodCompletesIPOrder ipo food =
  List.isEmpty (nonServedDrinks ipo) && (nonServedFoods ipo) = [food]  