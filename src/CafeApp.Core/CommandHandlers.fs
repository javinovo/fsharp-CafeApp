module CommandHandlers

open States
open Events
open Commands
open Errors
open Chessie.ErrorHandling

let handleOpenTab tab = function
  | ClosedTab _ -> [TabOpened tab] |> ok
  | _ -> TabAlreadyOpened |> fail

let execute state command =
  match command with
  | OpenTab tab -> handleOpenTab tab state
  | _ -> failwith "Todo"

let evolve state command =
  match execute state command with
  | Ok (events, _) ->
    let newState = List.fold States.apply state events
    (newState, events) |> ok
  | Bad err -> Bad err