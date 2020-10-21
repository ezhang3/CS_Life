open Event
open Tile

(** board type is implemented as an adjacency list *)
type gameboard = (Tile.tile * (Tile.tile list)) list

(** TODO: helper for generation tiles and path randomly *)
let rand_paths = failwith "notfound"

let event =
  create_event "Career Fair"
    "10"
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff."
    (Points [("Gain", 10)])

(* should ids be strings or numbers?*)
let tile = Tile.create Red event ""

let test_board = [tile, [tile]]

let rec create x = failwith "notfound"
(*
   match x with
| 0 -> acc
| x -> create (x-1) (tile :: acc
*)

(** [find_tile tile func board] finds the tile with id [tile] in board [board]
    that matches and gives the list of adjacent tiles ahead of it. 
    [func tile1 tile2] is a function used to compare tiles *)
let rec find_tile (tile : Tile.tile) func (board : gameboard) =
  match board with
  | [] -> failwith "notfound"
  | (a, b) :: t -> if func a tile then b else find_tile tile func t

(** [next_tile tile] is the list of adjacent tiles to [tile] *)
let next_tile = find_tile
