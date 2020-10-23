open Event
open Tile

(** board type is implemented as an adjacency list *)
type gameboard = (Tile.tile * (Tile.tile list)) list

exception No_Tile
(** TODO: helper for generation tiles and path randomly *)
let rand_paths = failwith "notfound"

let event =
  create_event "Career Fair"
    "10"
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff."
    (Points [("Gain", 10)])

(* should ids be strings or numbers?*)
let tile = Tile.create_tile Red event ""

let test_board = [tile, [tile]]

(* how to create boards:
   function to
   Generate tiles from a list of different tiles with events
   - automatically gives them a numerical string id by order of generation
     -
     tiles = [
      [color, event, neighbors, numoccur];
      [color, event, neighbors, numoccur];
     ]
     thinking we should add tile neighbors to the tile module
     Generate board using those tiles *)

let rec create x = failwith "notfound"
(*
   match x with
| 0 -> acc
| x -> create (x-1) (tile :: acc
*)

let rec find_tile (tile : Tile.tile) func (board : gameboard) =
  match board with
  | [] -> raise No_Tile
  | (a, b) :: t -> if func a tile then b else find_tile tile func t

(** [next_tile tile func board] is the list of adjacent tiles to [tile]
    [func tile1 tile2] is a function used to compare tiles *)
let next_tile = find_tile
