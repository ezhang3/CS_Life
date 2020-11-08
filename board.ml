open Tile
open Yojson.Basic.Util

(** board type is implemented as an adjacency list *)
type gameboard = (Tile.tile * (Tile.tile list)) list

exception No_Tile of string

(* 
(** TODO: helper for generation tiles and path randomly *)
(* let rand_paths = failwith "not_found" *)

let event =
  create_event "Career Fair"
    "10"
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff."
    (Points [("Gain", 10)])
*)


let tile = Tile.create_tile 
  "Tile1" 
  Red "Career Fair" 
  "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff." 
  (Points [("Gain", 10)])

let tile2 = Tile.create_tile 
  "Tile2" 
  Blue "Career Fair" 
  "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff." 
  (Points [("Gain", 10)])

let test_board = [(tile,[tile2]);(tile2,[tile])]

(* how to create boards:
   function to
   Generate tiles from a list of different tiles with events
   - automatically gives them a numerical string id by order of generation

   seems like json might be easiest to use
   List.map applies function to all elements and builds list
   start_tile json =
   json |> member "start_tile" |> to_string

   (* makes a tile with given attributes, with id = x *)
   tile_from_json j = 
   name = json |> member "name" |> to_string
   id = //make incremented id
   color = let color = json |> member "color" |> to_string in
   match color with | "red" -> Red | "blue" -> Blue | "green" -> Green
   | "yellow" -> Yellow
   description = json |> member "description" |> to_string
   effects = //make effect from string

    (* makes all the tiles in each stage*)
   stage j =
   j |> List.map tile_from_json;


   (* find stages and iterates over each stage*)
   stages input = 
   parse json |> member "stages" |> List.map func_stage


     input: {
       start_tile = {
         name:
         id:
         color:
         description:
         effects:
         next: by id
       }
       stages = {
         {
          [({
            name:
            id:
            color:
            description:
            effects:
            next:
          }, occurences);
          {
          }
          ]
         }
       }
     }]]]]]]]]]]]]]]]]]]]]]
     thinking we should add tile neighbors to the tile module
     Generate board using those tiles *)

let create_board x = 
  test_board
(*
   match x with
| 0 -> acc
| x -> create (x-1) (tile :: acc)
*)

let start_tile (board : gameboard) = 
  match board with
  | [] -> raise (No_Tile "Board has no start tile")
  | h :: t -> fst h

let rec find_tile (tile : Tile.tile) func (board : gameboard) =
  match board with
  | [] -> raise (No_Tile "No such tile exists in the given board")
  | (a, b) :: t -> if func a tile then b else find_tile tile func t

(** [next_tile tile func board] searches through the board to find the
    tile that matches tile and gives a list of adjacent tiles. *)
(* TODO: Think about how to optimize because search is O(n) *)
let next_tile = find_tile

let compare_tiles_id tile1 tile2 = if get_tile_id tile1 = get_tile_id tile2
  then true else false
