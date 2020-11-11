open Tile
open Yojson.Basic.Util

(** board type is implemented as an adjacency list *)
(* type gameboard = (Tile.tile * (Tile.tile list)) list  *)
(* it's possibile we might have to change up the implementation of board *)

exception No_Tile of string

let tile = Tile.create_tile 
    "Tile1" 
    "Red" "Career Fair" 
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff." 
    ["gain 10"]

let tile2 = Tile.create_tile 
    "Tile2" 
    "Blue" "Career Fair" 
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff." 
    (*["Gain"]*)
    ["gain 10"]


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
type stage = {
  tiles : (Tile.tile * (Tile.tile list)) list
}
type gameboard = {
  stages : stage (* list *)
}

let test_board = {stages = {tiles = [(tile,[tile2]);(tile2,[tile])]}}
(* id : tile_id;
   color : color;
   event_name : string; 
   description: string; 
   effects: effect; *)
let get_mem json str =
  json |> member str

(* replaces each tile of lst with a tuple of the tile and a list with the
   following tile in the list (empty list if no following tile)*)
let assign_next_tiles lst =
  let rec helper lst acc = 
    match lst with
    | [] -> failwith "Impossible"
    | h :: [] -> List.rev ((h, []) :: acc)
    | f :: s :: t -> helper t ((f, [s]) :: acc)
  in helper lst []

let build_tile json = 
  let id = get_mem json "id" |> to_string in
  let color = get_mem json "color" |> to_string in
  let event_name = get_mem json "event" |> to_string in
  let description = get_mem json "description" |> to_string in
  let effects = get_mem json "effects" |> to_list |> List.map to_string in
  create_tile id color event_name description effects

let build_tiles json = 
  json |> to_list |> List.map build_tile |> assign_next_tiles
let build_stage json = json |> member "tiles" |> build_tiles

let from_json json =
  try build_stage json
  with Type_error (s, _) -> failwith ("Failed to build board from json: " ^ s)

let create_board (x : int) = 
  test_board
(*
   match x with
| 0 -> acc
| x -> create (x-1) (tile :: acc)
*)

(* board is a flattened list *)
let start_tile (board : gameboard) = failwith "thinking of new implementation"
(* match board with
   | [] -> raise (No_Tile "Board has no start tile")
   | h :: t -> fst h *)

let rec find_tile (tile : Tile.tile) func (board : gameboard) =
  failwith "thinking of new implementation"
  (*
  match board with
  | [] -> raise (No_Tile "No such tile exists in the given board")
  | (a, b) :: t -> if func a tile then b else find_tile tile func t *)

(** [next_tile tile func board] searches through the board to find the
    tile that matches tile and gives a list of adjacent tiles. *)
(* TODO: Think about how to optimize because search is O(n) *)
let next_tile = (* find_tile *) failwith "thinking of new implementation"

let compare_tiles_id tile1 tile2 = get_tile_id tile1 = get_tile_id tile2
