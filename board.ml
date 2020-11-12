open Tile
open Yojson.Basic.Util

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
    ["gain 10"]

(** gamenode represents a node of the gameboard *)
type gamenode = Tile.tile * (Tile.tile list)

(** gameboard represents a gameboard parsed from a json. It is an acyclic
    directed graph. Implemented as an adjacency list containing
    tiles and neighboring tiles. *)
type gameboard = gamenode list

let test_board = [(tile,[tile2]);(tile2,[tile])]

(* ------------------- FUNCS FOR CREATING GAME BOARD -------------------- *)

(* Main gist of building board:
   build indiv tile -> tile list -> randomization -> tile list list
   -> flatten -> assign pointers -> gameboard*)

(** gets a string member from a json and converts to string *)
let get_mem json str =
  json |> member str

(** [Assign_next_tiles lst] replaces each tile in [lst] with a tuple
    of the tile and a list with the following tiles in the list.
    Tile.tile - > (Tile.tile * (Tile.tile list))
    Following tile list is empty list if there is no following tile in [lst]*)
let assign_next_tiles lst =
  let rec helper lst acc = 
    match lst with
    | [] -> raise (No_Tile "List of tiles empty")
    | h :: [] -> List.rev ((h, []) :: acc)
    | f :: s :: t -> helper t ((f, [s]) :: acc)
  in helper lst []

(** gets the last element of a list *)
let rec last_of_list = function
  | [] -> raise (No_Tile "List empty. No last element exists")
  | _ :: x :: [] -> x
  | h :: t -> last_of_list t

(** gets the first element of a list. Raises error if list is empty *)
let first_of_list = function
  | [] -> raise (No_Tile "List empty. No first element exists") | h :: t -> h

(** [build_tile json] creates an individual tile as specified by
    json information *)
let build_tile json = 
  let id = get_mem json "id" |> to_string in
  let color = get_mem json "color" |> to_string in
  let event_name = get_mem json "event" |> to_string in
  let description = get_mem json "description" |> to_string in
  let effects = get_mem json "effects" |> to_list |> List.map to_string in
  create_tile id color event_name description effects

(** [build_tiles json] builds list of tiles and randomizes *)
(* TODO: Randomization function after build_tile*)
let build_stage json =
  get_mem json "tiles" |> to_list |> List.map build_tile (* |> randomize *)

(** [build_stages json] builds a list of randomized stages, flattens them,
    and assigns pointers *)
(* TODO: Implement branching paths *)
let build_stages json =
  get_mem json "stages"|> to_list |> List.map build_stage |> List.flatten
  |> assign_next_tiles

(** [from_json json] parses a valid json into game_board*)
let from_json json =
  try build_stages json
  with Type_error (s, _) -> failwith ("Failed to build board from json: " ^ s)

(* ------------------------- INTERFACE FUNCTIONS -------------------------*)

let create_board json = from_json json
(* test_board *)

let start_tile board = match board with
  | [] -> raise (No_Tile "Board has no start tile")
  | h :: t -> fst h 

let end_tile (board : gameboard) = failwith "unimplemented"

let rec find_tile (tile : Tile.tile) func board =
  match board with
  | [] -> raise (No_Tile "No such tile exists in the given board")
  | (a, b) :: t -> if func a tile then b else find_tile tile func t

(** [next_tile tile func board] searches through the board to find the
    tile that matches tile and gives a list of adjacent tiles. *)
(* TODO: Think about how to optimize because search is O(n) *)
let next_tile = find_tile

let end_tile board = match last_of_list board with | (a, b) -> a

let compare_tiles_id tile1 tile2 = get_tile_id tile1 = get_tile_id tile2
