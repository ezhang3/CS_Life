open Tile
open Yojson.Basic.Util

exception No_Tile of string


(** gamenode represents a node of the gameboard *)
type gamenode = Tile.tile * (Tile.tile list)

(** gameboard represents a gameboard parsed from a json. It is an acyclic
    directed graph. Implemented as an adjacency list containing
    tiles and neighboring tiles. *)
type gameboard = gamenode list

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
    | f :: s :: t -> helper (s :: t) ((f, [s]) :: acc)
    | h :: [] -> (h, []) :: acc |> List.rev
  in helper lst []

(* Random compare function to use with List.sort *)
let rand_comp x y = let randn = (Random.int 3) 
  in print_endline (string_of_int randn); randn - 1

(* [randomize lst] is a randomized version of [lst] *)
let randomize lst = 
  let on = true in
  if not on then lst else List.sort rand_comp lst


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
  print_endline "Building tile...";
  print_endline "Getting id...";
  let id = get_mem json "id" |> to_string in
  print_endline "Getting color...";
  let color = get_mem json "color" |> to_string in
  print_endline "Getting event...";
  let event_name = get_mem json "event" |> to_string in
  print_endline "Getting description...";
  let description = get_mem json "description" |> to_string in
  print_endline "Getting effects...";
  let effects = get_mem json "effects" |> to_list |> List.map to_string in
  create_tile id color event_name description effects

(** [build_stage json] builds list of tiles and randomizes *)
(* TODO: Randomization function after build_tile*)
let build_stage json =
  get_mem json "tiles" |> to_list |> List.map build_tile |> randomize

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

(** ---for testing purposes, building a not random board--- *)

(** [build_stage_nr json] builds list of tiles in the order they are found
    in the json *)
let build_stage_nr json =
  get_mem json "tiles" |> to_list |> List.map build_tile

(** [build_stages_nr json] builds a list of stages, flattens them,
    and assigns pointers *)
let build_stages_nr json =
  get_mem json "stages"|> to_list |> List.map build_stage_nr |> List.flatten
  |> assign_next_tiles

(** [from_json_nr json] parses a valid json into game_board*)
let from_json_nr json =
  try build_stages_nr json
  with Type_error (s, _) -> failwith ("Failed to build board from json: " ^ s)

(* ------------------------- INTERFACE FUNCTIONS -------------------------*)

let create_board json = from_json json
(* test_board *)

(* for testing *)
let create_board_nr json = from_json_nr json 

let start_tile (board : gameboard) = match board with
  | [] -> raise (No_Tile "Board has no start tile")
  | h :: _ -> fst h 

let rec find_tiles (tile : Tile.tile) func board =
  match board with
  | [] -> raise (No_Tile "No such tile exists in the given board")
  | (a, b) :: t2 -> 
    if func a tile then b 
    else find_tiles tile func t2

(** [next_tile tile func board] searches through the board to find the
    tile that matches tile and gives a list of adjacent tiles. *)
(* TODO: Think about how to optimize because search is O(n) *)
let next_tiles = find_tiles

let rec end_tile (board : gameboard) = 
  (* match last_of_list board with | (a, b) -> a *)
  match board with 
  | [] -> failwith "emptyboard"
  | (last, _) :: [] -> last 
  | h :: t -> end_tile t

let compare_tiles_id tile1 tile2 = get_tile_id tile1 = get_tile_id tile2
