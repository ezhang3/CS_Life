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

(* Random compare function to use with List.sort *)
let rand_comp x y = let randn = (Random.int 3) in 
  (* print_endline (string_of_int randn);  *)
  randn - 1

(* [randomize lst] is a randomized version of [lst] *)
(* TODO: Add a field that determines whether randomized or not? *)
let randomize rand lst = 
  let on = false in
  if not on && not (rand) then lst else List.sort rand_comp lst

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

let build_next_tiles json = 
  get_mem json "id" |> to_string 

type stage_id = string

type stage = {
  id : stage_id;
  next_stages : stage_id list;
  tiles : tile list;
}

(** [build_stage json] builds a stage. Each stage contains a list of tiles that 
    can be randomized *)
let build_stage rand json =
  let id = get_mem json "id" |> to_string in
  let next_stages = get_mem json "next" |> to_list 
                    |> List.map build_next_tiles in
  let tiles = get_mem json "tiles" |> to_list |> List.map build_tile 
              |> randomize rand in 
  {
    id = id;
    next_stages = next_stages;
    tiles = tiles
  }

(** [Assign_next_tiles lst] replaces each tile in [lst] with a tuple
    of the tile and a list with the following tiles in the list.
    Tile.tile - > (Tile.tile * (Tile.tile list))
    Following tile list is empty list if there is no following tile in [lst]*)
(**
   recurse through list of stages
*)
(**[helper tiles acc] *)
let rec helper tiles acc = 
  match tiles with 
  | [] -> [] (*raise (No_Tile "List of tiles empty") *) 
  | f :: s :: t -> helper (s :: t) ((f, [s]) :: acc) 
  | h :: [] -> acc |> List.rev 

(**[get_last_tile lst] gets the last tile from [lst]. *)
let rec get_last_tile = function 
  | [] -> failwith "empty tile list"
  | h :: [] -> h 
  | h :: t -> get_last_tile t 

(**[find_stage id stages] is the stage with the id [id] in the list of stages 
   [stages]. Raises an error if the stage is not found in [stages]. *)
let rec find_stage id = function 
  | [] -> failwith (id ^ " could not be found")
  | h :: t -> if h.id = id then h 
    else find_stage id t

(**[get_next_stages stage lst] is the list of first tiles of each adjacent 
   stage of [stage]*)
let rec get_next_stages stage lst = 
  let next_stages = stage.next_stages in
  let rec helper acc = function 
    | [] -> List.rev acc 
    | h :: t -> 
      let fst_stage = find_stage h lst in
      helper (List.hd fst_stage.tiles :: acc) t in 
  helper [] next_stages

let assign_next_tiles stages =
  let rec recurse_stages stages2 acc = 
    match stages2 with 
    | [] -> acc 
    | h :: t -> 
      let tiles = helper h.tiles [] in 
      let last_tile = (get_last_tile h.tiles, get_next_stages h stages) in
      acc @ tiles @ [last_tile] |> recurse_stages t in
  recurse_stages stages [] 

(** [build_stages json] builds a list of randomized stages, flattens them,
    and assigns pointers *)
(* TODO: Implement branching paths *)
let build_board json rand =
  get_mem json "stages"|> to_list |> List.map (build_stage rand)
  |> assign_next_tiles

(** [from_json json] parses a valid json into game_board*)
let from_json json rand =
  try build_board json rand
  with Type_error (s, _) -> failwith ("Failed to build board from json: " ^ s)

(* ------------------------- INTERFACE FUNCTIONS -------------------------*)

let create_board json rand = from_json json rand
(* test_board *)

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

let rec find_tile_by_id id (board : gameboard) = 
  match board with 
  | [] -> raise (No_Tile "No such tile exists in the given board")
  | (a, b) :: t -> if a.id = id then a 
    else find_tile_by_id id t

let rec end_tile (board : gameboard) = 
  (* match last_of_list board with | (a, b) -> a *)
  match board with 
  | [] -> failwith "emptyboard"
  | (last, _) :: [] -> last 
  | h :: t -> end_tile t

let compare_tiles_id tile1 tile2 = get_tile_id tile1 = get_tile_id tile2
