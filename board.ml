(** board type is implemented as an adjacency list *)
type gameboard = (Tile.tile * (Tile.tile list)) list

let create x = failwith "unimplemented"

(** [next_tile tile] is the list of adjacent tiles to [tile] *)
(* should build something to match if length 1, go to the tile, otherwise
   prompt user choice and then go to tile*)
let next_tile tile func board = failwith "notfound"

(** [find_tile tile board] finds the tile with id [tile] in board [board] that 
    matches and gives the list of adjacent tiles ahead of it *)
let rec find_tile tile board =
  match board with
  | [] -> failwith "notfound"
  | h :: t -> failwith "notfound"

(** TODO: helper for generation tiles and path randomly *)
let rand_paths = failwith "notfound"
