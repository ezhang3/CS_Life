open Tile 

(** Represents the game board which consists of tiles *)
type gameboard

(** An exception give if the tile doesn't appear in board *)
exception No_Tile of string
(** [create_board x] is a board to be implemented with a json.
    Currently uses an integer as a placeholder value*)
val create_board : int -> gameboard

(** [start_tile board] is the first tile of the board.
    Raises: No_Tile if [board] has no starting tile *)
val start_tile : gameboard -> tile

(** [next_tile tile func board] is the list of adjacent tiles to [tile]
    [func tile1 tile2] is a function used to compare tiles.
    Raises: No_Tile if [tile] is not in [board] *)
(* TODO: think about rep invariant and abstraction function*)
val next_tile : tile -> (tile-> tile -> bool) -> gameboard -> tile list

val compare_tiles_id : tile -> tile -> bool
