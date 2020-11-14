open Tile 
open Yojson.Basic.Util

(** Represents the game board which consists of tiles *)
type gameboard

(** An exception give if the tile doesn't appear in board *)
exception No_Tile of string

(** [create_board x] is a board created from a valid json [x], which
    consists of nonempty stages and tiles, randomized *)
val create_board : Yojson.Basic.t -> gameboard

(** [create_board x] is a board created from a valid json [x], which
    consists of nonempty stages and tiles, not randomized; for testing *)
val create_board_nr : Yojson.Basic.t -> gameboard

(** [start_tile board] is the first tile of the board.
    Raises: No_Tile if [board] has no starting tile *)
val start_tile : gameboard -> tile

(** [end_tile board] is the last tile of the board.
    Raises: No_Tile if [board] has no ending tile *)
val end_tile : gameboard -> tile

(** [next_tile tile func board] is the list of adjacent tiles to [tile]
    [func tile1 tile2] is a function used to compare tiles.
    Raises: No_Tile if [tile] is not in [board] *)
(* TODO: think about rep invariant and abstraction function*)
val next_tiles : tile -> (tile-> tile -> bool) -> gameboard -> tile list

(** [compare_tiles_id t1 t2] is whether [t1] and [t2] have the same id.
    Ids should be unique to a tile. *)
val compare_tiles_id : tile -> tile -> bool
