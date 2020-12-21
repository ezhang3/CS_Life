(** 
   Representation of the board. 

   This module represents the board during the game.
*) 


open Tile 
open Yojson.Basic.Util

(** Represents the game board which consists of tiles. *)
type gameboard

(** An exception given if the tile doesn't appear in board. *)
exception No_Tile of string

(** [create_board x b] is a board created from a valid json [x], which
    consists of nonempty stages and tiles which will be randomized if
    b is true.
    A valid json consists of a list of stages.
    Stages have an id and a next field (which goes to the next sequence).
    Each stage has a list of tiles which have an id, color, event,
    description, and effect.*)
val create_board : Yojson.Basic.t -> bool -> gameboard

(** [start_tile board] is the first tile of the board.
    Raises: No_Tile if [board] has no starting tile *)
val start_tile : gameboard -> tile

(** [end_tile board] is the last tile of the board.
    Raises: No_Tile if [board] has no ending tile *)
val end_tile : gameboard -> tile

(** [next_tile tile func board] is the list of adjacent tiles to [tile]
    [func tile1 tile2] is a function used to compare tiles.
    Raises: No_Tile if [tile] is not in [board] *)
val next_tiles : tile -> (tile-> tile -> bool) -> gameboard -> tile list

(** [find_tile_by_id id board] is the tile in [board] with id [id]. *)
val find_tile_by_id : Tile.tile_id -> gameboard -> tile 

(** [compare_tiles_id t1 t2] is whether [t1] and [t2] have the same id.
    Ids should be unique to a tile. *)
val compare_tiles_id : tile -> tile -> bool
