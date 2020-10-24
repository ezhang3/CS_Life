open Tile 

(** Represents the game board which consists of tiles *)
type gameboard

exception No_Tile of string

(** [create_board x] is a board with [x] tiles*)
val create_board : int -> gameboard

(** [next_tile cur next game] determines if *)
(*val valid_move : Tile.tile -> Tile.tile -> gameboard -> bool*)

(** [to_tile x] is the tile at location x*)
(*val to_tile : int -> 'a*)

(** [start_tile board] is the first tile of the board. *)
val start_tile : gameboard -> Tile.tile

(** [next_tile tile func board] is the list of adjacent tiles to [tile]
    [func tile1 tile2] is a function used to compare tiles *)
(* TODO: think about rep invariant and abstraction function*)
val next_tile : Tile.tile -> (Tile.tile-> Tile.tile -> bool)-> 
  gameboard -> Tile.tile list
