open Tile 

(** Represents the game board which consists of tiles *)
type gameboard

exception No_Tile 

(** [create x] is a board with [x] tiles*)
val create : int -> gameboard

(** [next_tile cur next game] determines if *)
(*val valid_move : Tile.tile -> Tile.tile -> gameboard -> bool*)

(** [to_tile x] is the tile at location x*)
(*val to_tile : int -> 'a*)

(** [next_tile tile] is the list of the tiles in its path [tile]*)
val next_tile : Tile.tile -> (Tile.tile-> Tile.tile -> bool)-> 
  gameboard -> Tile.tile list
