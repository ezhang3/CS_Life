open Tile 

(** Represents the game board which consists of tiles *)
type 'a t

(** [create x] is a board of [x] tiles*)
val create : int -> 'a t

(** [next_tile t] is the tile after tile [t] on the board. *)
val next_tile : Tile.tile -> Tile.tile

(** [to_tile x] is the tile at location x*)
val to_tile : int -> 'a
(** [next_tile tile] is the list of adjacent tiles to [tile]*)
val next_tile : 'a -> ('a -> 'a -> bool)-> 'a list
