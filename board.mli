(** Represents the game board which consists of tiles *)
type 'a t

(** [create x] is a board of [x] tiles*)
val create : int -> 'a t

(** [next_tile tile] is the list of adjacent tiles to [tile]*)
val next_tile : 'a -> ('a -> 'a -> bool)-> 'a list