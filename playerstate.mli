(** 
   Representation of the changing player state. 

   This module represents the player state as the game goes on, including 
   location, any items owned, and reputation level(?).
*)

(** The abstract type representing the player state. *)
type st

(** [init_state p] is the initial state of player [p] at the start of game *)
val init_state : Player.player -> st

(** [current_tile_id] is the tile the player is currently on in state [st] *)
val current_tile_id: st -> Tile.tile_id

(** [have_items st] is the list of items the player has on hand in state [st]*)
val have_items: st -> string list

(** [rep level st] is the current reputation level player has in state [st] *)
val rep_level: st -> float