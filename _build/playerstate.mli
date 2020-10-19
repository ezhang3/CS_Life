(** 
   Representation of the changing player state. 

   This module represents the player state as the game goes on, including 
   location, any items owned, and reputation level(?).
*)

(** The abstract type representing the player state. *)
type st

(** The initial state of the player at the start of game. *)
val init_state : Player.player -> st