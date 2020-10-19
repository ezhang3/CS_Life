<<<<<<< HEAD
type player_name = string
type points = int
type project
type study_partners

type state 

val get_name : state -> string
val set_points : state -> Event.event -> unit
val get_points : state -> points

val add_study_partners : state -> int -> unit
val get_study_partners : state -> study_partners

val set_project : state -> project -> unit
val get_project : state -> project 

val set_current_tile : state -> Tile.tile -> unit
val get_current_tile : state -> Tile.tile

val get_visited_tiles : state -> Tile.tile list
=======
(** 
   Representation of the changing player state. 

   This module represents the player state as the game goes on, including 
   location, any items owned, and reputation level(?).
*)

(** The abstract type representing the player state. *)
type st

(** The initial state of the player at the start of game. *)
val init_state : Player.player -> st
>>>>>>> f4c3fb1a070fbdb670ca87f40ee16de6a86893ef
