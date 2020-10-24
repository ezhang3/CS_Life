(** 
   Representation of the changing player state. 

   This module represents the player state as the game goes on, including 
   location, any items owned, and reputation level(?).
*)

type player_name = string
type points = int
type project
type study_partners

(** The abstract type representing the player state. *)
type st 

val init_state : player_name -> Tile.tile -> st
val make_player_list : int -> Tile.tile -> st list
val get_name : st -> string
val set_points : st -> Event.event -> unit

(** [get_points st] is the current reputation points player has in state [st] *)
val get_points : st -> points

val add_study_partners : st -> int -> unit
val get_study_partners : st -> study_partners

val set_project : st -> project -> unit
val get_project : st -> project 

val set_current_tile : st -> Tile.tile -> unit
(** [get_current_tile] is the tile the player is currently on in state [st] *)
val get_current_tile : st -> Tile.tile

val get_visited_tiles : st -> Tile.tile list

(** [init_state p] is the initial state of player [p] at the start of game *)

(** [have_items st] is the list of items the player has on hand in state [st]*)
val get_items: st -> string list


