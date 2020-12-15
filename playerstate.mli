(** 
   Representation of the changing player state. 

   This module represents the player state as the game goes on, including 
   location, any items owned, and reputation level(?).
*) 
type player_name = string
type points = int
type salary = int
type project_name = string
type project = (project_name * salary) option
type study_partners = int

(** The abstract type representing the player state. *)
type player 

(** [init_state name start] is the initial state of a player with name [name] 
    and current_tile [start] at the start of a game *)
val init_state : player_name -> Tile.tile -> player
(** [print_state player] prints out the player's stats *)
val print_state : player -> unit
(** [make_player_list n start] creates a list of n players *)
val make_player_list : int -> Tile.tile -> player list
val get_nth_player : player list -> int -> player
val get_name : player -> string
val set_points : player -> int -> unit

(** [get_points st] is the current reputation points player has in state [st] *)
val get_points : player -> points

val add_study_partners : player -> int -> unit
val get_study_partners : player -> study_partners

val set_project : player -> project -> unit
val get_project : player -> project 
val get_salary : player -> salary
(** [go st board n] moves the player [st] [n] tiles forward. 
    Requires:
    [st] is a valid Playerstate.st
    [board] is a valid Board.gameboard
    [n] is an int
*)
val go : player -> Board.gameboard -> int -> unit

(** [set_current_tile st tile] sets the player's current tile to [tile]
    Requires:
    [st] is a valid Playerstate.st
    [tile] is a valid Tile.tile 
*)
val set_current_tile : player -> Tile.tile -> unit

(** [get_current_tile st] is the tile the player is currently on in state [st] *)
val get_current_tile : player -> Tile.tile

val get_visited_tiles : player -> Tile.tile list

(** [have_items st] is the list of items the player has on hand in state [st]*)
val get_items: player -> string list


