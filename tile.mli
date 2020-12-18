(** 
   Representation of data in a tile
   Stores the id, color, and events associated with it
*)

(** Abstract representation of values representing tiles. *)
(* type tile  *)
(** Type that represents the tile's color on the gameboard GUI
    [Red] : lose points
    [Green] : gain points
    [Blue] : special event
    [Yellow] : Must stop. End of stage
    [Black] : Default color. 
*)
type color = Red | Blue | Green | Yellow | Black
(** Type of tile identifiers. Make specific tile easy to find on board. *)
type tile_id = string
(** Type of effects *)
type effect = 
  | None
  | Points of (string * int) 
  | Minigame of string 
  | Study_Partner of int 
  | Energy of int 

type tile = {
  id : tile_id;
  color : color;
  event_name : string; 
  description: string; 
  effects: effect list; 
}
(* MIGHT HAVE TO CHANGE SIGNATURES AND SPECS *)

(** [create_tile id color event_name desc effects] creates a tile of 
    color [color] with id [id], description [desc], associated event 
    [event_name], and corresponding effects [effects] *)
val create_tile : tile_id -> string -> string -> string -> string list -> tile
(** [get_tile_id t] is the [tile_id] of tile [t] *)
val get_tile_id : tile -> tile_id
(** [tile_color t] is the color of tile [t] *)
val get_tile_color : tile -> color 
(** [tile_event t] is the name of the event that happens upon landing on the 
    tile [t] *)
val get_tile_event_name : tile -> string
(** [description t] is the description of the event occuring on tile [t] *)
val get_tile_description : tile -> string
(** [get_effects t] is the list of effects that landing on tile [t] will 
    cause a person. 
    Lists how much the points of each component will change. *)
val get_tile_effects : tile -> effect list
(** [get_effect_desc eff] is the description of effect [eff] *)
val get_effect_desc : effect -> string
(** [get_effect_points eff] is the total number of points accumulated by 
    effect eff*)
val get_effects : string -> effect