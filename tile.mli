(** 
   Representation of data in a tile
   Stores the id, color, and events associated with it
*)

(** Type that represents the tile's color on a hypothetical gameboard GUI.
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
  | Item of string 
  | Energy of int 

(** Abstract representation of values representing tiles. 
    Tiles have an identifier, color, name, description, and list of effects *)
type tile = {
  id : tile_id;
  color : color;
  event_name : string; 
  description: string; 
  effects: effect list; 
}

(** [create_tile id color event_name desc effects] creates a tile of 
    color [color] with id [id], description [desc], associated event 
    [event_name], and corresponding effects [effects] *)
val create_tile : tile_id -> string -> string -> string -> string list -> tile

(** [get_tile_id t] is the tile id of tile [t] *)
val get_tile_id : tile -> tile_id

(** [get_tile_color t] is the color of tile [t] *)
val get_tile_color : tile -> color 

(** [get_tile_event_name t] is the name of the event that happens upon landing
    on the tile [t] *)
val get_tile_event_name : tile -> string

(** [get_tile_description t] is the description of the event occuring on tile 
    [t] *)
val get_tile_description : tile -> string

(** [get_tile_effects t] is the list of effects that landing on tile [t] will 
    apply to a player. 
    Lists how much the points of each component will change. *)
val get_tile_effects : tile -> effect list

(** [get_effect_desc eff] is the description of effect [eff] *)
val get_effect_desc : effect -> string

(** [get_effects eff] is the total number of points accumulated by 
    effect [eff] *)
val get_effects : string -> effect