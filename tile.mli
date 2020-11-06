open Event
(** 
   Representation of data in a tile
   Stores the id, color, and events associated with it
*)

(** Abstract representation of values representing tiles. *)
type tile
(** Type that represents the tile's color on the gameboard GUI *)
type color = Red | Blue | Green | Yellow
(** Type of tile identifiers. Make specific tile easy to find on board. *)
type tile_id = string

(** [create color event id] creates a tile of color [color] with id
    [id] and associated event [event]*)
val create_tile : color ->  Event.event -> tile_id -> tile

(** [get_tile_id t] is the [tile_id] of tile [t] *)
val get_tile_id : tile -> tile_id

(** [tile_color t] is the color of the tile *)
val get_tile_color : tile -> color 

(** [tile_event t] is the name of the event that happens upon landing on the 
    tile [t] *)
val get_tile_event_name : tile -> string

(** [description t] is the description of the event occuring on tile [t] *)
val get_description : tile -> string

(** [get_effects t] is the list of effects that landing on tile [t] will 
    cause a person. 
    Lists how much the points of each component will change. *)
val get_tile_effects : tile -> effect

val get_effect_desc : tile -> string
(** [get_effect_points eff] is the total number of points accumulated by 
    effect eff*)
val get_effect_points : tile -> int