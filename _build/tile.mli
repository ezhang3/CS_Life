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
val create : color ->  Event.event -> tile_id -> tile

(** [tile_color t] is the color of the tile *)
val get_tile_color : tile -> color 

(** [tile_event t] is the event that happens upon landing on the tile [t] *)
val get_tile_event : tile -> Event.event

(** [get_tile_id t] is the [tile_id] of tile [t] *)
val get_tile_id : tile -> tile_id
