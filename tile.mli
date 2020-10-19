open Event
(** 
   Representation of data in a tile
   Stores the id, color, and events associated with it
*)

(** Abstract representation of values representing tiles. *)
type tile
(** Type that represents the tile's color on the gameboard GUI *)
type color 
(** Type of tile identifiers. Make specific tile easy to find on board. *)
type tile_id

(** [tile_color t] is the color of the tile *)
val get_tile_color : tile -> color 

(** [tile_event t] is the event that happens upon landing on the tile [t] *)
val get_tile_event : tile -> Event.event

(** [get_tile_id t] is the [tile_id] of tile [t] *)
val get_tile_id : tile -> tile_id
