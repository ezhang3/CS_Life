(** 
   Representation of data in a tile
   Stores the id, color, and events associated with it
*)

(** *)
type tile

type color 
type event
type tile_id

(** [tile_color t] is the color of the tile *)
val get_tile_color : tile -> color 

(** [tile_event t] is the event that happens upon landing on the tile [t] *)
val get_tile_event : tile -> event 

(** [get_tile_id t] is the [tile_id] of tile [t] *)
val get_tile_id : tile -> tile_id
