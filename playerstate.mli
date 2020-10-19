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
