(** The abstract type of values representing player *)
type player

(** The type of player names. *)
type player_name

(** [get name p] is the name of player [p]. *)
val get_name: player -> player_name

(** [get_points p] is the points player [p] has.*)
val get_points: player -> int 

(** [get_partners p] are the study partners, if any, player [p] has. 
    If there are none, is an empty list. *)
val get_partners: player -> player list