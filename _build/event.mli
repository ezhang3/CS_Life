(** 
   Representation of event data

   Stores all the data about an event including name, description, and effects
*)

(** Abstract type of values representing events *)
type event 

(** Type of event names *)
type event_name 

(** Type of event identifiers. To distinguish between events that have the same
    name *)
type event_id
type effect
(** [name e] is the name of event [e] *)
val get_name : event -> event_name

(** [get_id e] is the id of event [e] *)
val get_id : event -> event_id

(** [description e] is the description of event [e] *)
val get_description : event -> string

(** [get_effects e] is the list of effects that event [e] will cause a person. 
    Lists how much the points of each component will change. *)
val get_effects : event -> effect