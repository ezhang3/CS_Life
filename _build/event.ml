type event_name = string 
type event_id = string 
type effect = Points of (string * int) list 
type event = {
  name : event_name;
  id : event_id;
  description: string;
  effects : effect
  (* example of [effects] list: [("exp", 10)] ==> gain 10 exp *)
}

let get_name e = 
  e.name 

let get_id e = 
  e.id

let get_description e = 
  e.description

let get_effects e = 
  e.effects
