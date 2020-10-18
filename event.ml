type event_name = string 
type event_id = string 

type event = {
  name : event_name;
  id : event_id;
  description: string;
  effects : (string * int) list 
  (* example of [effects] list: [("exp", 10)] ==> gain 10 exp *)
}

let name e = 
  e.name 

let get_id e = 
  e.id

let description e = 
  e.description