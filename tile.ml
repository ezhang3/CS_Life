open Event
open Yojson.Basic.Util

type color = Red | Blue | Green | Yellow | Black

type tile_id = string

type effect = Points of (string * int) list 

type tile = {
  id : tile_id;
  color : color;
  event_name : string; 
  description: string; 
  effects: effect; 
  (* example of [effects] list: [("exp", 10)] ==> gain 10 exp *)
}

let get_color = function
  | "Red" -> Red
  | "Blue" -> Blue
  | "Green" -> Green
  | "Yellow" -> Yellow
  | _ -> Black

let create_tile id color event_name description effects= 
  {id = id; color = color; event_name = event_name; description = description;
   effects = effects }

let event_placeholder = create_event "placeholder" "" "lalala" (Points [])

(* sample code to extract from JSON. Need to wait until tile and event are combined*)
(* let tile_of_json json = {
   id = json |> member "id" |> to_string;
   color = json |> member "color" |> to_string |> get_color;
   event_name = event_placeholder; 
   } *)

let get_tile_id tile = 
  tile.id

let get_tile_color tile =
  tile.color

let get_tile_event_name tile = 
  tile.event_name

let get_tile_description tile = 
  tile.description

let get_tile_effects tile = 
  tile.effects

let get_effect_desc tile = 
  match tile.effects with 
  | Points [] -> failwith "empty effect"
  | Points ((s, _) :: t) -> s

let rec add_points lst acc = 
  match lst with
  | [] -> acc
  | (_, pts) :: t -> acc + pts |> add_points t 

let get_effect_points tile = 
  match tile.effects with 
  | Points lst -> add_points lst 0