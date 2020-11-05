open Event
open Yojson.Basic.Util

type color = Red | Blue | Green | Yellow | Black

type tile_id = string

type tile = {
  color : color;
  event : event; 
  id : tile_id;
}

let get_color = function
| "Red" -> Red
| "Blue" -> Blue
| "Green" -> Green
| "Yellow" -> Yellow
| _ -> Black

let create_tile color event id = 
  {color = color; event = event; id = id}

let event_placeholder = create_event "placeholder" "" "lalala" (Points [])

(* sample code to extract from JSON. Need to wait until tile and event are combined*)
let tile_of_json json = {
  color = json |> member "color" |> to_string |> get_color;
  event = event_placeholder;
  id = json |> member "id" |> to_string
}

let get_tile_color tile =
  tile.color

let get_tile_event tile = 
  tile.event

let get_tile_id tile = 
  tile.id