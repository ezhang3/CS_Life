open Yojson.Basic.Util

type color = Red | Blue | Green | Yellow | Black

type tile_id = string

type effect = Points of (string * int) (*see if we can write this so that
                                              it can activate a mini-game*)

type tile = {
  id : tile_id;
  color : color;
  event_name : string; 
  description: string; 
  effects: effect list; 
}

let get_color str = match String.lowercase_ascii str with
  | "red" -> Red
  | "blue" -> Blue
  | "green" -> Green
  | "yellow" -> Yellow
  | _ -> Black

(** [sep_effects s] separates the effects give in a string. *)
let sep_effects s = String.split_on_char ',' s

let parse_helper lst = 
  let rec helper acc = function
    | [] -> List.rev acc
    | "" :: t -> helper acc t
    | h :: t ->  helper (h :: acc) t
  in helper [] lst

let parse_effect str =
  let split = String.split_on_char ' ' str
  in split |> parse_helper

let get_effects str =
  if str = "" then failwith "invalid" else
    match parse_effect (String.lowercase_ascii str) with 
    | "gain" :: t :: [] -> Points ("Gained", int_of_string t)
    | "lose" :: t :: [] -> Points ("Lost", int_of_string t)
    | "minigame" :: t :: [] -> failwith "not implemeneted yet"
    | _ -> failwith "invalid"

let create_tile id color event_name description effects= 
  {id = id; color = get_color color; event_name = event_name; description = description;
   effects = List.map get_effects effects}

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
  | [] -> failwith "empty effects" 
  | Points (s,_) :: t -> s 
  (*
  | Points [] -> failwith "empty effect"
  | Points ((s, _) :: t) -> s *)

let rec add_points lst acc = 
  match lst with
  | [] -> acc
  | (_, pts) :: t -> acc + pts |> add_points t 

let get_effect_points tile = 
  match tile.effects with 
  | Points lst -> add_points lst 0

(* take string, output a function to apply to points, ie for losing,
   gaining, multiplying, etc points*)