open Yojson.Basic.Util

type color = Red | Blue | Green | Yellow | Black

type tile_id = string

type effect = Points of (string * int) | Minigame of string (*see if we can write this so that
                                                              it can activate a mini-game*)

type tile = {
  id : tile_id;
  color : color;
  event_name : string; 
  description: string; 
  effects: effect list; 
}

(** [get_color s] is the color of the same name as s. Case insensitive. *)
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

(*needs to handle multiple effects in a tile *)
let get_effects str =
  if str = "" then failwith "invalid effect for get_effects" else
    match parse_effect (String.lowercase_ascii str) with 
    | "gain" :: t :: [] -> Points ("Gained", int_of_string t)
    | "lose" :: t :: [] -> Points ("Lost", int_of_string t)
    | "minigame" :: t :: [] -> failwith "get_effects: minigame not implemented"
    | _ -> failwith "invalid effect for get_effects"

let create_tile id color event_name description effects= 
  {id = id; color = get_color color; event_name = event_name; 
   description = description; effects = List.map get_effects effects}

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
  | [] -> failwith "get_effect_desc empty effects" 
  | Points (s,_) :: t -> s 
  | Minigame s :: t -> s (* Note: minigames not implemented yet *)
  (*
  | Points [] -> failwith "empty effect"
  | Points ((s, _) :: t) -> s *)

let rec add_points lst acc = 
  match lst with
  | [] -> acc
  | (_, pts) :: t -> acc + pts |> add_points t 

(* If gain, return positive points
   If lose, return negative points
   If minigame, find minigame in special events(to be implemented)*)
let get_effect_points tile = failwith "get_effect_points not done yet"
(*match tile.effects with 
  | Points lst -> add_points lst 0 *)

(* take string, output a function to apply to points, ie for losing,
   gaining, multiplying, etc points*)