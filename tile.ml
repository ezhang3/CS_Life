open Yojson.Basic.Util

type color = Red | Blue | Green | Yellow | Black

type tile_id = string

type effect = 
  | None 
  | Points of (string * int) 
  | Minigame of string 
  | Study_Partner of int 

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

let get_effects str = 
  match parse_effect (String.lowercase_ascii str) with 
  | "" :: [] -> None
  | "gain" :: t :: [] -> Points ("Gained", int_of_string t)
  | "lose" :: t :: [] -> Points ("Lost", int_of_string t)
  | "study_partner" :: t :: [] -> Study_Partner (int_of_string t)
  | "minigame" :: t :: [] -> Minigame t
  | _ -> failwith "invalid effect for get_effects"

let create_tile id color event_name description effects = 
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

let get_effect_desc effect = 
  match effect with 
  | None -> ""
  | Points (s,n) -> s ^ " " ^ (string_of_int n) ^ " points\n" 
  | Study_Partner i -> "Gained 1 study partner!\n" 
  | Minigame s -> "Special Event!\n" 