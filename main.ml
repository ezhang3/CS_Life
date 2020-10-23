open Graphics
open Event
open Tile
open Playerstate
open Board

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Life (Cornell CS Version) Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()