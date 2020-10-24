open Graphics
open Event
open Tile
open Playerstate
open Board

let event =
  create_event "Career Fair"
    "10"
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff."
    (Points [("Gain", 10)])

(* should ids be strings or numbers?*)
let tile = Tile.create_tile Red event "Career Fair Red"
let tile2 = Tile.create_tile Blue event "Career Fair Blue"

let test_board = [(tile,[tile2]);(tile2,[tile])]
let start_tile board =
  match board with 
  | [] -> failwith "empty"
  | (tile, _) :: t -> tile

(** [play_game f] starts the adventure in file [f]. *)
let play_game players board =
  open_graph ""

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Game of Life (Cornell CS Version).\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | num -> 
    let players = Playerstate.make_player_list (int_of_string num) 
        (start_tile test_board) in
    play_game players test_board

(* Execute the game engine. *)
let () = main ()
