open Graphics
open Event
open Tile
open Playerstate
open Board

let (test_board : Board.gameboard) = Board.create_board 0
let start_tile = Board.start_tile test_board

let divide () = print_endline "\n********************************************************\n"

(** [roll player] *)
let rec roll player = 
  print_endline "\n\nType 'roll' to roll the dice: \n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | "roll" -> 1
  | _ -> print_endline "\nInvalid Input. Please try again.\n"; roll player 


(**[print_player_stats player] Prints player [player]'s stats *)
let print_player_stats player = 
  let name = Playerstate.get_name player in 
  print_endline ("\n\n" ^ name ^ "'s current stats: ");
  Playerstate.print_state player

let play_event player = 
  let tile = Playerstate.get_current_tile player  in
  print_endline (Tile.get_tile_event_name tile ^ "!\n" ^ 
                 Tile.get_tile_description tile);
  print_endline ("\n" ^ Tile.get_effect_desc tile ^ " " ^ 
                 (Tile.get_effect_points tile |> string_of_int) ^ " points\n");
  Playerstate.set_points player tile

(** [play_game players board] starts the game with players [players] and 
    board [board]. *)
let play_game players board =
  for i = 0 to (List.length players - 1) do 
    let player = Playerstate.get_nth_player players i in 
    let name = Playerstate.get_name player in
    divide ();
    print_endline ("\nIt is " ^ name ^ "'s turn: ");
    print_player_stats player;
    (**Roll dice *)
    let r = roll player in
    print_endline ("\n" ^ name ^ " rolled a " ^ string_of_int r ^ "\n");
    (**Go to new tile and play event *)
    Playerstate.go player board r; 
    play_event player;
    Playerstate.print_state player;
    divide (); divide ();
  done 


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Game of Life (Cornell CS Version).\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | num -> 
    let players = Playerstate.make_player_list (int_of_string num) start_tile in
    play_game players test_board

(* Execute the game engine. *)
let () = main ()
