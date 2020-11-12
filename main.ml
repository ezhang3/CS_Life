open Graphics
open Tile
open Playerstate
open Board

let (test_board : Board.gameboard) = Board.create_board 
    (Yojson.Basic.from_file "gameboard1.json")
let start_tile = Board.start_tile test_board


let divide () = print_endline "\n********************************************************\n"


(** [roll player] *)
let rec roll p = 
  print_endline "\n\nType 'roll' to roll the dice: \n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | "roll" -> Random.int 5 + 1
  | _ -> print_endline "\nInvalid Input. Please try again.\n"; roll p

let rec finish_player_round () = 
  print_endline "\n\nType 'done' to finish your turn: \n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | "done" -> ()
  | _ -> print_endline "\nInvalid Input. Please try again.\n"; 
    finish_player_round ()

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
let rec play_round players board =
  match players with 
  | [] -> ()
  | p :: t -> begin
      if compare_tiles_id (Playerstate.get_current_tile p) (Board.end_tile board) then play_round t board 
      else
        let name = Playerstate.get_name p in
        divide ();
        print_endline ("\nIt is " ^ name ^ "'s turn: ");
        print_player_stats p;
        (**Roll dice *)
        let r = roll p in
        print_endline ("\n" ^ name ^ " rolled a " ^ string_of_int r ^ "\n");
        (**Go to new tile and play event *)
        Playerstate.go p board r; 
        play_event p;
        Playerstate.print_state p;
        divide (); divide ();
        finish_player_round ();
        play_round t board
    end


(** [finished_game board players] is true if every player's current_tile 
    in players is board's end_tile. False otherwise.*)
let rec finished_game board = function 
  | [] -> false 
  | h :: t -> begin
      if compare_tiles_id (Playerstate.get_current_tile h) (Board.end_tile board) then 
        finished_game board t 
      else true
    end


(** [find_winner players] is the player with the most in players. 
    Does not account for ties yet*)
let rec find_winner max = function 
  | [] -> max 
  | p :: t -> 
    if Playerstate.get_points p > Playerstate.get_points max then 
      find_winner p t 
    else find_winner max t


let play_game players board = 
  while finished_game board players do 
    play_round players board
  done;
  let winner = find_winner (List.hd players) players in 
  print_endline ("\n\nCongratulations to " ^ (Playerstate.get_name winner) 
                 ^ " for winning with the most points!\n\n Thanks for playing!")


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
