open Graphics
open Tile
open Playerstate
open Board

let test_board (rand : bool) = Board.create_board 
    (Yojson.Basic.from_file "gameboard1.json") rand

let start_tile rand = test_board rand |> Board.start_tile

let divide () = print_endline "\n********************************************************\n"

type job = Jeff_Bezos | Google | Microsoft | Apple | Facebook | Intel | Tesla 
         | StartUP | Non_Tech | Web_Dev | Generic | IT 
         | Unknown | Unemployed | Married | Overseas

let rec next () iterations = 
  print_endline "Type \"next\" to continue";
  print_string "> ";
  if read_line() |> String.trim |> String.lowercase_ascii = "next" || 
     iterations > 2
  then () 
  else next () (iterations + 1)

(**[instructions ()] prints the instructions for the game *)
let instructions () = 
  divide ();
  print_endline "HOW TO PLAY\n\n";
  print_endline "GAMEPLAY:\n
  First, select the number of players who are playing the game. Then, type the 
  names of each player in order. Careful! The order you type the names is the 
  order the each player will go in during the game.\n";
  next () 0;
  print_endline "ON YOUR TURN:\n
  When it is your turn, follow the instructions given and type \"roll\" to move 
  forward. You will move forward automatically and end on a new tile that will 
  trigger some sort of event that will play out.\n";
  next() 0;
  print_endline "\nTo finish your turn, type \"done\". At the start of your 
  turn, you may end the game by typing \"quit\" or \"instructions\" to bring up 
  the instructions again.";
  next () 0;
  print_endline "\nYou also have an energy bar. Some events will use up energy, 
  so if you do not have enough energy, you may be unable to participate in some 
  events and gain points that way. If you notice your energy is getting low, 
  type \"skip\" at the beginning of your turn to earn back energy at the cost 
  of one turn. Keep track of your energy! If you do not have enough energy for 
  an exam, you may not do as well!\n";
  next () 0;
  print_endline "GETTING STARTED:\n
  On your first turn, decide whether you would like to start with CS 1110 or 
  CS 2110. Taking CS 1110 will allow you to choose better projects later on, but 
  will deduct 100 points as it will take longer for you to graduate. If you 
  start with CS 2110, you are unable to take CS 1110 and will progress through 
  the board faster as a result. No point deductions will occur if you start with 
  CS 2110 but you will be unable to obtain certain projects.\n";
  next () 0;
  print_endline "CHOOSING YOUR PROJECT:\n
  You will receive the option to choose a project right before you start 
  CS 2110. You will be given three random projects with set salaries, and you 
  will be given the option to choose which one you would like. If you have not 
  taken CS 1110, you may not choose a project that includes \"CS 1110 REQUIRED\" 
  in the project description. Each project will have a designated salary with 
  some more than others. Each time you pass a \"pay day\" tile (which you may or 
  may not actually stop for), you will earn a number of points equivalent to 
  your salary.\n";
  next() 0;
  print_endline "CHANGING YOUR PROJECT:\n
  Later in the game, you will have the chance to change your project. You will 
  either be given the option to change your project, be let go from your project 
  and forced to choose a new one, or you or another player may swap projects 
  during a special event.\n";
  next() 0;
  print_endline "TILES:\n
  Each tile will have certain colors to them. 
  YELLOW tiles force you to stop at them no matter what number you rolled. These 
  tiles include exams, choosing path tiles, graduation, choosing projects, etc. 
  RED tiles trigger events that cause you to lose points. 
  GREEN tiles trigger events that cause you to gain points. GREEN tiles include 
  PAYDAY tiles that will give you points depending on your salary. 
  BLUE tiles trigger special events that may cause you to gain or lose points 
  depending on the event. 
  BLACK tiles do not contain events.\n";
  next() 0;
  print_endline "GRADUATION:\n
  Once you reach the graduation tile, you have reached the end of the board and 
  cannot roll anymore. The game ends when everyone has graduated. The winner is 
  then decided by totaling the number of points of each player. Each player's 
  study partners will also be converted to a certain number of points. So make 
  sure to collect study partners along your journey! The placements of each 
  player will be decided by the total number of points each player has once they 
  have all graduated. Depending on where you stand in the rankings, you will 
  receive a different occupation post-undergrad! 
  \n\nGood luck!\n";
  divide ()

(** [roll player n] prompts the player to:
    'roll' -> a number that the player moves forward
    'skip' -> returns 0 so the player skips their turn
    'instructions' -> prints the instructions then prompts user again
    'quit' -> quit the game
*)
let rec roll n = 
  Random.self_init ();
  let custom_roll = 0 in
  print_endline "\n\nEnter:\n
  | \"roll\" to roll the dice
  | \"skip\" to skip your turn and earn back 50 energy
  | \"instructions\" to print the instructions again 
  | \"quit\" to end the game: \n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | "roll" -> 
    if custom_roll = 0 then (Random.int (n - 1)) + 1
    else custom_roll
  | "instructions" -> instructions (); roll n
  | "skip" -> 
    print_endline "\nYou have skipped your turn! Gained 50 energy.\n"; 0
  | "quit" -> exit 0
  | _ -> print_endline "\nInvalid Input. Please try again.\n"; roll n

(**[finish_player_round ()] prompts the player to end their turn *)
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
  print_endline ("\n" ^ name ^ "'s current stats: ");
  Playerstate.print_state player

(**[print_effect effect] prints the effect description *)
let print_effect effect = 
  print_endline ("\n" ^ Tile.get_effect_desc effect ^ "\n")

(** [play_event player players tile_effect] plays out each effect in 
    [tile_effect] for [player]. Depending on the effect, other players in 
    [players] can be affected as well *)
let play_event board player players (tile_effect: Tile.effect list)=
  let tile = get_current_tile player  in
  print_endline (Tile.get_tile_event_name tile ^ "!\n" ^ 
                 Tile.get_tile_description tile);
  let rec helper player players (tile_effect) = 
    match tile_effect with 
    | [] -> ()
    | None :: t -> () (* hope this works *)
    | Points ("Gained", n) as e :: t -> begin
        set_points player n; 
        print_effect e; 
        helper player players t
      end
    | Points ("Lost", n) as e:: t -> begin
        n * (-1) |> set_points player; 
        print_effect e; 
        helper player players t
      end
    | Points (_, _) :: t -> failwith "Invalid Points (_, _)"
    | Energy n as e :: t -> begin 
        chg_energy player n;
        print_effect e;
        helper player players t
      end
    | Item i as e :: t -> begin 
        add_items player i; 
        print_effect e;
        helper player players t; 
      end 
    | Study_Partner n as e :: t -> begin
        add_study_partners player n; 
        print_effect e; 
        helper player players t
      end
    | Minigame s as e :: t -> begin
        print_effect e;
        Specialevents.find_special_event player players board s
      end in
  helper player players tile_effect

(** [play_game players board] starts the game with players [players] and 
    board [board]. *)
(* TODO: Implement energy bar here to make it skip your turn if your 
   energy level is low. *)
let  play_round players board =
  let all_players = players in
  let rec helper players_lst board = 
    match players_lst with 
    | [] -> ()
    | p :: t -> begin
        if compare_tiles_id (Playerstate.get_current_tile p) 
            (Board.end_tile board) then helper t board 
        else begin
          let name = Playerstate.get_name p in
          divide ();
          print_endline ("\nIt is " ^ name ^ "'s turn: "); (*check energy here*)
          print_player_stats p;
          (**Roll dice *)
          let r = roll 6 in
          if r != 0 then 
            begin
              print_endline ("\n" ^ name ^ " rolled a " ^ string_of_int r ^ "\n");
              divide ();
              (**Go to new tile and play event *)
              Playerstate.go p board r; 
              Playerstate.get_current_tile p 
              |> Tile.get_tile_effects 
              |> play_event board p all_players;
              print_player_stats p;
              finish_player_round ();
              helper t board
            end
          else begin finish_player_round (); helper t board end
        end
      end in 
  helper players board

(** [finished_game board players] is true if every player's current_tile 
    in players is board's end_tile. False otherwise.*)
let rec finished_game board = function 
  | [] -> false 
  | h :: t -> begin
      if compare_tiles_id (Playerstate.get_current_tile h) 
          (Board.end_tile board) then 
        finished_game board t 
      else true
    end

(**[point_compare p1 p2] is [1] if [p1] has more points than [p2]. [0] if they 
   have the same points. [-1] if [pi] has less points than [p2]. *)
let point_compare p1 p2 = 
  let num_p1_items = Playerstate.get_items p1 |> List.length in 
  let num_p2_items = Playerstate.get_items p2 |> List.length in 
  let p1_points = Playerstate.get_points p1 
                  |> ( + ) (num_p1_items * 10) in 
  let p2_points = Playerstate.get_points p2
                  |> ( + ) (num_p2_items * 10) in
  if p1_points < p2_points then 1 
  else if p1_points > p2_points then -1 
  else 0

(**[sort_points players] is players sorted in decreasing order by the number 
   of points they have*)
let sort_points players = 
  List.sort point_compare players

(** [find_winner players] is the player with the most points in players. 
    Does not account for ties yet*)
let find_winner players = 
  let rec helper max = function
    | [] -> max 
    | p :: t -> 
      if Playerstate.get_points p > Playerstate.get_points max then 
        helper p t 
      else helper max t in 
  helper (List.hd players) players

(**[max_points players] is the maximum number of points out of all the players 
   in [players]*)
let max_points players = 
  let rec helper max = function
    | [] -> max 
    | p :: t -> 
      let points = Playerstate.get_points p in
      if points > max then 
        helper points t 
      else helper max t in 
  helper 0 players

(**[min_points players] is the minimum number of points out of all the players 
   in [players]*)
let min_points players = 
  let rec helper min = function
    | [] -> min 
    | p :: t -> 
      let points = Playerstate.get_points p in
      if points < min then 
        helper points t 
      else helper min t in 
  helper max_int players

(**[random_best_job p] is a random best job *)
let random_best_job p = 
  let n = roll 6 in
  match n with 
  | 1 -> Google 
  | 2 -> Microsoft 
  | 3 -> Apple
  | 4 -> Facebook 
  | 5 -> Intel 
  | 6 -> Tesla 
  | _ -> failwith "not reached"

(**[random_medium_job p] is a random medium job *)
let random_medium_job p = 
  Random.self_init ();
  let n = roll 12 in
  match n with 
  | 1 -> StartUP
  | 2 -> Non_Tech
  | 3 -> Web_Dev
  | 4 -> Generic
  | 5 -> IT
  | 6 -> Google 
  | 7 -> Microsoft 
  | 8 -> Apple
  | 9 -> Facebook 
  | 10 -> Intel 
  | 11 -> Tesla 
  | 12 -> Overseas
  | _ -> failwith "not reached"

(**[random_bad_job p] is a random bad job *)
let random_bad_job p = 
  Random.self_init ();
  let n = roll 3 in
  match n with 
  | 1 -> Unknown
  | 2 -> Unemployed
  | 3 -> Married
  | _ -> failwith "not reached"

(**[decide_jobs players max min] assigns all players in [players] a job 
   depending on their standing*)
let decide_jobs players max min = 
  let rec helper lst acc = 
    match lst with 
    | [] -> List.rev acc 
    | p :: t -> 
      begin
        match Playerstate.get_points p with 
        | n when n = max -> 
          let job = Jeff_Bezos in 
          (p, job) :: acc |> helper t
        | n when n = min -> 
          let job = random_bad_job p in 
          (p, job) :: acc |> helper t
        | _ -> 
          let job = random_medium_job p in 
          (p, job) :: acc |> helper t
      end in 
  helper players []

(**[print_job_desc player job] prints the description of [player]'s job *)
let print_job_desc player job = 
  let name = Playerstate.get_name player in 
  match job with 
  | Jeff_Bezos -> 
    print_endline 
      ("Literally Jeff Bezos:\n" ^  
       name ^ " is too extrordinarily talented and intelligent to work for 
       another company. " ^ "So, instead of applying for a job like fellow 
       classmates, " ^ name ^ " decides to start their own humble tech company 
       in Silicon Valley. In just two years, this company would rise above that 
       of Amazon and Google. Above all its competitors. " ^ name ^ " will enter 
       Forbes' list of wealthiest people alive in just 5 years after the company 
       was created. What a feat. What an individual\n")
  | Google -> ()
  | Microsoft -> ()
  | Apple -> ()
  | Facebook -> ()
  | Intel -> ()
  | Tesla -> ()
  | StartUP -> ()
  | Non_Tech -> ()
  | Web_Dev -> ()
  | Generic -> ()
  | IT -> ()
  | Overseas -> 
    print_endline 
      ("Overseas:\n" ^
       name ^ " is somewhere overseas (was it Germany? Sweden? Japan?) Last 
       time anyone heard, " ^ name ^ " is chilling in an ok swe position, 
       enjoying the expat life and better health care.")
  | Unknown -> 
    print_endline 
      ("Unknown:\n" ^
       name ^ " could not be found after graduation. Ever since " ^ name ^ 
       " failed to find a job after graduation, nobody knows where they went. No 
       job, no location. Nobody could get in contact with them. It's almost as 
       if they just disappeared...\n")
  | Unemployed -> 
    print_endline 
      ("Unemployed:\n" ^
       name ^ " unfortunately could not find a job after graduation. And could 
       not find a job for the rest of their life. The End.")
  | Married -> 
    print_endline 
      ("Get Married!:\n" 
       ^ name ^ " unfortunately fails to find a job after graduation. However, " 
       ^ name ^ " instead decided to get married! They were lucky enough to 
       have found their \"meant to be\" through their undergrad. The couple 
       live happily ever after!\n")

(**[print_jobs [(p1, job1);...;(pn, jobn)]] prints the jobs [job1,...jobn] of 
   players [p1,...pn]*)
let rec print_jobs = function 
  | [] -> ()
  | (p, job) :: t -> 
    let name = Playerstate.get_name p in 
    print_endline (name ^ ":\n");
    print_job_desc p job;
    next () 0;
    divide ();
    print_jobs t

(**[play_game players board] plays the game.
   Requires:
   [players] is a valid list of players (in order)
   [board] is a valid gameboard *)
let play_game players board = 
  while finished_game board players do 
    play_round players board
  done;
  let winner = find_winner players in 
  let sorted_players = sort_points players in 
  let min = min_points sorted_players in 
  let max = max_points sorted_players in
  let assign_jobs = decide_jobs sorted_players max min in 
  print_endline ("\n\nCongratulations to " ^ (Playerstate.get_name winner) 
                 ^ " for winning with the most points!\n\n");
  next () 0;
  print_endline "Everyone has graduated! Here are the final results: \n\n";
  next () 0;
  print_jobs assign_jobs;
  divide ();
  print_endline "\nThanks for playing!"

(**[check_valid_num num] is the string of int [num] if [num] is an integer.*)
let check_valid_num num = 
  try int_of_string num with 
  | _ -> print_endline "\nInvalid number. Please try again.\n\n"; 
    exit 0

let rec prompt_randomize () = 
  print_endline "Would you like to randomize the board?\n[Y/N]";
  print_string ">";
  match read_line () |> String.trim |> String.lowercase_ascii with 
  | "y" -> true 
  | "n" -> false 
  | _ -> print_endline "\nInvalid input. Please re-enter:\n";
    prompt_randomize ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Game of Life (Cornell CS Version).\n");
  instructions ();
  print_endline "Please enter the number of players between 1 and 6:\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | num -> begin 
      match check_valid_num num with 
      | n when n <= 6 && n > 0 ->
        let rand = prompt_randomize () in
        let players = start_tile rand |> Playerstate.make_player_list n in
        test_board rand |> play_game players
      | _ -> print_endline "\nInvalid number. Please try again.\n\n"; 
        exit 0
    end 

(* Execute the game engine. *)
let () = main ()
