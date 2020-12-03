
let rec find_player name = function 
  | [] -> None
  | p :: t -> 
    if Playerstate.get_name p |> String.lowercase_ascii |> String.trim = name 
    then Some p 
    else find_player name t

let rec academic_integrity player players =
  if List.length players < 2 then 
    print_endline "Sorry, there is no one to accuse of academic integrity?\n"
  else 
    print_endline "Who would you like to accuse of academic integrity?\n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | p -> 
    match find_player p players with 
    | None -> 
      print_endline "\nNot a valid player. Please re-enter: \n\n"; 
      academic_integrity player players 
    | Some p2 -> 
      Playerstate.set_points p2 (-1000);
      Playerstate.set_points player 1000

let rec minigame_choose_1110_2110 player board = 
  print_endline "\nDecide whether you would like to take CS 1110 or CS 2110 first.\nKeep in mind that once you take CS 2110, you cannot go back to take CS 1110. \nEnter [1110] or [2110]:\n";
  print_string  "> ";
  match read_line () |> String.trim with 
  | "1110" -> 
    let new_tile = Board.find_tile_by_id "1110 waiting spot" board in
    Playerstate.set_current_tile player new_tile
  | "2110" -> 
    let new_tile = Board.find_tile_by_id "2110 waiting spot" board in
    Playerstate.set_current_tile player new_tile
  | _ -> print_endline "\nInvalid input. Please re-enter: \n\n";
    minigame_choose_1110_2110 player board

let minigame_1110 player = 
  print_endline "\nUnimplemented \n\n"

let minigame_2110 player = 
  print_endline "How many loopy questions are there? \n";
  print_string  "> ";
  if (read_line () |> String.trim) = "4" 
  then begin
    print_endline "Good job! Gain 10 points";
    Playerstate.set_points player 10
  end
  else 
    print_endline "Wrong answer :( Lose 10 points";
  Playerstate.set_points player ~-10


let minigame_2800 player = 
  print_endline "Do you like regular expressions? Hope you do ;)\n";
  print_endline "What string does the regular expressions a match to? \n"; 
  print_string "> "; 
  if (read_line () |> String.trim) = "a" 
  then begin 
    print_endline "Correct! Gain 5 points \n";
    Playerstate.set_points player 5 
  end
  else 
    print_endline "Nope! The answer is a. Lose 5 points >_< \n";
  Playerstate.set_points player ~-5; 
  print_endline "Another question, on functions: \n"; 
  print_endline "True or false, one-to-one functions are injective \n"; 
  print_string "> "; 
  if (read_line () |> String.trim) = "true" || (read_line () |> String.trim) = "True"
  then begin 
    print_endline "Correct! Gain 5 points \n";
    Playerstate.set_points player 5 
  end
  else 
    print_endline "Nope! The answer is true. Lose 5 points >_< \n";
  Playerstate.set_points player ~-5; 
  print_endline "Thanks for playing! Hope you liked 2800 ^^"

let minigame_3110 player = 
  print_endline "Let's see how well you do on this 3110 quiz! The more you answer correctly, the more points you will gain.\n"

(* Putting assembly instructions in the right order? *)
let minigame_3410 player = 
  print_endline "\nUnimplemented \n\n"

(* *)
let minigame_4410 player = 
  print_endline "\nUnimplemented \n\n"

let minigame_4820 player = 
  print_endline "\nUnimplemented \n\n"

(* I'm intending to make this one extremely annoying to simulate what it feels
   like to debug stuff. Multiple spaces will have this. *)
let rec minigame_debug_v1 player num = 
  if num = 5 then begin
    print_endline "Give up :(";
    Playerstate.set_points player (~-50); end
  else begin
    print_endline "To debug, correctly guess a number between 1 and 5.\n";
    print_endline ("Attempt #"^ string_of_int num ^"\nType your number below");
    print_string "> ";
    let correct = string_of_int (Random.int 5 + 1) in 
    if (read_line () |> String.trim = correct) 
    then 
      print_endline "You did it!"
    else begin
      print_endline "\n Wrong answer. Lose 5 points.\nTry again."; 
      Playerstate.set_points player (-5);
      minigame_debug_v1 player (num+1)
    end
  end 

let minigame_ta player = 
  print_endline "You are hosting office hours for the class. \n"; 
  print_endline "There are a lot of students waiting, waiting for you guidance in hopefully passing this class. \n"; 
  print_endline "An hour passed where you answered lots of questions and corrected so much not great code your head hurts. \n";
  print_endline "So, was it a good experience? (Yes or No) \n"; 
  print_string "> "; 
  if (read_line () |> String.trim) = "yes" || (read_line () |> String.trim) = "Yes"
  then begin
    print_endline "Nice! Glad you found being a TA rewarding :) Maybe be one again next semester?";
    Playerstate.set_points player 20
  end
  else 
    print_endline "Oops, sorry it wasn't that great for you. Is the pay worth it? (Probably yes)";
  Playerstate.set_points player ~-5

let choose_project player = 
  print_endline "Name your project: \n";
  print_string "> ";
  let proj_name = read_line() |> String.trim in 
  let cur_sal = Playerstate.get_salary player in 
  let proj = Some (proj_name,cur_sal+2) in 
  Playerstate.set_project player proj

let change_project player = 
  print_endline "\nUnimplemented \n\n"

let lose_project player = 
  print_endline "\nUnimplemented \n\n"

let birthday (player : Playerstate.player) players = 
  let rec helper (player : Playerstate.player) players (acc : int) = 
    match players with 
    | [] -> Playerstate.set_points player acc 
    | p :: t -> 
      if Playerstate.get_name player = Playerstate.get_name p then 
        helper player t acc 
      else begin
        Playerstate.set_points p (-25);
        helper player t (acc + 25) 
      end in
  helper player players 0

let pay_day player = 
  print_endline "\nIt's pay day!\n";
  match Playerstate.get_project player with 
  | None -> print_endline "\nSorry, you don't have a project right now\n"
  | Some (name, salary) -> print_endline ("Thanks to all of your contributions 
   to " ^ name ^ ", you have earned " ^ (string_of_int salary) ^ " points!\n");
    Playerstate.set_points player salary

let pay_raise player = 
  print_endline "\nIt's pay day! Even better, you get a 10 point pay raise!\n";
  match Playerstate.get_project player with 
  | None -> print_endline "\nSorry, you don't have a project right now\n"
  | Some (name, salary) -> 
    Some(name, salary + 10) 
    |> Playerstate.set_project player;
    print_endline ("Thanks to all of your contributions to " ^ name ^ ", you 
    have earned " ^ (string_of_int salary) ^ " points!\n");
    Playerstate.set_points player salary

let find_special_event player players board str = 
  match str with 
  |"choose_1110_2110" -> minigame_choose_1110_2110 player board
  | "1110" -> minigame_1110 player 
  | "2110" -> minigame_2110 player 
  | "2800" -> minigame_2800 player
  | "3110" -> minigame_3110 player
  | "3410" -> minigame_3410 player
  | "4410" -> minigame_4410 player
  | "4820" -> minigame_4820 player
  | "debug1" -> minigame_debug_v1 player 1
  | "ta" -> minigame_ta player
  | "choose_project" -> choose_project player
  | "change_project" -> change_project player
  | "lose project" -> lose_project player 
  | "birthday" -> birthday player players
  | "pay_day" -> pay_day player 
  | "pay_raise" -> pay_raise player
  | "academic_integrity" -> academic_integrity player players
  | _ -> failwith "special event not found"
