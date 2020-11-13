
let rec find_player name = function 
  | [] -> None
  | p :: t -> 
    if Playerstate.get_name p |> String.lowercase_ascii |> String.trim = name 
    then Some p 
    else find_player name t

let rec academic_integrity player players =
  print_endline "Who would you like to accuse of academic integrity?\n";
  print_endline "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | p -> 
    match find_player p players with 
    | None -> 
      print_endline "\nNot a valid player. Please re-enter: \n\n"; 
      academic_integrity player players 
    | Some p2 -> 
      Playerstate.set_points p2 (-1000);
      Playerstate.set_points player 1000

let minigame_1110 player = failwith "unimplemented"

let minigame_2110 player = failwith "unimplemented"

let minigame_2800 player = failwith "unimplemented"

let minigame_3110 player = 
  print_endline "Let's see how well you do on this 3110 quiz! The more you answer correctly, the more points you will gain.\n"

let minigame_3410 player = failwith "unimplemented"

let minigame_4410 player = failwith "unimplemented"

let minigame_4820 player = failwith "unimplemented"

let choose_project player = failwith "unimplemented"

let change_project player = failwith "unimplemented"

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

let find_special_event player players str = 
  match str with 
  | "1110" -> minigame_1110 player 
  | "2110" -> minigame_2110 player 
  | "2800" -> minigame_2800 player
  | "3110" -> minigame_3110 player
  | "3410" -> minigame_3410 player
  | "4410" -> minigame_4410 player
  | "4820" -> minigame_4820 player
  | "choose_project" -> choose_project player
  | "change_project" -> change_project player
  | "birthday" -> birthday player players
  | "pay_day" -> pay_day player 
  | "pay_raise" -> pay_raise player
  | _ -> failwith "special event not found"
