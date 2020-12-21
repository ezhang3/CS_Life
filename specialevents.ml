let rec find_player name = function 
  | [] -> None
  | p :: t -> 
    if Playerstate.get_name p |> String.lowercase_ascii |> String.trim = name 
    then Some p 
    else find_player name t

let rec academic_integrity player players =
  if List.length players < 2 then 
    print_endline 
      "Sorry, there is no one to accuse of an academic integrity violation\n"
  else 
    print_endline 
      "Who would you like to accuse of an academic integrity violation?\n";
  print_string  "> ";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | p -> 
    match find_player p players with 
    | None -> 
      print_endline "\nNot a valid player. Please re-enter: \n\n"; 
      academic_integrity player players 
    | Some p2 -> 
      Playerstate.set_points p2 (-100);
      Playerstate.set_points player 100

let rec minigame_choose_1110_2110 player board = 
  print_endline 
    "Decide whether you would like to take CS 1110 or CS 2110 first.\n
  Keep in mind that once you take CS 2110, you cannot go back to take CS 1110.\n
  Enter [1110] or [2110]:\n";
  print_string  "> ";
  match read_line () |> String.trim with 
  | "1110" -> 
    let new_tile = Board.find_tile_by_id "1110 waiting spot" board in
    Playerstate.set_points player ~-100;
    Playerstate.set_current_tile player new_tile
  | "2110" -> 
    let new_tile = Board.find_tile_by_id "2110 waiting spot" board in
    Playerstate.set_current_tile player new_tile
  | _ -> print_endline "\nInvalid input. Please re-enter: \n\n";
    minigame_choose_1110_2110 player board

let rec get_study_buddy player players = 
  if List.length players < 2 then begin
    print_endline "Sorry, there are no other players to study with you\n";
    print_endline "You're on your own.\n"
  end
  else begin 
    let rec helper pl lst = 
      let player_name_list = List.map Playerstate.get_name lst in 
      print_endline "Who do you want to choose as a study buddy?\n
      Type in their name\n";
      print_string 
        "They must be a valid player or type \"no\" if you don't want one";
      print_string "> ";
      let answer = read_line () |> String.trim in 
      if (answer = Playerstate.get_name player) then 
        print_string 
          "You chose to have yourself as a study buddy since you're weird"
      else if List.mem answer player_name_list then begin
        Playerstate.add_study_partners pl 1;
        print_string ("Yay! " ^ answer ^ " is now your study buddy!")
      end
      else begin
        print_string ("Invalid input! "^answer^" is not a CS major.\n");
        print_endline "Try again\n";
        helper pl lst;
      end
    in 
    print_endline "Would you like a study buddy? Enter [yes] or [no]\n";
    match read_line () |> String.trim |> String.lowercase_ascii with 
    | "yes" -> helper player players
    | "no" -> print_endline "You decided you're good on your own\n"
    | _ -> begin
        print_endline "Invalid input! Try again!\n";
        get_study_buddy player players 
      end
  end 

and firstq_1110 input player = 
  if input = "yes" 
  then begin 
    print_endline "Your A7 was probably really great. You even implemented extra 
  features! Excellent ^^\n"; 
    Playerstate.set_points player 20
  end 
  else 
    print_endline "No? That's a shame, since that's all the last assignment is 
  goint to be about \n"

and secondq_1110 input player = 
  if input = "yes" then begin 
    print_endline "Wow, you were a diligent student. Not all can relate. Not at
    all. \n"; 
    print_endline "We'll give you some good student points.\n";
    Playerstate.set_points player 10
  end 
  else begin 
    print_endline "Yeah, it really be like that. Why not just do the lab on your
  own time, in the comfort of your room, instead of being stuck with everyone
  else in the room at that time? \n"; 
    print_endline "Smart, eh?" 
  end

let minigame_1110 player = 
  Playerstate.chg_energy player ~-5;
  print_endline "Do you like old arcade games like Invaders? \n
  Enter [yes] or [no]: \n"; 
  print_string "> "; 
  firstq_1110 (read_line () |> String.trim |> String.lowercase_ascii) player;
  print_endline "Did you actually attend all the discussion sections, or were 
  you the type to just let the ta grade your finished lab, and then rush out the
  moment it was graded?\n"; 
  print_endline "So, yes or no?\n";
  print_string "> "; 
  secondq_1110 (read_line () |> String.trim |> String.lowercase_ascii) player

and loopy_q player score =  
  print_endline "How many loopy questions are there? \n";
  print_string  "> ";
  if (read_line () |> String.trim) = "4" 
  || (read_line () |> String.trim |> String.lowercase_ascii) = "four"
  then begin
    print_endline "Good job! Gain 10 points";
    score := 1; 
    Playerstate.set_points player 10
  end
  else begin
    print_endline "Wrong answer :( Lose 10 points";
    Playerstate.set_points player ~-10; 
  end 

and program_exp player score = 
  print_endline "How many years of programming experience do you have? \n";
  print_endline "Please type in numbers. \n";
  print_string  "> ";
  if int_of_string (read_line () |> String.trim) < 59
  then begin
    print_endline "Too little. You will listen to what the great David Gries has
    to say in the course. \n";
    print_endline "YES, even when you think the four loopy questions are 
    tedious. \n";
    print_endline "Maybe you'll even figure out what the fifth loopy question is
    one day ;) \n"; 
    if !score = 1 then 
      score := 2
    else 
      score := 1
  end
  else begin 
    print_endline "Hmmmm. \n";
    print_endline "There's no way you could have more programming experience 
    than the great David Gries himself. Don't lie \n";
    Playerstate.set_points player ~-10
  end

let minigame_2110 player = 
  Playerstate.chg_energy player ~-10;
  let score = ref 0 in 
  loopy_q player score;
  program_exp player score; 
  if !score = 2 then begin 
    print_endline 
      "Great job! You did so well on the final, here's a gift for you. \n";
    print_endline "You received JavaHyperText! Added to your items\n";
    Playerstate.add_items player "JavaHyperText";
    print_endline "That's it for 2110!\n"
  end 
  else 
    print_endline "That's it for 2110!\n"

and firstq_2800 player = 
  print_endline "What string does the regular expressions a match to? \n"; 
  print_string "> "; 
  if (read_line () |> String.trim) = "a" 
  then begin 
    print_endline "Correct! Gain 5 points \n";
    Playerstate.set_points player 5 
  end
  else begin 
    print_endline "Nope! The answer is a. Lose 5 points >_< \n";
    Playerstate.set_points player ~-5
  end

and secondq_2800 player = 
  print_endline "Another question, on functions: \n"; 
  print_endline "True or false, one-to-one functions are injective \n"; 
  print_string "> "; 
  if (read_line () |> String.trim |> String.lowercase_ascii) = "true" 
  then begin 
    print_endline "Correct! Gain 5 points \n";
    Playerstate.set_points player 5 
  end
  else begin 
    print_endline "Nope! The answer is true. Lose 5 points >_< \n";
    Playerstate.set_points player ~-5
  end

let minigame_2800 player players = 
  print_endline "Do you like regular expressions? Hope you do ;)\n";
  firstq_2800 player;
  print_endline "2800 psets are such a grind >_< Maybe a study buddy can help?";
  get_study_buddy player players;
  secondq_2800 player;
  print_endline "Thanks for playing! Hope you liked 2800 ^^";
  Playerstate.chg_energy player ~-8

let minigame_3110 player = 
  print_endline "Let's see how well you do on this 3110 quiz! The more you 
  answer correctly, the more points you will gain.\n";
  print_endline "1. What does the follwing expression evaluate to?\n";
  print_endline "\"let x = 1 in x + 1";
  match read_line () |> String.trim with 
  | "2" -> print_endline "Correct!";
    print_endline "Thanks for playing! I hope you liked 3110 ^^"
  | _ -> print_endline "Wrong answer.";  
    print_endline "Thanks for playing! I hope you liked 3110 ^^"

and firstq_3410 player score = 
  print_endline "Which programming language is used in CS 3410?\n";
  print_string "> ";
  if (read_line () |> String.trim) = "C" 
  then begin  
    print_endline "C for Correct!\n";
    score := 1;
    Playerstate.set_points player 3
  end
  else begin
    print_endline "Incorrect. Lose 5 points\n";
    Playerstate.set_points player ~-3
  end

let secondq_3410 player score = 
  print_endline "Now you have to do some work with logism and circuits\n";
  print_endline "Name one of the basic logic gates\n";
  print_string "> ";
  let ans = read_line () |> String.trim |> String.lowercase_ascii in 
  let correct_ans = ["and";"or";"nand";"nor";"xor";"xand"] in 
  if List.mem ans correct_ans then begin
    print_endline "Nice! You know some gates\n";
    Playerstate.set_points player 3;
    if !score = 1 then 
      score := 2
    else 
      score := 1
  end
  else begin 
    print_endline "Nope. That's not a gate\n";
    Playerstate.set_points player ~-3
  end

let minigame_3410 player = 
  let score = ref 0 in 
  firstq_3410 player score; 
  secondq_3410 player score;
  if !score = 2 then begin 
    print_endline "Great job! You did well, here's a little momento for you.\n";
    print_endline "You received MIPS processor! Added to your items\n";
    Playerstate.add_items player "MIPS_processor";
    print_endline 
      "Thanks for taking 3410! I hope you've learned about how computers work";
  end 
  else 
    print_endline 
      "Thanks for taking 3410! I hope you've learned about how computers work";
  Playerstate.chg_energy player ~-10

let story_4410 = 
  print_endline "In the beginning, there was hardware. Now the hardware was 
    formless and empty, darkness was over the surface of silicon.\n";
  print_endline "And then the creator said “let there be operating systems,” 
  and there were OSes. The creator saw that OSes were good.\n";
  print_endline "And the creator said “let there be processes, and threads.” 
  OSes were teeming with processes and threads carrying out different tasks.\n";
  print_endline "Then the creator said “let
   the processes and threads synchronize with each other.” For this task, the 
   creator appointed human-kind.\n";
  print_endline "But humans were fallible, and weak, and they failed to get
   synchronization correct, and fallen angel BSOD (pronounced beesod), spawn of
   Beelzebub, ruled the day with great evil.\n"; 
  print_endline "From a handout \"12 Commandments of Synchronization\" by Emin 
  Sirer.\n"

let minigame_4410 player =
  Playerstate.chg_energy player ~-10;
  print_endline "Do you want to hear a story?\n";
  print_endline "Enter [yes] or [no]"; 
  print_string "> "; 
  match read_line () |> String.trim |> String.lowercase_ascii with 
  | "no" -> print_endline "No? Meh, move along then"
  | "yes" -> story_4410;
    print_endline "That was long, so here's a reward: 10 points and a copy of
    the 12 Commandments of Synchronization"; 
    Playerstate.set_points player 10; 
    print_endline 
      "You received 12 Commandments of Synchronization! Added to your items\n";
    Playerstate.add_items player "12Commandments";
  | _ -> print_endline 
           "You typed in something weird in response to a yes or no question\n";
    print_endline "You lose points for typing in something weird";
    Playerstate.set_points player ~-5

let minigame_4820 player = 
  print_endline "How many hours did you spend in OH a week?\n";
  print_string "> "; 
  if int_of_string (read_line () |> String.trim) > 4
  then begin 
    print_endline "Seems like you suffered a lot\n"; 
    print_endline "Going to take away some of your energy anyways so you can be 
  even more dead on the inside :)\n"; 
    Playerstate.chg_energy player ~-5
  end 
  else 
    print_endline "Wow, you did all the work without have to camp OH? Nice\n";
  Playerstate.set_points player 10; 
  print_endline "Time to make homeworks and exams harder next semester. . .\n"


let minigame_4120 player = 
  print_endline "Welcome to compilers.\n";
  print_endline "Be prepared, you won't get a lot of sleep this semester.\n";
  print_endline "If you can get through it without contemplated working at 
  a corn farm, you should be good.";
  print_endline "Some of your energy will be taken away, in anticipation of the 
  effort that lies ahead.\n"; 
  Playerstate.chg_energy player ~-15

let mini_game_networking player = 
  Playerstate.chg_energy player ~-8;
  print_endline "Trying to make professional connections, whether to get to know 
  a job better or for recruiting/referrals?/n";
  print_endline "Well here is the place to try!"; 
  print_endline "Would you like to cold email an alum?\nEnter [yes] or [no].";
  print_string "> "; 
  if (read_line () |> String.trim |> String.lowercase_ascii = "yes") 
  then begin 
    print_endline "You cold emailed/linkedin connected with an alum.\n You had a 
    good talk.\nIn the future, maybe you'll even get a referral from them.\n";
    print_endline "You received referral (?)! Added to your items\n";
    Playerstate.add_items player "referral(?)";
  end 
  else 
    print_endline "No? Good luck on the continuing grind!"

(* I'm intending to make this one extremely annoying to simulate what it feels
   like to debug stuff. Multiple spaces will have this. *)
let rec minigame_debug_v1 player num = 
  if num = 6 then begin
    print_endline "You gave up :(";
    Playerstate.set_points player ~-40; 
    Playerstate.chg_energy player ~-10;
  end
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
      Playerstate.chg_energy player ~-5;
      minigame_debug_v1 player (num+1)
    end
  end

let rec minigame_debug_v2 player = 
  print_endline "To debug, answer these questions correctly\n";
  print_endline "What is 42 / 22 / 2 / 2 ?\n";
  if (read_line() |> String.trim = "1/2" || read_line() |> String.trim = "0.5")
  then begin print_endline "Good. You found one part of the bug\n"; end
  else begin 
    print_endline "You found out you typed in the wrong variable\n";
    print_endline "Lose 5 points\n";
    Playerstate.set_points player ~-5;
  end;
  print_endline "\nWhat is a ghost's favorite datatype?\n";
  if (read_line() |> String.trim |> String.lowercase_ascii = "boolean")
  then begin print_endline "You found out you made a type error\n" end
  else begin print_endline "You made a type error that you didn't notice\n";
    print_endline "Lose 5 points\n";
    Playerstate.set_points player ~-5
  end;
  print_endline "What is a programmer's least favorite animal?\n";
  if (read_line() |> String.trim |> String.lowercase_ascii = "a bug") ||
     (read_line() |> String.trim |> String.lowercase_ascii = "bug")
  then begin print_endline "Congrats! You found the bug!"; end
  else begin 
    print_endline "Sadly, it will continue to bug you again\n"; 
    Playerstate.set_points player ~-5
  end;
  print_endline "After that whole ordeal, you feel like you've lost a few\n
                years of your life. Lose 10 energy.\n";
  Playerstate.chg_energy player ~-10


let minigame_ta player = 
  Playerstate.chg_energy player ~-10;
  print_endline "You were invited to become a TA. Do you accept?";
  print_endline "Type [yes] or [no] > ";
  match read_line () |> String.trim |> String.lowercase_ascii with 
  | "no" -> print_endline 
              "You decided not to become a TA in favor of pursuing other things \n"
  | "yes" -> begin
      print_endline "You are hosting office hours for the class. \n"; 
      print_endline "There are a lot of students waiting, waiting for your 
      guidance in hopefully passing this class. \n"; 
      print_endline "An hour passed where you answered lots of questions and 
      corrected so much not great code your head hurts.\n";
      print_endline "You feel your energy levels drop a bit \n";
      Playerstate.chg_energy player (Random.int 5 + 1);
      print_endline "So, was it a good experience? (Yes or No) \n"; 
      print_string "> "; 
      if (read_line () |> String.trim |> String.lowercase_ascii) = "yes" 
      then begin
        print_endline "Nice! Glad you found being a TA rewarding :) Maybe be one
         again next semester?";
        Playerstate.set_points player 20
      end
      else 
        print_endline "Oops, sorry it wasn't that great for you. Is the pay 
        worth it? (Probably yes)";
      Playerstate.set_points player ~-5
    end
  | _ -> print_endline 
           "You typed in something weird in response to a yes or no question\n";
    print_endline "Points are taken away for typing in something weird";
    Playerstate.set_points player ~-5

(**[print_project_lst lst] prints the projects in [lst]*)
let print_project_lst (lst : Playerstate.project list) = 
  match lst with 
  | Some (n1, d1, s1) :: Some (n2, d2, s2) :: Some (n3, d3, s3) :: t -> 
    print_endline ("1. Name: " ^ n1 ^ "\n   Description: " ^ d1 ^ 
                   "\n   Salary: " ^ (string_of_int s1) ^ "\n");
    print_endline ("2. Name: " ^ n2 ^ "\n   Description: " ^ d2 ^ 
                   "\n   Salary: " ^ (string_of_int s2) ^ "\n");
    print_endline ("3. Name: " ^ n3 ^ "\n   Description: " ^ d3 ^ 
                   "\n   Salary: " ^ (string_of_int s3) ^ "\n")
  | _ -> failwith "invalid list of projects"

(**[prompt_project player lst] prompts the [player] to choose a project out of 
   [lst]*)
let prompt_project player (lst : Playerstate.project list) = 
  let cs1110 = Playerstate.get_visited_tiles player |> List.mem "1110" in
  let rec helper = function 
    | Some (n1, d1, s1) :: Some (n2, d2, s2) :: Some (n3, d3, s3) :: t -> begin
        print_endline 
          "Please type the number of the project you would like: \n";
        print_string "> ";
        begin
          match read_line () |> String.trim with 
          | "1" -> 
            if d1 = "CS 1110 REQUIRED" && not (cs1110) then begin
              print_endline "\nYou have not taken CS 1110. You are not qualified 
              for this project. Please choose again.\n";
              helper lst end
            else begin
              print_endline ("Congrats! Your new project is " ^ n1 ^ " with a 
              salary of " ^ (string_of_int s1) ^ " points.\n");
              Some (n1, d1, s1) end
          | "2" -> 
            if d2 = "CS 1110 REQUIRED" && not (cs1110) then begin
              print_endline "\nYou have not taken CS 1110. You are not qualified
               for this project. Please choose again.\n";
              helper lst end
            else begin
              print_endline ("Congrats! Your new project is " ^ n2 ^ " with a 
              salary of " ^ (string_of_int s2) ^ " points.\n");
              Some (n2, d2, s2) end
          | "3" -> 
            if d3 = "CS 1110 REQUIRED" && not (cs1110) then begin
              print_endline "\nYou have not taken CS 1110. You are not qualified 
              for this project. Please choose again.\n";
              helper lst end
            else begin
              print_endline ("Congrats! Your new project is " ^ n3 ^ " with a 
              salary of " ^ (string_of_int s3) ^ " points.\n");
              Some (n3, d3, s3) end
          | _ -> print_endline "\nInvalid input. Please retry.\n"; helper lst
        end
      end
    | _ -> failwith "invalid list of projects" in 
  helper lst

let choose_project player = 
  print_endline "Generating three random projects. . . \n";
  let projects = Playerstate.three_rand_projects () in
  print_endline "Your project choices are: \n";
  print_project_lst projects;
  prompt_project player projects |> Playerstate.set_project player

let change_project player = 
  let cs1110 = Playerstate.get_visited_tiles player |> List.mem "1110" in
  print_endline "Oh no, for some reason you lost your project!\n"; 
  print_endline "Unfortunately, it's time for a new project.\n"; 
  (**check if CS1110 is required *)
  let rec helper () = 
    match Playerstate.rand_project () with 
    | None -> failwith "not reached"
    | Some (name, desc, salary) as p -> 
      if desc = "CS 1110 REQUIRED" && not (cs1110) then
        helper ()
      else begin
        print_endline ("Your new project is: " ^ name);
        Playerstate.set_project player p 
      end in 
  helper ()

let rec swap_salary player players =
  print_endline 
    "Please type the name of the player you would like to swap salaries with:";
  print_string "\n>";
  match read_line () |> String.lowercase_ascii |> String.trim with 
  | p -> 
    match find_player p players with 
    | None -> 
      print_endline "\nNot a valid player. Please re-enter: \n\n"; 
      swap_salary player players 
    | Some p2 -> 
      let s1 = Playerstate.get_salary player in 
      let s2 = Playerstate.get_salary p2 in
      Playerstate.set_salary player s2;
      Playerstate.set_salary p2 s1;
      print_endline "\nSalaries have been swapped.\n"

let birthday (player : Playerstate.player) players = 
  let rec helper (player : Playerstate.player) players (acc : int) = 
    match players with 
    | [] -> Playerstate.set_points player acc 
    | p :: t -> 
      if Playerstate.get_name player = Playerstate.get_name p then 
        helper player t acc 
      else begin
        Playerstate.set_points p (-5);
        helper player t (acc + 5) 
      end in
  helper player players 0

let pay_day player = 
  match Playerstate.get_project player with 
  | None -> print_endline "\nSorry, you don't have a project right now\n"
  | Some (name, desc, salary) -> 
    print_endline ("Thanks to all of your contributions to " ^ 
                   name ^ ", you have earned " ^ (string_of_int salary) 
                   ^ " points!\n");
    Playerstate.set_points player salary

let pay_raise player = 
  match Playerstate.get_project player with 
  | None -> print_endline "\nSorry, you don't have a project right now\n"
  | Some (name, desc, salary) -> 
    Some(name, desc, salary + 10) 
    |> Playerstate.set_project player;
    print_endline ("Thanks to all of your contributions to " ^ name ^ ", you 
    have earned an extra 10 points to your salary!\n");
    Playerstate.set_points player (salary+10)

let good_interview player= 
  print_endline "Wow! You did great at your interview!";
  print_endline "Actually, we don't know if you got the job or not. Haha\n"; 
  print_endline "You'll know soon though!"; 
  print_endline "Hopefully you did get a good one. ^^";  
  Playerstate.set_points player 25

let bad_interview player = 
  print_endline "Oops, the job interview didn't go so great. \n"; 
  print_endline "Just got to keep going then. \n"; 
  print_endline "Who knows? You might still get a great job. It's just this 
    one didn't go as great. \n";
  print_endline "Don't give up!";
  Playerstate.set_points player ~-10

let job_interview player = 
  Playerstate.chg_energy player ~-12;
  print_endline "You're getting a job, but is it the one you want, or just some 
  job you took because you needed one? \n"; 
  print_endline "Enter a number from 1 to 10:\n";
  print_string "> "; 
  let correct = string_of_int (Random.int 10 + 1) in 
  if (read_line () |> String.trim = correct) 
  then good_interview player
  else bad_interview player

let minigame_coffee_break player =
  (fun () -> Playerstate.chg_energy player (Gui.coffee_break_gui ()))

let minigame_party player = 
  print_endline "Stressed out by all the assignments and psets, you decide to 
  let loose for one night.\n";
  print_endline "So you went to your first (and only?) party at Cornell.\n";
  print_endline 
    "You didn't find it too fun, so left early to wake Netflix on your bed.\n";
  print_endline "So even though you didn't party, you wake up late the next day, 
exhausted, with no motivation to finish the work you put off.\n";
  Playerstate.chg_energy player ~-5

let find_special_event player players board str = 
  let nrg = Playerstate.get_energy player in 
  if nrg < 10 then begin
    print_endline "You do not have enough energy to do special events\n";
    print_endline "Please do yourself a favor and take a break\n"
  end
  else begin
    match str with 
    | "choose_1110_2110" -> minigame_choose_1110_2110 player board
    | "1110" -> minigame_1110 player 
    | "2110" -> minigame_2110 player 
    | "2800" -> minigame_2800 player players
    | "3110" -> minigame_3110 player
    | "3410" -> minigame_3410 player
    | "4410" -> minigame_4410 player
    | "4820" -> minigame_4820 player
    | "4120" -> minigame_4120 player
    | "debug1" -> minigame_debug_v1 player 1
    | "debug2" -> minigame_debug_v2 player 
    | "ta" -> minigame_ta player
    | "choose_project" -> choose_project player
    | "change_project" -> change_project player
    | "swap_project" -> swap_salary player players
    | "birthday" -> birthday player players
    | "pay_day" -> pay_day player 
    | "pay_raise" -> pay_raise player
    | "academic_integrity" -> academic_integrity player players
    | "job_interview1" -> job_interview player
    | "coffee_break" -> minigame_coffee_break player ()
    | "party" -> minigame_party player
    | _ -> failwith "special event not found"
  end
