type player_name = string
type points = int
type salary = int
type project_name = string
type project = (project_name * string * salary) option
type projects = project array
type study_partners = int

type player = {
  name : player_name;
  mutable points : points;
  mutable study_partners : study_partners;
  mutable project : project;
  mutable current_tile : Tile.tile;
  mutable visited_tiles : Tile.tile_id list;
  mutable items : string list;
  mutable energy : int 
}

let init_state name start = {
  name = name;
  points = 0;
  study_partners = 0;
  project = None;
  current_tile = start;
  visited_tiles = [Tile.get_tile_id start];
  items = [];
  energy = 100
}
let extract_opt = function 
  | None -> "none"
  | Some (p, _, _) -> p 

let rec string_list acc = function 
  | [] -> acc
  | h :: t -> if t = [] then string_list (acc ^ h) t
    else string_list (acc ^ h ^ ",") t

let print_state player = 
  print_endline ("Name: " ^ player.name ^ 
                 "\nPoints: " ^ string_of_int player.points ^ 
                 "\nStudy Partners: " ^ string_of_int player.study_partners ^ 
                 "\nProject: " ^ extract_opt player.project^ 
                 "\nItems: " ^ string_list "" player.items ^
                 "\nEnergy: "^string_of_int player.energy)

let check_valid_player player players = not (List.mem player players)

let make_player_list (n : int) (start : Tile.tile) = 
  let rec make_list n count acc = 
    if count = n then acc 
    else begin
      let num = count + 1 in
      print_endline ("\nPlease enter the name of Player " ^ 
                     string_of_int num ^ "\n");
      print_string  ("> ");
      match read_line ()|> String.lowercase_ascii |> String.trim with
      | name -> 
        let p = init_state name start in
        if check_valid_player p acc then 
          p :: acc |> make_list n num
        else begin
          print_endline "\nPlayer name is already taken\n";
          make_list n num acc
        end 
    end in
  make_list n 0 [] |> List.rev

let get_name st = 
  st.name

let set_points st pts = 
  let orig_pts = st.points in
  st.points <- pts + orig_pts

let get_points st = 
  st.points

let add_study_partners st n = 
  let orig_partners = st.study_partners in 
  st.study_partners <- orig_partners + n

let get_study_partners st = 
  st.study_partners

let p : projects = [|
  Some ("CS 1110 Head TA", "CS 1110 REQUIRED", 100);
  Some ("DTI Developer", "A software developer for Cornell DTI", 80);
  Some ("AppDev Developer", "A software developer for Cornell AppDev", 80);
  Some ("Hack4Impact Developer", "A software developer for Cornell H4I", 60);
  Some ("CS Research", "You're on a research team with a CS professor", 70);
  Some ("Part-time barista", "Part-time barista at the CTown Starbucks", 30);
  Some ("Olin Library Part-time", "Olin Library part timer", 40);
  Some ("Rose Dining Part-time", "Rose Dining hall part timer", 40);
  Some ("CS 1110 Consultant", "CS 1110 REQUIRED", 90);
  Some ("Robotics Club", "Member of the robotics club", 70);
  Some ("Sorority/Fraternity", "You're apart of Greek Life!", 50);
  Some ("Business Club", 
        "Maybe minor in business? It's good to step outside of STEM to broaden your skills!", 60)
|]

let three_rand_projects () = 
  Random.self_init ();
  let rec helper acc count = 
    if count = 3 then acc 
    else
      let i = ref (Random.int 10) in
      while List.mem p.(!i) acc || p.(!i) = None do 
        i := Random.int 10
      done;
      count + 1 |> helper (p.(!i) :: acc) in
  helper [] 0

let rand_project () = 
  Random.self_init ();
  let i = ref (Random.int 10) in
  while p.(!i) = None do 
    i := Random.int 10
  done;
  p.(!i)

let set_project st proj = 
  st.project <- proj;
  for i = 0 to Array.length p - 1 do 
    if p.(i) = proj then p.(i) <- None
    else ()
  done

let get_project st = 
  st.project

let get_salary st = 
  match st.project with
  | None -> 0
  | Some (_, _, salary) -> salary

let set_salary st salary = 
  match st.project with 
  | None -> ()
  | Some (n, d, s) -> set_project st (Some (n, d, salary))

let set_current_tile st tile = 
  let orig_visited = st.visited_tiles in 
  st.current_tile <- tile; 
  st.visited_tiles <- Tile.get_tile_id tile :: orig_visited

let get_current_tile st = 
  st.current_tile

let get_energy st = 
  st.energy

let chg_energy st nrg = 
  let cur_e = get_energy st in 
  if cur_e + nrg < 0 then 
    st.energy <- 0 
  else 
    st.energy <- cur_e + nrg

let go st board n = 
  let rec find_tile tile board n =
    match n with
    | 0 -> set_current_tile st tile
    | other -> begin
        match Board.next_tiles st.current_tile Board.compare_tiles_id board with
        | [] -> set_current_tile st tile
        | tile :: [] -> set_current_tile st tile; 
          if Tile.get_tile_color tile = Yellow then ()
          else if Tile.get_tile_event_name tile = "Pay Day!" && other > 1 then 
            begin
              print_endline "\nYou passed a pay day...\n";
              get_salary st |> set_points st;
              find_tile tile board (n - 1)
            end
          else
            find_tile tile board (n - 1)
        | h :: t -> set_current_tile st h
      end in 
  find_tile st.current_tile board n

let get_visited_tiles st = 
  st.visited_tiles 

let get_items st = 
  st.items

let add_items st i = 
  st.items <- (i :: st.items)