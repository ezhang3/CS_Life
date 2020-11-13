type player_name = string
type points = int
type salary = int
type project_name = string
type project = (project_name * salary) option
type study_partners = int

type player = {
  name : player_name;
  mutable points : points;
  mutable study_partners : study_partners;
  mutable project : project;
  mutable current_tile : Tile.tile;
  mutable visited_tiles : Tile.tile list;
  mutable items : string list; (** not sure *)
}

let init_state name start = {
  name = name;
  points = 0;
  study_partners = 0;
  project = None;
  current_tile = start;
  visited_tiles = [start];
  items = []
}

let print_state player = 
  print_endline ("Name: " ^ player.name ^ 
                 "\nPoints: " ^ string_of_int player.points ^ 
                 "\nStudy Partners: " ^ string_of_int player.study_partners ^ 
                 "\nProject: \nItems: ")

let rec get_nth_player players n = 
  match players with 
  | [] -> failwith "no players"
  | h :: t -> if n = 0 then h 
    else get_nth_player t (n - 1)

(** current implementation of make_player_list does not account for different 
    starting points. All players will currently all start at the same tile. *)
let make_player_list (n : int) (start : Tile.tile) = 
  let rec make_list n acc = 
    match n with 
    | 0 -> acc
    | _ ->
      print_endline ("\nPlease enter the name of Player " ^ 
                     string_of_int n ^ "\n");
      print_string  ("> ");
      match read_line () with
      | name -> 
        let p = init_state name start in
        p :: acc |> make_list (n - 1) in
  make_list n []

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

let set_project st proj = 
  st.project <- proj

let get_project st = 
  st.project

let get_salary st = 
  match st.project with
  | None -> 0
  | Some (_, salary) -> salary

let set_current_tile st tile = 
  let orig_visited = st.visited_tiles in 
  st.current_tile <- tile; 
  st.visited_tiles <- tile :: orig_visited

let get_current_tile st = 
  st.current_tile

(** moves the player n spaces forward.
    TODO: Cannot handle branching paths yet *)
let go st board n = 
  let rec find_tile tile board n =
    match n with
    | 0 -> set_current_tile st tile
    | _ -> begin
        set_current_tile st tile;
        match Board.next_tiles st.current_tile Board.compare_tiles_id board with
        | [] -> Board.end_tile board |> set_current_tile st 
        | tile :: [] -> find_tile tile board (n - 1)
        | h :: t -> failwith "branching paths case not implemented, prompt user"
      end in find_tile st.current_tile board n

let get_visited_tiles st = 
  st.visited_tiles 

let get_items st = 
  st.items


