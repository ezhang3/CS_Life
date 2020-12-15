open OUnit2
open Board
open Playerstate
open Tile 

let tile1 = Tile.create_tile 
    "Tile1" 
    "Red" "Career Fair" 
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff." 
    ["gain 10"]

let tile2 = Tile.create_tile 
    "Tile2" 
    "Blue" "Prelims" 
    "Prelims. A time to shut yourself in your room to study and hopefully pass all your classes." 
    ["lose 15"]

let tile_proj = Tile.create_tile 
    "Make a project" "Green" "Start your own project"
    "You have started making your own CS project. You plan to show it off in your resume and gain some good technical experience"
    ["gain 100"; "minigame choose_project"]

let tile_start = Tile.create_tile "start" "Black" "Start Tile" "Start Tile"
    ["gain 0"]

let tile_id_test (name : string) tile (expected : tile_id) = 
  name >:: (fun _ -> assert_equal expected (get_tile_id tile))

let tile_color_test (name : string) tile (expected : color) = 
  name >:: (fun _ -> assert_equal expected (get_tile_color tile))

let tile_event_name_test (name : string) tile (expected: string) = 
  name >:: (fun _ -> assert_equal expected (get_tile_event_name tile))

let tile_description_test (name : string) tile (expected : string) = 
  name >:: (fun _ -> assert_equal expected (get_tile_description tile))

let tile_effects_test (name : string) tile (expected : effect list) =
  name >:: (fun _ -> assert_equal expected (get_tile_effects tile))

let tile_test = [
  tile_color_test "Red tile" tile1 Red;
  tile_color_test "Blue tile" tile2 Blue;
  tile_event_name_test "career fair event" tile1 "Career Fair";
  tile_event_name_test "prelims event" tile2 "Prelims";
  tile_id_test "tile1 id" tile1 "Tile1";
  tile_id_test "tile12 id" tile2 "Tile2";
  tile_description_test "tile1 description" tile1 "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff.";
  tile_description_test "tile2 description" tile2 "Prelims. A time to shut yourself in your room to study and hopefully pass all your classes.";
  tile_effects_test "tile1 effects" tile1 [Points ("Gained", 10)]; 
  tile_effects_test "tile2 effects" tile2 [Points ("Lost", 15)]; 
  tile_effects_test "project tile effects" tile_proj [Points ("Gained", 100); Minigame "choose_project"]
]

let test_board = Board.create_board (Yojson.Basic.from_file "gameboard1.json")
let start = start_tile test_board
let last = end_tile test_board

(* helper for comparing Tile.tile lists, get tile_id list for non-abstract 
   types,easier comparison *)
let rec tile_id_list acc (lst: Tile.tile list)= match lst with
  | [] -> acc
  | h :: t -> tile_id_list (Tile.get_tile_id h :: acc) t

let start_tile_test (name : string) (board : gameboard) (expected : string) =
  name >:: (fun _ -> assert_equal expected (get_tile_id (start_tile board)))

let end_tile_test (name : string) (board: gameboard) (expected: string) =
  name >:: (fun _ -> assert_equal expected (get_tile_id (end_tile board)))

let next_tile_test (name : string) tile compare board (expected) =
  let next_tiles_id_list = tile_id_list [] (next_tiles tile compare board) in 
  name >:: (fun _ ->
      assert_equal expected next_tiles_id_list)

let find_tile_by_id_test (name: string) id board expected =
  let found_id = find_tile_by_id id board |> get_tile_id in
  name >:: (fun _ -> assert_equal expected found_id)

let compare_tiles_id_test (name: string) tile_f tile_s (expected : bool) 
  = name >:: (fun _ -> assert_equal expected (compare_tiles_id tile_f tile_s))

let board_test = [
  start_tile_test "start tile is start" test_board "start";
  end_tile_test "end tile is graduation" test_board "Graduation";
  compare_tiles_id_test "start and end tile" start last false;
  next_tile_test "first to second 2 tile board" start compare_tiles_id 
    test_board ["choose 1110 or 2110"];
  next_tile_test "next tile for end tile" last compare_tiles_id 
    test_board [];  
  (* when branching paths implemented *)                              
  (* next_tile_test "multiple next tiles" start compare_tiles_id test_board 
     [stuff] *)
  find_tile_by_id_test "1110 waiting in board" "1110 waiting spot" test_board 
    "1110 waiting spot";
  find_tile_by_id_test "CS 2800 final find tile id" "CS 2800 final" test_board 
    "CS 2800 final";
  "tile id not in board" >:: 
  (fun _ -> assert_raises (No_Tile "No such tile exists in the given board") 
      (fun () -> find_tile_by_id "hi there" test_board));
  compare_tiles_id_test "tile1, tile2 compare" tile1 tile2 false;
  compare_tiles_id_test "start in board, tile_start compare" start tile_start 
    true; 
]

let get_player_name_test 
    (name : string)  
    (st: player)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_name st))     

let get_points_test 
    (name: string)
    (st: player)
    (expected_output: int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_points st))

let get_study_partners_test 
    (name: string)
    (st: player)
    (expected_output: study_partners) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_study_partners st))

let get_project_test 
    (name: string)
    (st: player)
    (expected_output: project) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_project st))

let get_salary_test (name: string) (st: player) (expected) = 
  name >:: (fun _ -> assert_equal expected (get_salary st))

let get_current_tile_test 
    (name: string)
    (st: player)
    (expected_output: tile_id) : test = 
  name >:: (fun _ ->
      print_endline ("id from tile in test: " ^ (get_current_tile st 
                                                 |> get_tile_id)); 
      assert_equal expected_output (get_current_tile st 
                                    |> get_tile_id))

let get_visited_tiles_test
    (name: string)
    (st: player)
    (expected_output: tile_id list) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (st 
                                    |> get_visited_tiles
                                    |> tile_id_list []))

let get_items_test 
    (name: string)
    (st: player)
    (expected_output: string list) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_items st))      

let go_test (name : string) player board moves (expected: string) = 
  name >:: (fun _ -> go player board moves; 
             assert_equal expected (player 
                                    |> get_current_tile 
                                    |> get_tile_id))

(* let one_space_id = go test_player test_board 1; get_current_tile test_player 
                                                |> get_tile_id
   let two_spaces_id = go test_player test_board 2; get_current_tile test_player 
                                                 |> get_tile_id *)

let test_player = init_state "Jason" (start_tile test_board)

let player_state_test = [
  get_player_name_test "player name" test_player "Jason";
  get_points_test "Just started, 0" test_player 0; 
  get_project_test "no proj yet" test_player None; 
  get_salary_test "0 salary rn" test_player 0; 
  get_items_test "no items yet" test_player [];
  get_study_partners_test "no study partners yet" test_player 0;
  (* get_current_tile_test "On start" test_player "start"; *)
  (* get_visited_tiles_test "visited start only" test_player ["start"];
     go_test "go test 1 move" test_player test_board 1 "choose 1110 or 2110";
     get_current_tile_test "moved one to tile 1" (go test_player test_board 1; test_player) 
     "choose 1110 or 2110"; *)
  (* get_visited_tiles_test "start and tile1" test_player ["choose 1110 or 2110"; "start"]; *)
  (* go_test "go test 2 moves" test_player test_board 1 "1110"; *)
]

let suite =
  "test suite for game"  >::: List.flatten [
    tile_test;
    board_test;
    player_state_test;
  ]

let _ = run_test_tt_main suite