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
    ["gain 15"]

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

let tile_effects_points_test (name : string) tile (expected : int) = 
  name >:: (fun _ -> assert_equal expected (get_effect_points tile))

let tile_test = [
  tile_color_test "Red tile" tile1 Red;
  tile_color_test "Blue tile" tile2 Blue;
  tile_event_name_test "career fair event" tile1 "Career Fair";
  tile_event_name_test "prelims event" tile2 "Prelims";
  tile_id_test "tile1 id" tile1 "Tile1";
  tile_id_test "tile12 id" tile2 "Tile2";
  tile_description_test "tile1 description" tile1 "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff.";
  tile_description_test "tile2 description" tile2 "Prelims. A time to shut yourself in your room to study and hopefully pass all your classes.";
  (* tile_effects_test "tile1 effects" tile1 ["gain"; "10"];  *)
  (* tile_effects_test "tile2 effects" tile2 *) (* issue with abstract type*)
  (* tile_effects_points_test *)
]
let test_player = init_state "Jason" tile1
let test_board = create_board (Yojson.Basic.from_file "gameboard1.json")

let start_tile_test (name : string) (board : gameboard) (expected : string) =
  name >:: (fun _ -> assert_equal expected (get_tile_event_name 
                                              (start_tile board)))

let end_tile_test (name : string) (board) (expected) =
  name >:: (fun _ -> assert_equal expected (get_tile_event_name 
                                              (end_tile board)))

let next_tile_test (name : string) tile compare board expected =
  name >:: (fun _ ->
      assert_equal expected (next_tile tile compare board))

let compare_tiles_id_teset (name: string) tile_f tile_s (expected : bool) 
  = name >:: (fun _ -> assert_equal expected (compare_tiles_id tile_f tile_s))

let just_two = create_board (Yojson.Basic.from_file 
                               "superbasicboard.json")
let start = start_tile just_two
let last = end_tile just_two

let board_test = [
  start_tile_test "start tile is career fair" test_board "Career Fair";
  end_tile_test "end tile is prelims" test_board "Start CS 3110"; 
  next_tile_test "first to second 2 tile board" start compare_tiles_id 
    just_two [last];
  (* next_tile_test "second to first 2 tile board" tile2 compare_tiles_id 
     test_board [tile1]; *)
  (* TODO: test next_tile on a tile w/o adjacent tiles*)
]

(* let new_tile = go test_player test_board 1; get_current_tile test_player
   let two_spaces = go test_player test_board 2; get_current_tile test_player *)

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

let go_test (name : string) player board moves (expected: string) = 
  name >:: (fun _ -> assert_equal expected (get_tile_event_name(
      get_current_tile player)))

let player_state_test = [
  get_player_name_test "Works?" test_player "Jason";
  get_points_test "Just started, 0" test_player 0; 
  (* go_test "go test 1 move" test_player test_board 1 "Start CS 1110";
     go_test "go test 2 moves" test_player test_board 1 "Start CS 3110" *)
]

let suite =
  "test suite for game"  >::: List.flatten [
    tile_test;
    board_test;
    player_state_test;
  ]

let _ = run_test_tt_main suite
