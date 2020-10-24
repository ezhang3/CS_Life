open OUnit2
open Board
open Event
open Playerstate
open Tile 

let e = create_event "Day 1" "school starts" "You are now a student at Cornell" (Points [("None", 0)])
let started = create_tile Green e "Start"
let dummy = init_state "Jason" started

let event =
  create_event "Career Fair"
    "10"
    "The Career Fair. A place to stand in line, chat with recruiters, and trade resumes for free stuff."
    (Points [("Gain", 10)])
let tile = Tile.create_tile Red event "Career Fair Red"
let tile2 = Tile.create_tile Blue event "Career Fair Blue"
let dummy_player = init_state "Jason" tile
let test_board = create_board 2

(* Should this be default in board? *)
let compare_tiles tile1 tile2 = if get_tile_id tile1 = get_tile_id tile2
  then true else false

let get_player_name_test 
    (name : string)  
    (st: st)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_name st))

let get_points_test 
    (name: string)
    (st: st)
    (expected_output: int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_points st))

let start_tile_test (name : string) (board) (expected) =
  name >:: (fun _ -> assert_equal expected (start_tile board))

let next_tile_test (name : string) (tile) (compare) (board)
    expected =
  name >:: (fun _ ->
      assert_equal expected (next_tile tile compare board))

let player_state_test = [
  get_player_name_test "Works?" dummy "Jason";
  get_points_test "Just started, 0" dummy 0;
]

(* creates a board and runs next tile to get the next board*)
let board_test = [
  (* need a start tile to give to players*)
  start_tile_test "start tile is career fair" test_board tile;
  next_tile_test "first to second 2 tile board"
    tile compare_tiles test_board [tile2];
  next_tile_test "second to first 2 tile board"
    tile2 compare_tiles test_board [tile];
  (* test next_tile on a tile w/o adjacent tiles*)
]

let suite =
  "test suite for game"  >::: List.flatten [
    board_test;
  ]

let _ = run_test_tt_main suite
