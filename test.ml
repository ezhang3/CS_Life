open OUnit2
open Board
open Event
open Playerstate
open Tile 

let e = create_event "Day 1" "school starts" "You are now a student at Cornell" (Points [("None", 0)])
let started = create_tile Green e "Start"
let dummy = init_state "Jason" started

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

let player_state_test = [
  get_player_name_test "Works?" dummy "Jason";
  get_points_test "Just started, 0" dummy 0;
]

let board_test = [

]

let suite =
  "test suite for game"  >::: List.flatten [
    [

    ]
  ]

let _ = run_test_tt_main suite
