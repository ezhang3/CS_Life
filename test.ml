open OUnit2
open Board
open Event
open Player
open Playerstate
open Tile 



let suite =
  "test suite for game"  >::: List.flatten [

  ]

let _ = run_test_tt_main suite
