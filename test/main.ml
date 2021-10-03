open OUnit2
open Analyzer
open Intake

let state_test : test = "name" >:: fun _ -> assert_equal "" ""

let make_state_test : test = state_test

let intake_tests = []

let suite = "test suite for Final" >::: List.flatten [ intake_tests ]

let _ = run_test_tt_main suite
