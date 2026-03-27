open OUnit2
open Teamproject

(** Test suite for CS 3110 team project *)

let greet_test name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (greet "Team") ~printer:(fun x -> x)

let version_test =
  "version test" >:: fun _ -> assert_equal "1.0.0" version ~printer:(fun x -> x)

let suite =
  "test suite for teamproject"
  >::: [ greet_test "greet test" "Welcome to Team's project!"; version_test ]

let () = run_test_tt_main suite
