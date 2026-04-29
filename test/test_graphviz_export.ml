open OUnit2
open Ocaml_autodiff.Graphviz_export
open Ocaml_autodiff.Expr

let count_occurrences str sub =
  let sub_len = String.length sub in
  let str_len = String.length str in
  let count = ref 0 in
  for i = 0 to str_len - sub_len do
    if String.sub str i sub_len = sub then incr count
  done ;
  !count

let rec expected_edges = function
  | Const _ | Var _ -> 0
  | Neg e | Exp e | Log e | Sin e | Cos e | Tanh e -> 1 + expected_edges e
  | Add (l, r) | Sub (l, r) | Mul (l, r) | Div (l, r) ->
      2 + expected_edges l + expected_edges r

let test_constant _ =
  let dot = expr_to_dot (Const 1.) in
  assert_equal 1 (count_occurrences dot "label=\"1.\"")

let test_var _ =
  let dot = expr_to_dot (Var "x") in
  assert_equal 1 (count_occurrences dot "label=\"x\"")

let test_var_shared _ =
  let dot = expr_to_dot (Add (Var "x", Var "x")) in
  assert_equal 1 (count_occurrences dot "label=\"x\"")

let test_unary_edges _ =
  let ast = Log (Neg (Var "x")) in
  let dot = expr_to_dot ast in
  assert_equal (expected_edges ast) (count_occurrences dot "->")

let test_binary_edges _ =
  let ast = Add (Mul (Var "x", Var "y"), Var "z") in
  let dot = expr_to_dot ast in
  assert_equal (expected_edges ast) (count_occurrences dot "->")

let test_edges _ =
  let ast = Div (Mul (Var "x", Var "y"), Sin (Var "z")) in
  let dot = expr_to_dot ast in
  assert_equal (expected_edges ast) (count_occurrences dot "->")

let test_operator_labels _ =
  let dot = expr_to_dot (Add (Var "x", Var "y")) in
  assert_equal 1 (count_occurrences dot "label=\"+\"")

let test_multiple_op_labels _ =
  let dot = expr_to_dot (Add (Var "x", Add (Var "y", Var "z"))) in
  assert_equal 2 (count_occurrences dot "label=\"+\"")

let test_write_expr_dot _ =
  let path = Filename.temp_file "test_dot" ".dot" in
  let ast = Sub (Var "x", Const 1.0) in
  let result = write_expr_dot path ast in
  assert_equal (Ok ()) result ;
  let channel = open_in path in
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel ;
  assert_equal (expr_to_dot ast) contents ;

  Sys.remove path

let test_write_expr_dot_error _ =
  let result = write_expr_dot "/nonexistent/path/file.dot" (Var "x") in
  assert_bool "should be wrapped in Error, not propagated Sys_error"
    (match result with Error _ -> true | Ok _ -> false)

let suite =
  "graphviz_export property tests"
  >::: [
         "constants" >:: test_constant;
         "variable" >:: test_var;
         "multiple variable" >:: test_var_shared;
         "unary op edges" >:: test_unary_edges;
         "binary op edges" >:: test_binary_edges;
         "unary and binary edges" >:: test_edges;
         "operator labels" >:: test_operator_labels;
         "multiple op labels" >:: test_multiple_op_labels;
         "writing dot file" >:: test_write_expr_dot;
         "writing dot error" >:: test_write_expr_dot_error;
       ]

let () = run_test_tt_main suite
