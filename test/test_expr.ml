open OUnit2
open Ocaml_autodiff.Expr

let assert_result_error result expected_prefix =
  match result with
  | Ok _ -> assert_failure "expected Error but got Ok"
  | Error message ->
      let starts_with =
        String.length message >= String.length expected_prefix
        && String.sub message 0 (String.length expected_prefix)
           = expected_prefix
      in
      assert_bool
        ("expected error prefix: " ^ expected_prefix ^ ", got: " ^ message)
        starts_with

let to_string_test _ =
  assert_equal "1.5" (to_string (Const 1.5)) ;
  assert_equal "x" (to_string (Var "x")) ;
  assert_equal "(x + 1.)" (to_string (Add (Var "x", Const 1.0))) ;
  assert_equal "(x - 1.)" (to_string (Sub (Var "x", Const 1.0))) ;
  assert_equal "(x * 1.)" (to_string (Mul (Var "x", Const 1.0))) ;
  assert_equal "(x / 1.)" (to_string (Div (Var "x", Const 1.0))) ;
  assert_equal "(-x)" (to_string (Neg (Var "x"))) ;
  assert_equal "exp(x)" (to_string (Exp (Var "x"))) ;
  assert_equal "log(x)" (to_string (Log (Var "x"))) ;
  assert_equal "sin(x)" (to_string (Sin (Var "x"))) ;
  assert_equal "cos(x)" (to_string (Cos (Var "x"))) ;
  assert_equal "tanh(x)" (to_string (Tanh (Var "x")))

let parse_invalid_char_test _ =
  assert_result_error (parse "1 + @") "unexpected character '@'"

let parse_invalid_number_test _ =
  assert_result_error (parse "1.2.3") "invalid number literal:"

let parse_trailing_token_test _ =
  assert_result_error (parse "1 + 1 1") "unexpected trailing token"

let parse_missing_paren_test _ =
  assert_result_error (parse "(1 + 1") "expected token )"

let parse_unexpected_token_primary_test _ =
  assert_result_error (parse "()") "unexpected token )" ;
  assert_result_error (parse "* 2") "unexpected token *" ;
  assert_result_error (parse "1 + * 2") "unexpected token *"

let parse_error_propagation_test _ =
  assert_result_error (parse "1 + ()") "unexpected token )" ;
  assert_result_error (parse "1 - ()") "unexpected token )" ;
  assert_result_error (parse "1 * ()") "unexpected token )" ;
  assert_result_error (parse "1 / ()") "unexpected token )" ;
  assert_result_error (parse "-(*)") "unexpected token *" ;
  assert_result_error (parse "sin(*)") "unexpected token *"

let parse_token_name_coverage_test _ =
  assert_result_error (parse "1 x") "unexpected trailing token identifier(x)" ;
  assert_result_error (parse "+ 1") "unexpected token +" ;
  assert_result_error (parse "* 1") "unexpected token *" ;
  assert_result_error (parse "/ 1") "unexpected token /" ;
  assert_result_error (parse "(1) (") "unexpected trailing token (" ;
  assert_result_error (parse "(1) )") "unexpected trailing token )"

let parse_unary_error_test _ =
  assert_result_error (parse "-@") "unexpected character '@'" ;
  assert_result_error (parse "1 / @") "unexpected character '@'"

let parse_function_error_test _ =
  assert_result_error (parse "sin(1") "expected token ) but found end-of-input" ;
  (* Trigger the parse_function_call error when expression parsing fails *)
  assert_result_error (parse "sin(@") "unexpected character '@'"

let parse_paren_success_test _ =
  match parse "(1)" with
  | Ok _ -> ()
  | Error message -> assert_failure ("parse surprisingly failed: " ^ message)

let parse_success_coverage_test _ =
  (match parse "1 / 2" with
  | Ok (Div (Const 1., Const 2.)) -> ()
  | _ -> assert_failure "expected Div") ;

  (match parse "-1" with
  | Ok (Neg (Const 1.)) -> ()
  | _ -> assert_failure "expected Neg") ;

  match parse "tanh(1)" with
  | Ok (Tanh (Const 1.)) -> ()
  | _ -> assert_failure "expected Tanh"

let suite =
  "expr tests"
  >::: [
         "to_string tests" >:: to_string_test;
         "parse invalid char" >:: parse_invalid_char_test;
         "parse invalid number literal" >:: parse_invalid_number_test;
         "parse trailing token" >:: parse_trailing_token_test;
         "parse missing paren" >:: parse_missing_paren_test;
         "parse unexpected token primary"
         >:: parse_unexpected_token_primary_test;
         "parse error propagation" >:: parse_error_propagation_test;
         "parse token name coverage" >:: parse_token_name_coverage_test;
         "parse unary error" >:: parse_unary_error_test;
         "parse function error" >:: parse_function_error_test;
         "parse paren success" >:: parse_paren_success_test;
         "parse success coverage" >:: parse_success_coverage_test;
       ]

let () = run_test_tt_main suite
