open OUnit2
open Ocaml_autodiff

let assert_float_equal ?(eps = 1e-6) expected actual =
  let float_close left right = Float.abs (left -. right) <= eps in
  assert_equal expected actual ~printer:string_of_float ~cmp:float_close

let assert_result_ok_gradient result expected_value expected_grads =
  match result with
  | Error message -> assert_failure ("expected Ok but got Error: " ^ message)
  | Ok (value, grads) ->
      assert_float_equal expected_value value ;
      let rec check_expected items =
        match items with
        | [] -> ()
        | (name, expected_grad) :: rest ->
            let actual =
              match List.assoc_opt name grads with
              | None -> assert_failure ("missing gradient for variable " ^ name)
              | Some g -> g
            in
            assert_float_equal expected_grad actual ;
            check_expected rest
      in
      check_expected expected_grads

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
        ("expected error prefix " ^ expected_prefix ^ ", got: " ^ message)
        starts_with

let gradient_expr_test_polynomial _ =
  let expr = Expr.Add (Expr.Mul (Expr.Var "x", Expr.Var "x"), Expr.Var "x") in
  let result = Forward_ad.gradient_expr expr [ ("x", 2.0) ] in
  assert_result_ok_gradient result 6.0 [ ("x", 5.0) ]

let gradient_expr_test_multivar _ =
  let expr =
    Expr.Add (Expr.Mul (Expr.Var "x", Expr.Var "y"), Expr.Sin (Expr.Var "x"))
  in
  let x = 1.2 in
  let y = 0.5 in
  let expected_value = (x *. y) +. sin x in
  let expected_dx = y +. cos x in
  let expected_dy = x in
  let result = Forward_ad.gradient_expr expr [ ("x", x); ("y", y) ] in
  assert_result_ok_gradient result expected_value
    [ ("x", expected_dx); ("y", expected_dy) ]

let gradient_expr_test_log_domain_error _ =
  let expr = Expr.Log (Expr.Var "x") in
  let result = Forward_ad.gradient_expr expr [ ("x", 0.0) ] in
  assert_result_error result "log undefined"

let gradient_expr_test_missing_var_error _ =
  let expr = Expr.Add (Expr.Var "x", Expr.Var "y") in
  let result = Forward_ad.gradient_expr expr [ ("x", 1.0) ] in
  assert_result_error result "missing variable assignment"

let gradient_expr_test_division_by_zero _ =
  let expr = Expr.Div (Expr.Const 1.0, Expr.Var "x") in
  let result = Forward_ad.gradient_expr expr [ ("x", 0.0) ] in
  assert_result_error result "division by zero"

let gradient_expr_test_complex_trig _ =
  let expr = Expr.Add (Expr.Tanh (Expr.Var "x"), Expr.Cos (Expr.Var "y")) in
  let x = 0.5 in
  let y = 1.0 in
  let t = tanh x in
  let expected_value = t +. cos y in
  let expected_dx = 1.0 -. (t *. t) in
  let expected_dy = -.sin y in
  let result = Forward_ad.gradient_expr expr [ ("x", x); ("y", y) ] in
  assert_result_ok_gradient result expected_value
    [ ("x", expected_dx); ("y", expected_dy) ]

let gradient_expr_test_exponential_log _ =
  let expr = Expr.Div (Expr.Exp (Expr.Var "x"), Expr.Log (Expr.Var "y")) in
  let x = 2.0 in
  let y = 3.0 in
  let ex = exp x in
  let ly = log y in
  let expected_value = ex /. ly in
  let expected_dx = ex /. ly in
  let expected_dy = -.ex /. (y *. ly *. ly) in
  let result = Forward_ad.gradient_expr expr [ ("x", x); ("y", y) ] in
  assert_result_ok_gradient result expected_value
    [ ("x", expected_dx); ("y", expected_dy) ]

let gradient_expr_test_neg_sub _ =
  let expr = Expr.Sub (Expr.Neg (Expr.Var "x"), Expr.Var "y") in
  let result = Forward_ad.gradient_expr expr [ ("x", 1.0); ("y", 2.0) ] in
  assert_result_ok_gradient result (-3.0) [ ("x", -1.0); ("y", -1.0) ]

let gradient_expr_test_const _ =
  let expr = Expr.Const 5.0 in
  let result = Forward_ad.gradient_expr expr [] in
  assert_result_ok_gradient result 5.0 []

let suite =
  "forward_ad tests"
  >::: [
         "gradient polynomial" >:: gradient_expr_test_polynomial;
         "gradient multivar" >:: gradient_expr_test_multivar;
         "gradient complex trig" >:: gradient_expr_test_complex_trig;
         "gradient exponential log" >:: gradient_expr_test_exponential_log;
         "gradient neg sub" >:: gradient_expr_test_neg_sub;
         "gradient const" >:: gradient_expr_test_const;
         "gradient log domain error" >:: gradient_expr_test_log_domain_error;
         "gradient missing var error" >:: gradient_expr_test_missing_var_error;
         "gradient division by zero" >:: gradient_expr_test_division_by_zero;
       ]

let () = run_test_tt_main suite
