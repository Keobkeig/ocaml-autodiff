open OUnit2
open Ocaml_autodiff

let assert_float_equal ?(eps = 1e-6) expected actual =
  let float_close left right = Float.abs (left -. right) <= eps in
  assert_equal expected actual ~printer:string_of_float ~cmp:float_close

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

let parity_config : Trainer.adam_config =
  { learning_rate = 0.05; beta1 = 0.9; beta2 = 0.999; epsilon = 1e-8 }

let test_adam_loss_decreases _ =
  let initial_loss =
    Trainer.mse_loss Trainer.default_model Trainer.demo_samples
  in
  match
    Trainer.train_linear_adam ~epochs:1000 ~config:parity_config
      Trainer.default_model Trainer.demo_samples
  with
  | Error message -> assert_failure ("training failed: " ^ message)
  | Ok (model, history) ->
      let final_loss =
        match List.rev history with [] -> initial_loss | head :: _ -> head
      in
      assert_bool
        ("expected final loss < initial loss, got initial="
        ^ string_of_float initial_loss
        ^ " final=" ^ string_of_float final_loss)
        (final_loss < initial_loss) ;
      assert_float_equal ~eps:0.2 2.0 model.w ;
      assert_float_equal ~eps:0.2 1.0 model.b

let test_adam_history_length _ =
  match
    Trainer.train_linear_adam ~epochs:25 ~config:Trainer.default_adam_config
      Trainer.default_model Trainer.demo_samples
  with
  | Error message -> assert_failure ("training failed: " ^ message)
  | Ok (_, history) ->
      assert_equal 25 (List.length history) ~printer:string_of_int

let test_adam_zero_epochs _ =
  match
    Trainer.train_linear_adam ~epochs:0 ~config:Trainer.default_adam_config
      Trainer.default_model Trainer.demo_samples
  with
  | Error message -> assert_failure ("training failed: " ^ message)
  | Ok (model, history) ->
      assert_equal 0 (List.length history) ;
      assert_float_equal Trainer.default_model.w model.w ;
      assert_float_equal Trainer.default_model.b model.b

let test_adam_bad_epochs _ =
  let result =
    Trainer.train_linear_adam ~epochs:(-1) ~config:Trainer.default_adam_config
      Trainer.default_model Trainer.demo_samples
  in
  assert_result_error result "epochs must be non-negative"

let test_adam_bad_lr _ =
  let bad_config : Trainer.adam_config =
    { learning_rate = 0.0; beta1 = 0.9; beta2 = 0.999; epsilon = 1e-8 }
  in
  let result =
    Trainer.train_linear_adam ~epochs:10 ~config:bad_config
      Trainer.default_model Trainer.demo_samples
  in
  assert_result_error result "learning_rate must be positive"

let test_adam_bad_beta1 _ =
  let bad_config : Trainer.adam_config =
    { learning_rate = 0.001; beta1 = 1.0; beta2 = 0.999; epsilon = 1e-8 }
  in
  let result =
    Trainer.train_linear_adam ~epochs:10 ~config:bad_config
      Trainer.default_model Trainer.demo_samples
  in
  assert_result_error result "beta1 must be in (0, 1)"

let test_adam_bad_beta2 _ =
  let bad_config : Trainer.adam_config =
    { learning_rate = 0.001; beta1 = 0.9; beta2 = 0.0; epsilon = 1e-8 }
  in
  let result =
    Trainer.train_linear_adam ~epochs:10 ~config:bad_config
      Trainer.default_model Trainer.demo_samples
  in
  assert_result_error result "beta2 must be in (0, 1)"

let test_adam_bad_epsilon _ =
  let bad_config : Trainer.adam_config =
    { learning_rate = 0.001; beta1 = 0.9; beta2 = 0.999; epsilon = 0.0 }
  in
  let result =
    Trainer.train_linear_adam ~epochs:10 ~config:bad_config
      Trainer.default_model Trainer.demo_samples
  in
  assert_result_error result "epsilon must be positive"

let test_adam_empty_samples _ =
  let result =
    Trainer.train_linear_adam ~epochs:5 ~config:Trainer.default_adam_config
      Trainer.default_model []
  in
  assert_result_error result "cannot train on empty dataset"

let write_temp_csv contents =
  let path = Filename.temp_file "samples" ".csv" in
  let channel = open_out path in
  output_string channel contents ;
  close_out channel ;
  path

let test_csv_basic _ =
  let path = write_temp_csv "0,1\n1,3\n2,5\n3,7\n" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  match result with
  | Error message -> assert_failure ("expected Ok, got Error: " ^ message)
  | Ok samples ->
      assert_equal 4 (List.length samples) ~printer:string_of_int ;
      let first = List.hd samples in
      assert_float_equal 0.0 first.Trainer.x ;
      assert_float_equal 1.0 first.Trainer.y

let test_csv_skips_blank_lines _ =
  let path = write_temp_csv "\n0,1\n\n1,3\n\n" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  match result with
  | Error message -> assert_failure ("expected Ok, got Error: " ^ message)
  | Ok samples -> assert_equal 2 (List.length samples) ~printer:string_of_int

let test_csv_handles_whitespace _ =
  let path = write_temp_csv "  1.5 ,  2.5  \n" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  match result with
  | Error message -> assert_failure ("expected Ok, got Error: " ^ message)
  | Ok [ sample ] ->
      assert_float_equal 1.5 sample.Trainer.x ;
      assert_float_equal 2.5 sample.Trainer.y
  | Ok _ -> assert_failure "expected exactly one sample"

let test_csv_missing_file _ =
  let result = Trainer.load_csv_samples "/nonexistent/path/data.csv" in
  match result with
  | Ok _ -> assert_failure "expected Error for missing file"
  | Error _ -> ()

let test_csv_empty_file _ =
  let path = write_temp_csv "" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  assert_result_error result "csv file contains no samples"

let test_csv_bad_columns _ =
  let path = write_temp_csv "1,2,3\n" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  assert_result_error result "expected two columns"

let test_csv_bad_number _ =
  let path = write_temp_csv "1,abc\n" in
  let result = Trainer.load_csv_samples path in
  Sys.remove path ;
  assert_result_error result "invalid number"

let test_csv_loads_repo_dataset _ =
  let result = Trainer.load_csv_samples "../data/y_eq_2x_plus_1.csv" in
  match result with
  | Error message -> assert_failure ("expected Ok, got Error: " ^ message)
  | Ok samples ->
      assert_equal 4 (List.length samples) ~printer:string_of_int ;
      let first = List.hd samples in
      assert_float_equal 0.0 first.Trainer.x ;
      assert_float_equal 1.0 first.Trainer.y

let test_csv_repo_dataset_trains _ =
  match Trainer.load_csv_samples "../data/y_eq_3x_minus_2.csv" with
  | Error message -> assert_failure ("csv load failed: " ^ message)
  | Ok samples -> (
      match
        Trainer.train_linear ~epochs:500 ~learning_rate:0.05
          Trainer.default_model samples
      with
      | Error message -> assert_failure ("training failed: " ^ message)
      | Ok (model, _) ->
          assert_float_equal ~eps:0.2 3.0 model.w ;
          assert_float_equal ~eps:0.2 (-2.0) model.b)

let test_csv_train_roundtrip _ =
  let path = write_temp_csv "0,1\n1,3\n2,5\n3,7\n" in
  let samples_result = Trainer.load_csv_samples path in
  Sys.remove path ;
  match samples_result with
  | Error message -> assert_failure ("csv load failed: " ^ message)
  | Ok samples -> (
      match
        Trainer.train_linear ~epochs:300 ~learning_rate:0.05
          Trainer.default_model samples
      with
      | Error message -> assert_failure ("training failed: " ^ message)
      | Ok (model, _) ->
          assert_float_equal ~eps:0.2 2.0 model.w ;
          assert_float_equal ~eps:0.2 1.0 model.b)

let suite =
  "trainer adam tests"
  >::: [
         "adam loss decreases" >:: test_adam_loss_decreases;
         "adam history length" >:: test_adam_history_length;
         "adam zero epochs" >:: test_adam_zero_epochs;
         "adam bad epochs" >:: test_adam_bad_epochs;
         "adam bad learning rate" >:: test_adam_bad_lr;
         "adam bad beta1" >:: test_adam_bad_beta1;
         "adam bad beta2" >:: test_adam_bad_beta2;
         "adam bad epsilon" >:: test_adam_bad_epsilon;
         "adam empty samples" >:: test_adam_empty_samples;
         "csv basic" >:: test_csv_basic;
         "csv skips blank lines" >:: test_csv_skips_blank_lines;
         "csv handles whitespace" >:: test_csv_handles_whitespace;
         "csv missing file" >:: test_csv_missing_file;
         "csv empty file" >:: test_csv_empty_file;
         "csv bad columns" >:: test_csv_bad_columns;
         "csv bad number" >:: test_csv_bad_number;
         "csv loads repo dataset" >:: test_csv_loads_repo_dataset;
         "csv repo dataset trains" >:: test_csv_repo_dataset_trains;
         "csv train roundtrip" >:: test_csv_train_roundtrip;
       ]

let () = run_test_tt_main suite
