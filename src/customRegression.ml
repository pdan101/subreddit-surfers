open Owl
module MX = Owl.Dense.Matrix.D

type regression =
  | Ridge
  | LASSO
  | Logistic
  | SVM
  | OLS

type train_test_data = {
  features_training : Owl.Mat.mat;
  features_test : Owl.Mat.mat;
  output_training : Owl.Mat.mat;
  output_testing : Owl.Mat.mat;
}

let get_model regression_type features output =
  match regression_type with
  | OLS -> Regression.D.ols ~i:true features output
  | Ridge -> Regression.D.ridge ~i:true features output
  | LASSO -> Regression.D.lasso ~i:true features output
  | Logistic -> Regression.D.logistic ~i:true features output
  | SVM -> Regression.D.svm ~i:true features output

let get_num_posts (rows, cols) = rows

let get_num_features (rows, cols) = cols

let create_matrix data =
  let floats =
    List.map (fun x -> Array.map (fun x -> float_of_int x) x) data
  in
  let array = Array.of_list floats in
  Mat.of_arrays array

let get_training_data matrix percent_training =
  let shape = Mat.shape matrix in
  let num_posts = get_num_posts shape in
  let num_features = get_num_features shape in
  let rows_training =
    int_of_float (float_of_int num_posts *. percent_training)
  in
  let num_cols = num_features in
  let features_training =
    Mat.get_slice [ [ 0; rows_training ]; [ 0; num_cols - 2 ] ] matrix
  in
  let output_training =
    Mat.get_slice [ [ 0; rows_training ]; [ num_cols - 1 ] ] matrix
  in
  let features_test =
    Mat.get_slice [ [ rows_training; -1 ]; [ 0; num_cols - 2 ] ] matrix
  in
  let output_testing =
    Mat.get_slice [ [ rows_training; -1 ]; [ num_cols - 1 ] ] matrix
  in
  { features_training; features_test; output_training; output_testing }

let calc_upvotes features_test weights =
  let posts = Mat.to_arrays features_test in
  let with_weights =
    Array.map
      (fun encoded_post ->
        Array.mapi
          (fun index word -> word *. (weights.(index) *. 500.))
          encoded_post)
      posts
  in
  let without_last_weight =
    Array.map
      (fun post_array ->
        Array.fold_right
          (fun element init -> element +. init)
          post_array 0.0)
      with_weights
  in
  Array.map
    (fun weight -> weight +. weights.(Array.length weights - 1))
    without_last_weight

let calc_error predicted_upvotes actual_upvotes =
  let actual_upvotes = Mat.to_array actual_upvotes in
  let squared_sum_array =
    Array.mapi
      (fun index element -> (element -. actual_upvotes.(index)) ** 2.0)
      predicted_upvotes
  in
  let squared_sum =
    Array.fold_right
      (fun element init -> element +. init)
      squared_sum_array 0.0
  in
  squared_sum /. float_of_int (Array.length predicted_upvotes)

let train_test_model data percent_training regression_type =
  let matrix = create_matrix data in
  let train_test_data = get_training_data matrix 0.75 in
  let weights =
    get_model regression_type train_test_data.features_test
      train_test_data.output_testing
  in
  let dense_matrix = MX.(weights.(0) @= weights.(1)) in
  Owl_dense_matrix.D.to_array dense_matrix

let get_absolute_max array =
  let current_max = ref 1 in
  for i = 0 to Array.length array - 1 do
    if array.(i) < 0 then
      if array.(i) * -1 > !current_max then
        current_max := array.(i) * -1
      else ()
    else if array.(i) > !current_max then current_max := array.(i)
    else ()
  done;
  !current_max

let generate_spaces num =
  let str = ref "" in
  for i = 0 to num do
    str := !str ^ " "
  done;
  !str

let create_column (predicted_vote : float) (actual_vote : float) =
  let col = " |" in
  if predicted_vote < actual_vote then
    let pred_spaces = generate_spaces (int_of_float predicted_vote) in
    let col = col ^ pred_spaces ^ "⊡" in
    let actual_spaces =
      generate_spaces (int_of_float (actual_vote -. predicted_vote))
    in
    let extra_spaces =
      generate_spaces (int_of_float (50.0 -. actual_vote))
    in
    let col = col ^ actual_spaces ^ "⊙" ^ extra_spaces in
    col
  else
    let actual_spaces = generate_spaces (int_of_float actual_vote) in
    let col = col ^ actual_spaces ^ "⊙" in
    let pred_spaces =
      generate_spaces (int_of_float (predicted_vote -. actual_vote))
    in
    let extra_spaces =
      generate_spaces (int_of_float (50.0 -. predicted_vote))
    in
    let col = col ^ pred_spaces ^ "⊡" ^ extra_spaces in
    col

let graph_results predicted_upvotes (actual_upvotes : float array) =
  let actual_upvotes_int =
    Array.map (fun e -> int_of_float e) actual_upvotes
  in
  let max_predicted = get_absolute_max predicted_upvotes in
  let max_actual = get_absolute_max actual_upvotes_int in
  let max =
    if max_predicted <= max_actual then max_actual else max_predicted
  in
  let scaling_factor = 50.0 /. float_of_int max in
  let columns =
    [ "  ―――――――――――――――――――――――――――――――――――――――――――――――˃" ]
  in

  let predicted_upvotes =
    Array.map (fun e -> float_of_int e) predicted_upvotes
  in
  print_newline ();
  print_newline ();
  print_string "     ⊙- Actual Upvotes, ⊡- Predicted Upvotes";
  print_newline ();

  let additional_cols =
    Array.mapi
      (fun i e ->
        create_column (scaling_factor *. e)
          (scaling_factor *. actual_upvotes.(i)))
      predicted_upvotes
  in
  let all_columns = columns @ Array.to_list additional_cols in
  List.iter
    (fun i ->
      print_string i;
      print_newline ())
    all_columns;
  print_string " |";
  print_newline ();
  print_string " ˅";
  print_newline ();
  print_newline ()
