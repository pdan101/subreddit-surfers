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
          (fun index word -> word *. weights.(index))
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
  let formatted_weights = Owl_dense_matrix.D.to_array dense_matrix in
  let predicted_upvotes =
    calc_upvotes train_test_data.features_test formatted_weights
  in
  calc_error predicted_upvotes train_test_data.output_testing
