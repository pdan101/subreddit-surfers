open Owl
module MX = Owl.Dense.Matrix.D

type regression =
  | Ridge
  | LASSO
  | Logistic
  | SVM
  | OLS

let get_model regression_type features output =
  match regression_type with
  | OLS -> Regression.D.ols ~i:true features output
  | Ridge -> Regression.D.ridge ~i:true features output
  | LASSO -> Regression.D.lasso ~i:true features output
  | Logistic -> Regression.D.logistic ~i:true features output
  | SVM -> Regression.D.svm ~i:true features output

let get_num_posts (rows, cols) = rows

let get_num_features (rows, cols) = cols

let train_test_model data percent_training regression_type =
  let floats =
    List.map (fun x -> Array.map (fun x -> float_of_int x) x) data
  in
  let array = Array.of_list floats in

  let matrix = Mat.of_arrays array in

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
  let output_test =
    Mat.get_slice [ [ rows_training; -1 ]; [ num_cols - 1 ] ] matrix
  in

  let weights =
    get_model regression_type features_training output_training
  in
  let dense_matrix = MX.(weights.(0) @= weights.(1)) in
  Array.to_list (Owl_dense_matrix.D.to_array dense_matrix)

(* let calc_upvotes test_features test_output weights = let
   shape_features = Mat.shape test_features in let num_posts =
   get_num_posts shape_features in let num_features = get_num_features
   shape_features in let num_upvotes = ref 0.0 in for post_num = 0 to
   num_posts - 1 do for feature_num = 0 to num_features - 1 do
   num_upvotes := !num_upvotes +. weights.(feature_num) *. float_of_int
   test_features.(feature_num) done done *)
