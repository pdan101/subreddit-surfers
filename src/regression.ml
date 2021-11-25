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

let format_data data regression_type =
  let floats =
    List.map (fun x -> Array.map (fun x -> float_of_int x) x) data
  in
  let array = Array.of_list floats in

  let matrix = Mat.of_arrays array in

  let features =
    Mat.get_slice [ []; [ 0; Array.length array.(0) - 2 ] ] matrix
  in
  let output =
    Mat.get_slice [ []; [ Array.length array.(0) - 1 ] ] matrix
  in
  let weights = get_model regression_type features output in
  let dense_matrix = MX.(weights.(0) @= weights.(1)) in
  Array.to_list (Owl_dense_matrix.D.to_array dense_matrix)
