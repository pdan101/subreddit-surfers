open Owl
module MX = Owl.Dense.Matrix.D

let format_data data =
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
  let weights = Regression.D.ols ~i:true features output in
  let dense_matrix = MX.(weights.(0) @= weights.(1)) in
  Array.to_list (Owl_dense_matrix.D.to_array dense_matrix)
