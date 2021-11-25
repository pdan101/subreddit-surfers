open Owl

let format_data data =
  let matrix = Mat.of_arrays data in
  let features = Mat.get_slice [ []; [ 0; 1 ] ] matrix in
  let output = Mat.get_slice [ []; [ 2 ] ] matrix in
  Regression.D.ols ~i:true features output
