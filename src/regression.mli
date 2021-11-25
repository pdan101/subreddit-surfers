(**This module contains that functions necessary to create training and
   test data*)

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

val train_test_model :
  int array list -> float -> regression -> float array

val get_training_data : Owl.Mat.mat -> float -> train_test_data

val create_matrix : int array list -> Owl.Mat.mat
