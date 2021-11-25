(**This module contains that functions necessary to create training and
   test data*)

type regression =
  | Ridge
  | LASSO
  | Logistic
  | SVM
  | OLS

val train_test_model :
  int array list -> float -> regression -> float list
