(**This module contains that functions necessary to create training and
   test data*)

type regression =
  | Ridge
  | LASSO
  | Logistic
  | SVM
  | OLS

val format_data : int array list -> regression -> float list
