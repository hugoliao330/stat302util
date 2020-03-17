#' k-Nearest Neighbors Cross-Validation Classification
#'
#' k-nearest neighbor cross-validatory classification from training set.
#'
#' @param train input data frame.
#' @param cl true class value of your training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#'
#' @keywords inference prediction
#'
#' @return a list with objects \code{class}, a vector of the predicted class
#'   for all observations, and \code{cv_err}, a numeric with the
#'   cross-validation misclassification error.
#' @import class magrittr
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame(train, "fold" = fold)
  cl_folded <- data.frame(cl, "fold" = fold)
  misclassification_rate <- rep(NA, length = k_cv)
  for (i in 1:k_cv) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    class <- cl_folded %>% filter(fold != i)
    t_class <- cl_folded %>% filter(fold == i)
    prediction <- knn(train = data_train[,-ncol(data_train)],
                      test = data_test[,-ncol(data_train)],
                      cl = class[,1], k = k_nn)
    misclassification_rate[i] <- mean(prediction != t_class[, 1])
  }
  class <- knn(train, train, cl, k_nn)
  return(list("class" = class,
              "cv_err" = mean(misclassification_rate)))
}
