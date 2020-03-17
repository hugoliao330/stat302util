#' k-Nearest Neighbors Cross-Validation
#'
#'
# Input: train, input data frame
# Input: cl, true class value of the training data
# Input: k_nn, integer representing the number of neighbors
# Input: k_cv, integer representing the number of folds
# Output: a list with objects class - a vector of the predicted class y_hat_i
#         for all obsevations, and cv_err - a numeric with the cross validation
#         misclassification error
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame(train, "fold" = fold)
  cl_folded <- data.frame(cl, "fold" = fold)
  misclassification_rate <- rep(NA, length = k_cv)
  for (i in 1:k_cv) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    class <- cl_folded %>% filter(fold != i)
    #print(paste("iteration ", i, "; test data", data_test))
    prediction <- knn(train = data_train[, -5], test = data_test[, -5], cl = class[, 1], k = k_nn)
    misclassification_rate[i] <- mean(prediction != class[, 1])
  }
  class <- knn(train, train, cl, k_nn)
  return(list("class" = class,
              "cv_err" = mean(misclassification_rate)))
}
