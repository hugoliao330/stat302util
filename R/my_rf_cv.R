#' My Random Forest Cross-Validation
#'
#' This will predict an output using the covariates within the training data
#'   \code{train}.
#'
#' @param trainx input data frame containing columns of predictor variables.
#' @param trainy response vector with length equal to number of rows of
#'   \code{trainx}
#' @param k number of cross-validation folds.
#'
#' @keywords inference prediction.
#'
#' @return a numeric with the cross-validation error.
#'
#' @import class randomForest
#'
#' @export
my_rf_cv <- function(trainx, trainy, k) {
  fold <- sample(rep(1:k, length = nrow(trainx)))
  df <- data.frame(trainy, trainx, "fold" = fold)
  mse <- rep(NA, length = k)
  if (ncol(trainx) > 1) {
    fmla <- as.formula(paste(colnames(trainy),
                             "~",
                             paste(colnames(trainx), collapse = "+")))
  } else {
    fmla <- as.formula(paste(colnames(trainy), "~", colnames(trainx)))
  }

  for (i in 1:k) {
    data_train <- df %>% filter(fold != i)
    data_test <- df %>% filter(fold == i)

    model <- randomForest(fmla, data = data_train, ntree = 100)
    predictions <- predict(model, data_test[, -1])
    mse[i] <- sum((data_test[, 1] - predictions) ^ 2) / nrow(data_test)
  }

  return(mean(mse))
}
