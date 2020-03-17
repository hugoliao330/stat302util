#' My Fitting Linear Models
#'
#' \code{my_lm} is used to fit linear models.
#'
#' @param formula a formula class object, which is a symbolic description of
#'   the model to be fitted.
#' @param data an input data frame.
#'
#' @keywords inference
#'
#' @return A \code{table} with statistics for this linear model, invluding the
#'   \code{(Intercept)!} and columns for the \code{Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples
#' my_lm(formula = mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extract data of independent variables
  ind_v <- model.matrix(formula, data)
  # extract data of dependent variable
  dep_v <- model.response(model.frame(formula = formula, data = data))
  # estimated coefficients
  beta_hat <- solve(t(ind_v) %*% ind_v) %*% t(ind_v) %*% dep_v
  # degree of freedom
  df <- nrow(data) - length(beta_hat)

  # calculate expected values
  expected <- rep(0, nrow(ind_v))
  for (i in 1 : ncol(ind_v)) {
    expected <- expected + beta_hat[i] * ind_v[, i]
  }

  # calculate variance
  variance <- 0
  for (i in 1 : length(dep_v)) {
    variance <- variance + (dep_v[i] - expected[i]) ^ 2
  }
  variance <- variance / df

  # calculate standard error
  se <- sqrt(diag(variance * solve(t(ind_v) %*% ind_v)))
  # t-score
  t_score <- beta_hat / se
  # p-value
  p_val <- pt(abs(t_score), df, lower.tail = FALSE) * 2

  # return results as a data frame
  return(data.frame("Estimate" = beta_hat,
                    "Std. Error" = se,
                    "t value" = t_score,
                    "Pr(>|t|)" = p_val))
}
