#' My t-Test
#'
#' This function performs a one sample t-test on vectors of data.
#'
#' @param x Numeric vector input of data
#' @param alternative A character string specifying the alternative hypothesis.
#'   It only accepts \code{"two.sided"}, \code{"less"}, or
#'   \code{"greater"}.
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @keywords inference
#'
#' @return a list with elements \code{test_stat}, the numeric test statistics,
#'   \code{df}, the degrees of freedom, \code{alternative}, the value of the
#'   parameter \code{alternative}, and \code{p_val}, the numeric p-value.
#'
#' @examples
#' my_t.test(1:10, alternative = "two.sided", mu = 5)
#' my_t.test(3:1000, alternative = "less", mu = 100)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  if (alternative != "two.sided" && alternative != "less"
      && alternative != "greater") {
    stop("The 'alternative' parameter can only be 'two.sided', 'less', or 'greater'")
  }
  # calculate t-score
  t_score <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  # degree of freedom
  df <- length(x) - 1
  if (alternative == "less") {
    p_val <- pt(t_score, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(t_score, df, lower.tail = FALSE)
  } else {
    p_val <-  pt(abs(t_score), df, lower.tail = FALSE) * 2
  }

  # return result
  result <- list("test_stat" = t_score,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_val)
  return(result)
}
