#' Convert Factor To Numeric While Printing Levels
#'
#' @param factor_vector A vector that is a factor.
#'
#' @return
#' @export
#'
#' @examples
#' a <- factor(c("Agree","Disagree","Agree"))
#' fct2num(a)
fct2num <- function(factor_vector)
{
  # print out levels before conversion
  cat("Levels:", levels(factor_vector), "\n")
  # convert to numeric
  return(as.numeric(factor_vector))
}
