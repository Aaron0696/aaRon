#' Convert Factor To Numeric While Printing Levels
#'
#' @param factor_vector A vector that is a factor.
#'
#' @return
#' @export
#'
#' @examples
#' a <- factor(c("Agree","Disagree","Agree","Agree","Disagree"))
#' fct2num(a)
fct2num <- function(factor_vector)
{
  # check if the input is a vector
  if(is.factor(factor_vector))
  {
    # print out levels before conversion
    if(length(levels(factor_vector)) < 20)
    {
      cat("Levels:", levels(factor_vector), "\n")
    } else {
      cat("More than 15 levels, printing suppressed.\n")
    }
  } else {
    warning("Input is not a factor.")
  }
  # convert to numeric
  return(as.numeric(factor_vector))
}
