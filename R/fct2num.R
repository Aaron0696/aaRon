#' fct2num()
#'
#' Converts a factor to a numeric vector while adjusting for the starting value of the numeric vector.
#' Useful for converting from categorical responses (e.g. "Agree","Disagree") to numeric (1,2).
#'
#' @param factor_vector A vector that is a factor.
#' @param start Defaults to 1. The starting value of the resultant numeric vector. The value inputted for \code{start} will be the lowest numeric value for the lowest factor level.
#'
#' @return
#' @export
#'
#' @examples
#' a <- factor(c("Agree","Disagree","Agree","Agree","Disagree"))
#' fct2num(a)
fct2num <- function(factor_vector, start = 1)
{
  # check if the input is a vector
  if(is.factor(factor_vector))
  {
    # print out levels before conversion
    if(length(levels(factor_vector)) < 20)
    {
      message("Levels: ", paste0(levels(factor_vector), collapse = ", "), "\n")
    } else {
      message("More than 15 levels, printing suppressed.\n")
    }
  } else {
    warning("Input is not a factor.")
  }
  # convert to numeric
  mynum <- as.numeric(factor_vector)
  # adjust to starting value inputted
  mynum <- mynum - (1 - start)
  return(mynum)
}

