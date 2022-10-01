#' Convert Odds To Probabilities
#'
#' Convert odds to probabilities using the formula of p = odds/(1 + odds)
#'
#' @param p Probability value to be converted
#'
#' @return Return the odds
#' @export
#'
#' @examples
#' odd <- c(0.5,0.75,1)
#' odd2prob(odd)
odd2prob <- function(odd)
{
  prob <- odd/(1+odd)
  return(prob)
}
