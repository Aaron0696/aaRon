#' Convert Probabilities To Odds
#'
#' Convert probabilities to odds using the formula of odds = p/(1-p)
#'
#' @param p Probability value to be converted
#'
#' @return Return the odds
#' @export
#'
#' @examples
#' p <- c(0.3,0.4,0.5)
#' prob2odd(p)
prob2odd <- function(p)
{
  odd <- p/(1-p)
  return(odd)
}
