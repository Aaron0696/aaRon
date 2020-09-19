#' Reverse Coding of Scales
#'
#'Implements reverse-coding of items using \code{factor()}. One advantage over \code{psych::reverse.code} is that input need not be numeric.
#'
#' @param vector A numeric, character or factor vector.
#' @param original_levels A character vector denoting the order of the original scale (lowest level on the left)
#'
#' @return Returns a factor vector of the same length as vector with reverse-coded responses, with levels corresponding to original_level
#' @export
#'
#' @examples
#' mydata <- data.frame(myscale = c("Disagree", "Neutral", "Agree", "Neutral","Agree"))
#' code.reverse(vector = mydata$myscale, original_levels = c("Disagree","Neutral","Agree"))
code.reverse <- function(vector, original_levels){
  # convert vector input to a factor with the original levels
  vector <- factor(vector, levels = original_levels)
  # reverse code values in the vector by flipping the order of the vector
  levels(vector) <- levels(vector)[length(levels(vector)):1]
  # return the levels to the original without changing values in the vector
  vector <- factor(vector, levels = original_levels)
  # return
  return(vector)
}
