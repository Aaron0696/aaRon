#' Automatically Classify A Vector As Numeric/Factor/Character
#'
#' Takes in a vector as input, coerces the vector into a specific class using \code{as.numeric()}, \code{factor()} or \code{as.character()}. Coerced vector is returned as output.
#' The classification of the class is based on the following chain of logic:
#' \itemize{
#'   \item If the vector contains elements with non-numeric characters and have less than \code{X} unique values, classify as factor.
#'   \item If the vector contains elements with non-numeric characters and have more than \code{X} unique values, classify as character.
#'   \item If the vector contains elements with only numeric characters and have more than \code{X} unique values, classify as numeric.
#'   \item If the vector contains elements with only numeric characters and have less than \code{X} unique values, classify as factor.
#' }
#'
#' @param vector A vector to be classified.
#' @param unique.thres.fac The maximum number of unique categories for a vector to be classified as a factor, \code{X} in the description takes this value. Defaults to 20.
#'
#' @return
#' @export
#'
#' @examples
#' # return numeric
#' auto.class(1:100)
#'
#' # return factor
#' auto.class(rep(1:7,10))
#'
#' # return character
#' auto.class(c(1:100,"hello"))
#'
#' # return factor
#' auto.class(c("Strongly Agree","Agree","Disagree","Strongly Disagree"))
#'
#' # use with lapply to apply across entire dataframe
#' mydata <- data.frame(A = 1:100, B = c(1:99,"hello"))
#' mydata <- data.frame(lapply(mydata, FUN = auto.class))
#' str(mydata)
auto.class <- function(vector, unique.thres.fac = 20)
{
  # if input is a character, transform all "" to NAs
  if(is.character(vector))
  {
    vector[vector == ""] <- NA
  }

  # count number of NAs if vector is coerced to numeric
  NA.ifnumeric <- suppressWarnings(sum(is.na(as.numeric(vector))))
  # original number of NAs
  NA.original <- sum(is.na(vector))

  # if coercing the vector to numeric produces NAs...
  if(NA.ifnumeric > NA.original)
  {
    # ... and there are less than x unique values
    if(length(unique(vector)) < unique.thres.fac)
    {
      # then become a factor
      return(factor(vector))
    } else {
      # else, become a character
      return(as.character(vector))
    }
  } else { # if coercing the vector to numeric produces no NAs...
    # ... and there are less than x unique values
    if(length(unique(vector)) < unique.thres.fac)
    {
      # become a factor
      return(factor(vector))
    } else {
      # else, become a numeric
      return(as.numeric(vector))
    }
  }
}
