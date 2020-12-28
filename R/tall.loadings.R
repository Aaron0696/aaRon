#' Tall/Long Factor Loading Matrices
#'
#' Factor loadings from factor analysis functions such as \code{psych::fa()} produce factor loading matrices in wide-format.
#' Tall versions of factor loading matrices allow us to better inspect the items and factor loadings of each individual factor.
#' This is a function created to automate the conversion to tall format, eliminating factor loadings below a certain threshold and sorting the output by factors or items.
#'
#' @param structure A matrix/dataframe of factor loadings in wide-format.
#' @param cut Defaults to 0.2. The minimum cut-off for factor loadings. Factor loadings below this value are removed.
#' @param dp Defaults to 2. Number of decimal points for all numeric values.
#' @param sortby Defaults to "FACTOR". Sorts the output by factors, change value to "ITEM" to sort by items.
#'
#' @return
#' @export
#'
#' @examples
#' library(psych)
#' myefa <- fa(Harman74.cor$cov, 4, fm = "wls")
#' tall.loadings(myefa$Structure)
#' tall.loadings(myefa$loadings)
tall.loadings <- function(structure,
                          cut = 0.2,
                          dp = 2,
                          sortby = "FACTOR")
{
  # get dimensions of structure
  dimen <- dim(structure)
  # get the loadings as a dataframe
  loadings <- data.frame(as.matrix(structure[1:dimen[1],]))
  # remove values below threshold
  loadings[loadings < cut] <- NA
  # add question column
  loadings$Item <- row.names(loadings)
  # pivot to long
  loadings.long <- tidyr::pivot_longer(loadings, cols = -Item)
  loadings.long$value <- round(loadings.long$value, dp)
  # remove NAs from long
  loadings.long <- na.omit(loadings.long)
  # rename
  names(loadings.long) <- toupper(names(loadings.long))
  # sort by...
  if(sortby == "FACTOR")
  {
    return(loadings.long[order(loadings.long$NAME),])
  } else if(sortby == "ITEM")
  {
    return(loadings.long[order(loadings.long$ITEM),])
  } else {
    stop("Error!")
  }
}
