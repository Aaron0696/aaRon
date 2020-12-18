#' Calculate KMO and Bartlett' Tests of Sphericity
#'
#' A function that automates and simplifies the code to conduct the Kaiser-Meyer-Olkin (KMO) Test for Sampling Adequacy
#' and Bartlett's Test of Sphericity. These tests determine the suitability of the data for Factor Analysis.
#' The code for this function was taken from the notes created by Professor Mike Cheung's statistical module. Visit his website at http://mikewlcheung.github.io/.
#'
#' @param data A dataframe containing only the numeric variables to be included in the factor analysis.
#'
#' @return
#' @export
#'
#' @examples
#' library(psych)
#' library(lavaan)
#' efa.diag(HolzingerSwineford1939[,7:15])
efa.diag <- function(data)
{
  # number of variables
  novar <- ncol(data)
  # df
  df <- novar*(novar-1)/2
  # transform into matrix then calculate
  test <- rela::paf(as.matrix(data))
  cat("KMO: ", test["KMO"][[1]], "\n")
  cat("Bartlett's Test Of Sphericity: ",
      test["Bartlett"][[1]],
      ", df = ",
      df,
      ", pvalue = ",
      pchisq(test["Bartlett"][[1]],
             df = df,
             lower.tail = FALSE))
}
