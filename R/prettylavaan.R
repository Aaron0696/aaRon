#' Pretty Printing of Lavaan Results
#'
#' Results from \code{lavaan} get messed up when \code{summary()} is called within a for loop within an RMarkdown document with the chunk option \code{results = "asis"}.
#' This function is an alternative to \code{summary()} that can print out a pretty output within such a loop.
#'
#' @param fitobj A lavaan fit object.
#' @param output_format Output format, currently only "asis" is implemented.
#'
#' @return
#' @export
#'
#' @examples TODO
prettylavaan <- function(fitobj, output_format = "asis")
{
  # extract parameter estimates
  params <- lavaan::parameterEstimates(fitobj, standardized = TRUE)
    # formatting
  # change column names to sentence case
  names(params) <- toupper(names(params))
  # keep only certain columns
  params <- params[,grep("LHS|OP|RHS|EST|SE|^Z$|PVALUE|CI|STD.ALL",names(params))]

  # extract fit indices
  fitind <- data.frame(Values = round(fitMeasures(fitobj,
                                                  fit.measures = c("chisq","df","pvalue","cfi",
                                                                   "tli","rmsea","srmr","aic","bic")),2))
    # formatting
  row.names(fitind) <- toupper(row.names(fitind))
  if(output_format == "asis")
  {
    cat("Fit Indices:\n\n")
    print(knitr::kable(fitind))
    cat("\n\nParamter Estimates:\n\n")
    print(knitr::kable(params, digits = 2))
  }
}
