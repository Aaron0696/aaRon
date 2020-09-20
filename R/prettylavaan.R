#' Pretty Printing of Lavaan Results
#'
#' Results from \code{lavaan} get messed up when \code{summary()} is called within a for loop within an RMarkdown document with the chunk option \code{results = "asis"}.
#' This function is an alternative to \code{summary()} that can print out a pretty output within such a loop.
#'
#' @param fitobj A lavaan fit object.
#' @param output_format Output format, either "asis" for use with \code{results = "asis"} chunks. Or "datatable" for general html output.
#'
#' @return
#' @export
#'
#' @examples
#' library(lavaan)
#' # The famous Holzinger and Swineford (1939) example
#' HS.model <-  "visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9"
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' prettylavaan(fit, output_format = "datatable")
prettylavaan <- function(fitobj, output_format = "asis")
{
  # extract parameter estimates
  params <- lavaan::parameterEstimates(fitobj, standardized = TRUE)
  # formatting
  # change column names to sentence case
  names(params) <- toupper(names(params))
  # rearrange columns
  params <- params[,c("LHS","OP","RHS","STD.ALL","EST","SE","Z","PVALUE")]

  # extract fit indices
  fitind <- data.frame(Values = round(lavaan::fitMeasures(fitobj,
                                                          fit.measures = c("chisq","df","pvalue","cfi",
                                                                           "tli","rmsea","srmr","aic","bic")),2))
  # formatting
  row.names(fitind) <- toupper(row.names(fitind))
  if(output_format == "asis")
  {
    cat("**Fit Indices**:\n\n")
    print(knitr::kable(fitind))
    cat("\n\n**Parameter Estimates**:\n\n")
    print(knitr::kable(params, digits = 2))
  }

  if(output_format == "datatable")
  {
    # create text output for fit indices
    text <- paste0("Fit Indices:\nCHISQ = ",
                   fitind$Values[1], ", df = ",
                   fitind$Values[2],", p-value = ",
                   fitind$Values[3], "\nCFI = ",
                   fitind$Values[4], "\nTLI = ",
                   fitind$Values[5], "\nRMSEA = ",
                   fitind$Values[6], "\nSRMR = ",
                   fitind$Values[7], "\nAIC = ",
                   fitind$Values[8], "\nBIC = ",
                   fitind$Values[9])
    # print
    cat(text)
    # change numeric values to 3 DP
    params[,-1:-3] <- data.frame(lapply(params[,-1:-3], function(e){round(e,3)}))
    # return the datatable
    return(DT::datatable(params))
  }
}
