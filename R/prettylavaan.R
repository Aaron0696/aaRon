#' Pretty Printing of Lavaan Results
#'
#' Results from \code{lavaan} get messed up when \code{summary()} is called within a for loop within an RMarkdown document with the chunk option \code{results = "asis"}.
#' This function is an alternative to \code{summary()} that can print out a pretty output within such a loop.
#'
#' @param fitobj A lavaan fit object.
#' @param output_format Output format, either "asis" for use with \code{results = "asis"} chunks. Or "datatable" for general html output.
#' @param robust Defaults to \code{FALSE}, set to \code{TRUE} to print out scaled and robust fit indicators.
#' @param modindice.nrow Defaults to 10. Number of rows to display for modification indices.
#' @param ... Additional arguments passed onto \code{kable()} or \code{datatable} depending on \code{output_format}.
#'
#' @return
#' @export
#'
#' @examples
#' library(lavaan)
#' # the famous Holzinger and Swineford (1939) example
#' HS.model <-  "visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9"
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' prettylavaan(fit, output_format = "datatable")
#'
#' # using robust estimators
#' robustfit <- cfa(HS.model, data = HolzingerSwineford1939)
#' prettylavaan(robustfit, output_format = "datatable")
#'
#' # request for robust fit indices
#' prettylavaan(robustfit, output_format = "datatable", robust = TRUE)
prettylavaan <- function(fitobj, output_format = "asis", robust = FALSE, modindice.nrow = 10, ...)
{
  # 1. extract parameter estimates
  params <- lavaan::parameterEstimates(fitobj, standardized = TRUE)
  # formatting
  # change column names to sentence case
  names(params) <- toupper(names(params))
  # rearrange columns
  params <- params[,c("LHS","OP","RHS","STD.ALL","EST","SE","Z","PVALUE")]

  # 2. extract fit indices
  fitind <- data.frame(Values = round(lavaan::fitMeasures(fitobj,
                                                          fit.measures = c("npar",
                                                                           "chisq",
                                                                           "df",
                                                                           "pvalue",
                                                                           "cfi",
                                                                           "tli",
                                                                           "nnfi",
                                                                           "nfi",
                                                                           "rmsea",
                                                                           "srmr",
                                                                           "aic",
                                                                           "bic"
                                                          )),3))

  if(robust)
  {
    # get scaled fit indices
    fitind.scaled <- data.frame(Values = round(lavaan::fitMeasures(fitobj,
                                                                   fit.measures = c("npar",
                                                                                    "chisq.scaled",
                                                                                    "df.scaled",
                                                                                    "pvalue.scaled",
                                                                                    "cfi.scaled",
                                                                                    "tli.scaled",
                                                                                    "nnfi.scaled",
                                                                                    "nfi.scaled",
                                                                                    "rmsea.scaled",
                                                                                    "srmr_bentler"
                                                                   )),3))
    # get robust fit indices
    fitind.robust <- data.frame(Values = round(lavaan::fitMeasures(fitobj,
                                                                   fit.measures = c("npar",
                                                                                    "cfi.robust",
                                                                                    "tli.robust",
                                                                                    "nnfi.robust",
                                                                                    "nfi.robust",
                                                                                    "rmsea.robust"
                                                                   )),3))
    # add rownames as extra column for merging
    fitind$row <- row.names(fitind)
    fitind.scaled$row <- gsub(".scaled|_bentler", "",row.names(fitind.scaled))
    fitind.robust$row <- gsub(".robust", "",row.names(fitind.robust))
    # add index variable to be used to resort the dataframe after merge()
    # merge() sorts the dataframe even though sort = FALSE
    fitind$index <- 1:nrow(fitind)
    # merge into one dataframe to be displayed
    temp <- merge(fitind, fitind.scaled,
                  by = "row",
                  all.x = TRUE, all.y = FALSE,
                  sort = FALSE)
    temp <- merge(temp, fitind.robust,
                  by = "row",
                  all.x = TRUE, all.y = FALSE,
                  sort = FALSE)
    # save the merged dataframe as fitind.all
    # also remove the row column used for merging
    fitind.all <- temp[,-grep("row", names(temp))]
    row.names(fitind.all) <- temp$row
    # sort fitind.all by index and remove index column
    fitind.all <- fitind.all[order(fitind.all$index),-grep("index", names(fitind.all))]
    names(fitind.all) <- c("Naive", "Scaled", "Robust")
    # change NA to blanks
    fitind.all[is.na(fitind.all)] <- ""
  } else {
    fitind.all <- fitind
  }

  # formatting
  row.names(fitind.all) <- toupper(row.names(fitind.all))

  # 3. Modification Indices
  modind <- modificationIndices(fitobj, maximum.number = modindice.nrow, sort. = TRUE)
  # formatting
  # change column names to sentence case
  names(modind) <- toupper(names((modind)))

  if(output_format == "asis")
  {
    # print
    cat("\n\n**Converged**:", fitobj@Fit@converged, "\n\n")
    cat("**Iterations**:", fitobj@Fit@iterations, "\n\n")
    cat("**Original Sample Size**:", as.numeric(fitobj@Data@norig), "\n\n")
    cat("**Effective Sample Size**:", fitobj@SampleStats@ntotal, "\n\n")
    cat("***\n\n")
    cat("**Fit Indices**:\n\n")
    print(knitr::kable(fitind.all, digits = 2, align = "c", ...))
    cat("***\n\n")
    cat("\n\n**Parameter Estimates**:\n\n")
    print(knitr::kable(params, digits = 2, align = "c", ...))
    cat("\n\n**Modification Indices**:\n\n")
    print(knitr::kable(modind, digits = 2, align = "c", ...))
  }

  if(output_format == "datatable")
  {
    # print
    cat("Converged:", fitobj@Fit@converged, "\n")
    cat("Iterations:", fitobj@Fit@iterations, "\n")
    cat("Original Sample Size:", as.numeric(fitobj@Data@norig), "\n")
    cat("Effective Sample Size:", fitobj@SampleStats@ntotal)
    cat("\n\n")
    cat("Fit Indices:\n")
    print(fitind.all)
    cat("\n\n")
    # change numeric values to 3 DP
    modind[,-1:-3] <- data.frame(lapply(modind[,-1:-3], function(e){round(e,3)}))
    # return the modind datatable
    cat("Modification Indices:\n")
    print(modind)
    # change numeric values to 3 DP
    params[,-1:-3] <- data.frame(lapply(params[,-1:-3], function(e){round(e,3)}))
    # return the params datatable
    cat("\n\n")
    cat("Parameter Estimates:\n")
    return(DT::datatable(params, filter = "top", ...))
  }
}
