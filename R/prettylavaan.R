#' Pretty Printing of Lavaan Results
#'
#' Results from \code{lavaan} get messed up when \code{summary()} is called within a \code{for} loop within an R Markdown chunk with the option \code{results = "asis"}.
#' This function is an alternative to \code{summary()} that can print out a pretty output in such situations.
#'
#' @param fitobj A lavaan fit object.
#' @param output_format Output format, default is "asis" for use with \code{results = "asis"} chunks. Use "datatable" for an interactive parameter table. Or "kableExtra" to generate a list of kable tables that can be independently piped to kableExtra functions.
#' @param robust Defaults to \code{FALSE}, set to \code{TRUE} to print out scaled and robust fit indicators.
#' @param modindice.nrow Defaults to 10. Number of rows to display for modification indices.
#' @param param.type Defaults to only show path coefficients (~), factor loadings (=~) and covariances (~~). Use (|) for thresholds if WLS estimators were used.
#' @param dp Defaults to 3. Number of decimal points for all numeric values.
#' @param align Defaults to "c", which stands for centered. Adjusts the alignment of text within table cells. Works the same way as \code{kable()}.
#' @param multigroup Defaults to FALSE. Set to \code{TRUE} if a multiple-group analysis was conducted. Keeps the grouping column in \code{paramterEstimates()}.
#' @param show.modind Defaults to FALSE. Set to \code{TRUE} to print modification indices.
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
#' robustfit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM")
#' prettylavaan(robustfit, output_format = "datatable")
#'
#' # request for robust fit indices
#' prettylavaan(robustfit, output_format = "datatable", robust = TRUE)
#'
#' # multigroup
#' mfit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM", group = "sex")
#' prettylavaan(mfit, robust = TRUE, multigroup = TRUE)
#'
#' # for piping to kableExtra for further editing
#' # note: library(kableExtra) may mess up formatting of normal kable tables in the same Rmd document
#' # refer to https://github.com/haozhu233/kableExtra/issues/265
#' # format = "html" is required to work with kableExtra
#' library(kableExtra)
#' mylist <- prettylavaan(robustfit, output_format = "kableExtra", format = "html", robust = TRUE)
#' mylist$Param %>% kable_styling(font_size = 9)
prettylavaan <- function(fitobj,
                         output_format = "asis",
                         robust = FALSE,
                         modindice.nrow = 10,
                         param.type = c("=~","~~","~"),
                         dp = 3,
                         align = "c",
                         multigroup = FALSE,
                         show.modind = FALSE,
                         ...)
{
  # 1. extract parameter estimates
  params <- lavaan::parameterEstimates(fitobj, standardized = TRUE, output = "data.frame")
  # formatting
  # change column names to sentence case
  names(params) <- toupper(names(params))
  # if multigroup is TRUE, keep the group column
  if(multigroup)
  {
    # rearrange columns
    params <- params[,c("GROUP","LHS","OP","RHS","STD.ALL","EST","SE","Z","PVALUE")]
    params$GROUP <- factor(params$GROUP)
    levels(params$GROUP) <- fitobj@Data@group.label
  } else {
    # rearrange columns
    params <- params[,c("LHS","OP","RHS","STD.ALL","EST","SE","Z","PVALUE")]
  }

  # filter only the relevant parameter estimates as determined by param.type
  params <- params[params$OP %in% param.type ,]

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
                                                          )),dp))

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
                                                                   )),dp))
    # get robust fit indices
    fitind.robust <- data.frame(Values = round(lavaan::fitMeasures(fitobj,
                                                                   fit.measures = c("npar",
                                                                                    "cfi.robust",
                                                                                    "tli.robust",
                                                                                    "nnfi.robust",
                                                                                    "nfi.robust",
                                                                                    "rmsea.robust"
                                                                   )),dp))
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
    names(fitind.all) <- c("Naive", ".Scaled", ".Robust")
  } else {
    fitind.all <- fitind
  }

  # formatting
  fitind.all <- data.frame(t(fitind.all))
  names(fitind.all) <- toupper(names(fitind.all))

  # 3. Modification Indices
  modind <- modificationIndices(fitobj, maximum.number = modindice.nrow, sort. = TRUE)
  # formatting
  # change column names to sentence case
  names(modind) <- toupper(names((modind)))

  # 4. Estimator
  estimator <- fit@call[["estimator"]]
  if(is.null(estimator))
  {
    estimator <- fit@loglik[["estimator"]]
  }

  if(output_format == "asis")
  {
    # print
    cat("\n\n**Estimator**:", estimator, "\n\n")
    cat("**Converged**:", fitobj@Fit@converged, "\n\n")
    cat("**Iterations**:", fitobj@Fit@iterations, "\n\n")
    cat("**Original Sample Size**:", as.numeric(fitobj@Data@norig), "\n\n")
    cat("**Effective Sample Size**:", fitobj@SampleStats@ntotal, "\n\n")

    cat("**Fit Indices**:\n\n")
    print(knitr::kable(fitind.all, digits = dp, align = align, ...))
    cat("\n\n")
    cat("\n\n**Parameter Estimates**:\n\n")
    print(knitr::kable(params, digits = dp, align = align, ...))
    cat("\n\n")
    if(show.modind)
    {
      cat("**Modification Indices**:\n\n")
      print(knitr::kable(modind, digits = dp, align = align, ...))
    }
  }

  if(output_format == "kableExtra")
  {
    # return each kable table as an element in the list for piping to kableExtra
    return(list(Fit = knitr::kable(fitind.all, digits = dp, align = align, ...),
                Param = knitr::kable(params, digits = dp, align = align, ...),
                ModInd = knitr::kable(modind, digits = dp, align = align, ...)))
    if(show.modind)
    {
      return(list(Fit = knitr::kable(fitind.all, digits = dp, align = align, ...),
                  Param = knitr::kable(params, digits = dp, align = align, ...),
                  ModInd = knitr::kable(modind, digits = dp, align = align, ...)))
    } else
    {
      return(list(Fit = knitr::kable(fitind.all, digits = dp, align = align, ...),
                  Param = knitr::kable(params, digits = dp, align = align, ...)))
    }
  }

  if(output_format == "datatable")
  {
    # print
    cat("Estimator:", estimator, "\n")
    cat("Converged:", fitobj@Fit@converged, "\n")
    cat("Iterations:", fitobj@Fit@iterations, "\n")
    cat("Original Sample Size:", as.numeric(fitobj@Data@norig), "\n")
    cat("Effective Sample Size:", fitobj@SampleStats@ntotal)
    cat("\n\n")
    cat("Fit Indices:\n")
    print(fitind.all)
    cat("\n")
    # change numeric values to 3 DP
    modind[,-1:-3] <- data.frame(lapply(modind[,-1:-3], function(e){round(e,dp)}))
    if(show.modind)
    {
      # return the modind datatable
      cat("Modification Indices:\n")
      print(modind)
    }
    # change numeric values to 3 DP
    params[,-1:-3] <- data.frame(lapply(params[,-1:-3], function(e){round(e,dp)}))
    # return the params datatable
    cat("\n\n")
    cat("Parameter Estimates:\n")
    return(DT::datatable(params, filter = "top", ...))
  }
}
