#' Pretty Printing of psych::alpha() Results
#'
#' Results from \code{psych::alpha()} get messed up when \code{alpha()} is called within a \code{for} loop within an R Markdown document with the chunk option \code{results = "asis"}.
#' This function is an alternative to \code{alpha()} that can print out a pretty output in such situations.
#'
#' @param alphaobj Summary object from executing psych::alpha().
#' @param ... Additional arguments passed onto \code{kable()}.
#' @param dp Defaults to 3. Number of decimal points for all numeric values.
#' @param align Defaults to "c", which stands for centered. Adjusts the alignment of text within table cells. Works the same way as \code{kable()}.
#'
#' @return
#' @export
#'
#' @examples
#' myalpha <- alpha(HolzingerSwineford1939[,7:15])
#' prettyalpha(myalpha)
prettyalpha <- function(alphaobj, dp = 2, align = "c", ...)
{
  # for each of these dataframes within alphaobj, total, alpha.drop and item.stats
  # use kable() to print out a nice table for each
  cat("\n\n")
  cat("**Cronbach Alpha**:")
  cat("\n\n")
  names(alphaobj$total) <- toupper(names(alphaobj$total))
  print(knitr::kable(alphaobj$total,align = align,digits = dp,...))
  cat("\n\n")
  cat("**Alpha Values If Certain Items Were Dropped**:")
  cat("\n\n")
  names(alphaobj$alpha.drop) <- toupper(names(alphaobj$alpha.drop))
  print(knitr::kable(alphaobj$alpha.drop, row.names = TRUE, align = align, digits = dp, ...))
  cat("\n\n")
  cat("**Item-Level Statistics**:")
  cat("\n\n")
  names(alphaobj$item.stats) <- toupper(names(alphaobj$item.stats))
  print(knitr::kable(alphaobj$item.stats, row.names = TRUE, align = align, digits = dp, ...))
  cat("\n\n")
}
