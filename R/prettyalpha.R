#' Pretty Printing of psych::alpha() Results
#'
#' Results from \code{psych::alpha()} get messed up when \code{alpha()} is called within a for loop within an RMarkdown document with the chunk option \code{results = "asis"}.
#' This function is an alternative to \code{alpha()} that can print out a pretty output within such a loop.
#'
#' @param alphaobj Summary object from executing psych::alpha().
#'
#' @return
#' @export
#'
#' @examples To be added...
prettyalpha <- function(alphaobj)
{
  # for each of these dataframes within alphaobj, total, alpha.drop and item.stats
  # use kable() to print out a nice table for each
  cat("\n\n")
  cat("**Cronbach Alpha**:")
  cat("\n\n")
  names(alphaobj$total) <- toupper(names(alphaobj$total))
  print(knitr::kable(alphaobj$total))
  cat("\n\n")
  cat("**Alpha Values If Certain Items Were Dropped**:")
  cat("\n\n")
  names(alphaobj$alpha.drop) <- toupper(names(alphaobj$alpha.drop))
  print(knitr::kable(alphaobj$alpha.drop, row.names = FALSE))
  cat("\n\n")
  cat("**Item-Level Statistics**:")
  cat("\n\n")
  names(alphaobj$item.stats) <- toupper(names(alphaobj$item.stats))
  print(knitr::kable(alphaobj$item.stats, row.names = FALSE))
  cat("\n\n")
}
