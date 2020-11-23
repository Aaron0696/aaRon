#' Applying \code{aaRon.css} to Rmarkdown Documents
#'
#' Function to apply cascading stylesheets from \code{aaRon} to any Rmarkdown document.
#'
#' @return
#' @export
#'
#' @examples
#' aaRon::use.style() # include into any R code chunk
use.style <- function()
{
  # path to css file
  mypath <- system.file("stylesheets","aaRon.css", package = "aaRon")
  # include CSS in html output
  htmltools::includeCSS(mypath)
}
