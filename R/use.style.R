#' Applying \code{aaRon.css} to Rmarkdown Documents
#'
#' Function to apply cascading stylesheets from \code{aaRon} to any Rmarkdown document. Also accessible through the templates provided in the package.
#'
#' @param mode Defaults to "work", can take the option of "casual". Switches between CSS themes for work and casual use. The casual theme is slightly more flashy.
#'
#'
#' @return
#' @export
#'
#' @examples
#' aaRon::use.style() # include into any R code chunk
#' aaRon::use.style(mode = "casual")
use.style <- function(mode = "work")
{
  # path to css file
  if(mode == "work")
  {
    mypath <- system.file("stylesheets","aaRon.css", package = "aaRon")
  } else {
    mypath <- system.file("stylesheets","aaRon.casual.css", package = "aaRon")
  }

  # include CSS in html output
  htmltools::includeCSS(mypath)
}
