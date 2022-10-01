#' Identify extreme values based on percentiles
#'
#' Identify high improbable values using a percentile threshold
#'
#' @param vec Vector
#' @param threshold Threshold in percentile expressed between 1 to 100, defaults to 99.5
#'
#' @return Return a logical vector indicating TRUE if value is above the percentile threshold
#' @export
#'
#' @examples
#' vec <- c(1:10, 10000)
#' extremes(vec)
#' vec[extremes(vec)]
#' data.frame(vec = vec) %>% filter(!extremes(vec))
#' data.frame(vec = vec) %>% filter(extremes(vec))
extremes <- function(vec, threshold = 99.5)
{
  # calculate percentile
  perc_cut <- quantile(vec, threshold/100)

  return(ifelse(vec > perc_cut, TRUE, FALSE))
}
