#' aggregateScale()
#'
#' Calculates the mean or sum of each row of a dataframe using \code{rowMeans()}
#' while applying a condition to only aggregate rows where missingess is below a certain threshold.
#'
#' @param dataframe A dataframe containing numeric scores to be aggregated. Each row should correspond to one observation. Each column is one variable.
#' @param aggregate Defaults to \code{"SUM"}. Calculates sum scores, use \code{"MEAN"} for mean scores.
#' @param NA.threshold The maximum proportion of missingness allowed for one observation/row before the aggregate score is coerced to NA. Defaults to 0.5, which means that rows with NA in more than half of its observation will have an NA aggregate score.
#' @param na.rm Whether missing values should be ignored in the calculation for the aggregate scores. Defaults to \code{TRUE}. Does not affect the calculation of proportion of missing data in \code{NA.threshold}.
#'
#' @return
#' @export
#'
#' @examples
#' mydata <- data.frame(a = c(1:8,NA,NA), b = 1:10, c = rep(NA,10))
#' aggregate.scale(mydata)
#' aggregate.scale(mydata, NA.threshold = 0.4)
aggregateScale <- function(dataframe,aggregate = "SUM",NA.threshold = 0.5,na.rm = TRUE)
{
  # calculate rowMeans or rowSums
  if(aggregate == "SUM")
  {
    agg <- rowSums(dataframe, na.rm = na.rm)
  }
  if(aggregate == "MEAN")
  {
    agg <- rowMeans(dataframe, na.rm = na.rm)
  }
  # calculate proportion of missingness for each row as a vector
  prop.missing <- rowMeans(is.na(dataframe))
  # if the row has proportion of missingness above NA.threshold, make the aggregate score NA
  agg[prop.missing > NA.threshold] <- NA
  return(agg)
}
