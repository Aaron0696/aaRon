#' VLookup In R Using \code{merge()}
#'
#' Implementing code\{vlookup} in R using \code{merge()}.
#'
#' @param data WIP
#' @param lookup_table WIP
#' @param data_col WIP
#' @param lookup_col WIP
#' @param lookup_value WIP
#'
#' @return
#' @export
#'
#' @examples
#' mydata <- data.frame(Type = rep(1:3,5), Rand = 4:6)
#' mytable <- data.frame(Type = 1:3, Magnitude = c("Low","Medium","High"))
#' vlookup(data = mydata,
#'         lookup_table = mytable,
#'         data_col = "Type",
#'         lookup_col = "Type",
#'         lookup_value = "Magnitude")
vlookup <- function(data,lookup_table,data_col,lookup_col,lookup_value)
{
  # create an index such that we can resort the dataframe back into order after merge()
  # o571 is just a random column name... so that there will be no duplicate columns
  data$o571 <-1:nrow(data)
  out <- merge(x = data,y = lookup_table[,c(lookup_col,lookup_value)],
        by.x = data_col, by.y = lookup_col,
        all.x = TRUE)
  # sort
  out <- out[order(out$o571),]
  # remove index column
  out <- out[,-grep("^o571$",names(out))]
  return(out)
}
# for testing purposes...
# data <- data.frame(Type = rep(1:3,5), b = 4:6)
# lookup_table <- data.frame(Type = 1:3, Magnitude = c("Low","Medium","High"))
# data_col <- "Type"
# lookup_col <- "Type"
# lookup_value <- "Magnitude"
