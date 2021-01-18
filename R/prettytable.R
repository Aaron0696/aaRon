#' Printing 1 & 2 Way Frequency Tables With Relative Proportions
#'
#' Automatically append relative proportions/percentages to raw counts calculated from \code{table()}. Currently only works for 1 and 2 way frequency tables.
#'
#' @param table A table object from \code{table()}.
#' @param margin Defaults to 1, calculate row-wise proportions. Change to 2 to calculate column-wise proportions.
#'
#' @return Returns a table with relative proportions/percentages appended to raw counts.
#' @export
#'
#' @examples
#' mydata <- data.frame(x1 = rep(c("Male","Female","Male"), 100), x2 = rep(c("High","Medium","Low"),100))
#' # two way frequency table
#' a <- table(mydata$x1,mydata$x2)
#' prettytable(a)
#' # feed it into kable for a prettier table
#' knitr::kable(prettytable(a), align = "c")
#' # one way frequency table
#' a <- table(mydata$x1)
#' prettytable(a)
prettytable <- function(mytable, margin = 1)
{
  # account for one and two dimensional tables
  if(length(dim(mytable)) == 2)
  {
    # calculate proportions
    props <- prop.table(mytable, margin = margin)

    for(r in 1:nrow(mytable)){
      for(c in 1:ncol(mytable)){
        mytable[r,c] <- paste0(mytable[r,c]," (",round(props[r,c],2) * 100,"%)")
      }
    }
  } else if(length(dim(mytable)) == 1)
  {
    # calculate proportions
    props <- prop.table(mytable)

    for(r in 1:nrow(mytable)){
      mytable[r] <- paste0(mytable[r]," (",round(props[r],2) * 100,"%)")
    }
  }

  return(mytable)
}
