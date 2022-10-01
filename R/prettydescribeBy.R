#' Pretty formatting for psych::describeBy()
#'
#' Format the output of describeBy() as columns for the statistics of different groups
#'
#' @param desc_out Output from describeBy()
#' @param y Variable of interest
#'
#' @return Dataframe with outputs from psych::describeBy()
#' @export
#'
#' @examples
#' mydata <- data.frame(x = c(rep("Control", 50), rep("Treatment", 50)), y = c(1:50,101:150))
#' psych::describeBy(mydata, group = "x") %>% prettydescribeBy(y = "y")
prettydescribeBy <- function(desc_out, y)
{
  outs <- list()
  for(r in 1:length(desc_out)){
    outs[[r]] <- desc_out[[r]] %>%
      # create rowname column to filter out key variable
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == y) %>%
      select(-rowname) %>%
      # pivoting begins
      pivot_longer(cols = everything(),
                   names_to = "Details",
                   values_to = paste0(names(desc_out[r]),"_Values",
                                      collapse = ""),
                   values_transform = as.numeric) %>%
      data.frame()
  }
  # merging the dataframes and creating final output
  as.data.frame(outs) %>%
    select(Details, ends_with("_Values")) %>%
    mutate(Details = toupper(Details)) %>%
    return()
}
