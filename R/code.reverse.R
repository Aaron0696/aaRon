# reverse codes a vector using factor()
# 1,2,3,4,5 -> 5,4,3,2,1
code.reverse <- function(vector, original_levels){
  # convert vector input to a factor with the original levels
  vector <- factor(vector, levels = original_levels)
  # reverse code values in the vector by flipping the order of the vector
  levels(vector) <- levels(vector)[length(levels(vector)):1]
  # return the levels to the original without changing values in the vector
  vector <- factor(vector, levels = original_levels)
  # return
  return(vector)
}
# demonstration
# vector <- c("SA","A","D","SD")
# code.reverse(vector, original_levels = c("SD","D","A","SA"))
