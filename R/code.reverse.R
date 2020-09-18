# reverse codes a vector using factor()
# 1,2,3,4,5 -> 5,4,3,2,1
code.reverse <- function(vector, original_levels){
  vector <- factor(vector, levels = original_levels)
  levels(vector) <- levels(vector)[length(levels(vector)):1]
  return(vector)
}
# vector <- c("SA","A","D","SD")
# code.reverse(vector, original_levels = c("SD","D","A","SA"))
