#' Wrapper to keep trying an expression until no error is received
#'
#' Convenience wrapper to allow an expression to keep evaluating (until 10 attempts) until it completes without error, plays a sound when done if beepr is installed.
#'
#' @param expr Expression to be evaluated
#' @param sound Fed into beepr::beep(). Defaults to 'fanfare'
#'
#' @return Output of expression when completed
#' @export
#'
#' @examples
#' test <- function(e){if(runif(1) < 0.8){stop("Error")}else{"Good"}}
#' never_give_up(test())
#' never_give_up(test(), sound = "mario", message = "I wish this would work...")
never_give_up <- function(expr, sound = 'fanfare', message = "Never gonna give you up.")
{
  out <- message
  # counter for number of evaluations
  counter <- 0
  while(out == message & counter < 10){
    counter <- counter + 1
    out <- tryCatch(eval.parent(substitute(expr)), error = function(e){message})
    if(out == message){cat("## Eval",counter,"|",out,"\n")}
  }

  if(out != message){
    # success
    tryCatch(beepr::beep(sound), error = function(e){"Success!"})
    return(out)
  } else {
    # failure
    tryCatch(beepr::beep('sword'), error = function(e){"Failure :("})
  }
}
