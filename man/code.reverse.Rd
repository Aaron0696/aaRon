\name{code.reverse}
\alias{code.reverse}
\title{Reverse Coding of Scales}
\usage{
code.reverse(vector, original_levels)
}
\arguments{
  \item{vector}{A numeric, character or factor vector.}
  \item{original_levels}{A character vector denoting the order of the original scale (lowest level on the left)}
}
\value{
Returns a factor vector of the same length as \code{vector} with reverse-coded responses, with levels corresponding to \code{original_levels}
}
\description{
Implements reverse-coding of items using \code{factor()}. One advantage over \code{psych::reverse.code} is that input need not be numeric.
}
\examples{
mydata <- data.frame(myscale = c("Disagree", "Neutral", "Agree", "Neutral","Agree"))
code.reverse(vector = mydata$myscale, original_levels = c("Disagree","Neutral","Agree"))
}
