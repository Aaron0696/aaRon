\name{code.reverse}
\alias{code.reverse}
\title{Reverse Coding of Scales Using factor()}
\usage{
code.reverse(c("SA","A","D","SD"), original_levels = c("SD","D","A","SA"))
}
\description{
Takes in a numeric, character or factor vector as its first argument. The second argument (original_levels) takes in a character vector denoting the order of the original scale (lowest level on the left).
    The output is a vector where values from the first argument have been reverse-coded, responses indicating the lowest levels now indicate the highest levels. (E.g Strongly Agree -> Strongly Disagree)
    One advantage over psych::reverse.code is that input need not be numeric.
}
\examples{
vector <- c("SA","A","D","SD")
code.reverse(vector, original_levels = c("SD","D","A","SA"))
}
