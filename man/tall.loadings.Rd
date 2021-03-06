% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tall.loadings.R
\name{tall.loadings}
\alias{tall.loadings}
\title{Tall/Long Factor Loading Matrices}
\usage{
tall.loadings(structure, cut = 0.2, dp = 2, sortby = "FACTOR")
}
\arguments{
\item{structure}{A matrix/dataframe of factor loadings in wide-format.}

\item{cut}{Defaults to 0.2. The minimum cut-off for factor loadings. Factor loadings below this value are removed.}

\item{dp}{Defaults to 2. Number of decimal points for all numeric values.}

\item{sortby}{Defaults to "FACTOR". Sorts the output by factors, change value to "ITEM" to sort by items.}
}
\value{

}
\description{
Factor loadings from factor analysis functions such as \code{psych::fa()} produce factor loading matrices in wide-format.
Tall versions of factor loading matrices allow us to better inspect the items and factor loadings of each individual factor.
This is a function created to automate the conversion to tall format, eliminating factor loadings below a certain threshold and sorting the output by factors or items.
}
\examples{
library(psych)
myefa <- fa(Harman74.cor$cov, 4, fm = "wls")
tall.loadings(myefa$Structure)
tall.loadings(myefa$loadings)
}
