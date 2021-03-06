% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vlookup.R
\name{vlookup}
\alias{vlookup}
\title{vlookup In R Using \code{merge()}}
\usage{
vlookup(data, lookup_table, data_col, lookup_col, lookup_value)
}
\arguments{
\item{data}{Dataframe to \code{vlookup} from.}

\item{lookup_table}{Table to lookup from, contains an identifier and a value column.}

\item{data_col}{String. The column from data that contains the identifier.}

\item{lookup_col}{String. The column from lookup_table that contains the identifier.}

\item{lookup_value}{String. The column from lookup_table that contains the value.}
}
\value{

}
\description{
Implementing \code{vlookup} in R using \code{merge()}.
}
\examples{
mydata <- data.frame(Type = rep(1:3,5), Rand = 4:6)
mytable <- data.frame(Type = 1:3, Magnitude = c("Low","Medium","High"))
vlookup(data = mydata,
        lookup_table = mytable,
        data_col = "Type",
        lookup_col = "Type",
        lookup_value = "Magnitude")
}
