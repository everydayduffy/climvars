% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bio16.R
\name{bio16}
\alias{bio16}
\title{bio16: Calculate precipitation of the wettest quarter.}
\usage{
bio16(prec, tme)
}
\arguments{
\item{prec}{a vector of precipitation values, normally for one year (see
details).}

\item{tme}{a \code{POSIXlt} object representing the date and time of each \code{prec}
value.}
}
\value{
a single numeric value of total precipitation of the wettest quarter.
}
\description{
\code{bio16} is used to calculate the total precipitation of the
wettest quarter of the year.
}
\details{
Precipitation for each quarter is calculated and total
precipitation in the wettest quarter returned. If data span more than one
year, calculations are performed on all data and a single value returned.
}
\examples{
tme <- tmecreate(2010, 1)
plot(hourly_precip~as.POSIXct(tme), type = "l", xlab = "Month",
ylab = "Precipitation")
bio16(hourly_precip, tme)
}
\seealso{
the \code{\link[=tmecreate]{tmecreate()}} function can be used to create a POSIXlt object.
}
