% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bio14.R
\name{bio14}
\alias{bio14}
\title{bio14: Calculate precipitation of the driest period.}
\usage{
bio14(prec, tme, period = 7)
}
\arguments{
\item{prec}{a vector of precipitation values, normally for one year (see
details).}

\item{tme}{a \code{POSIXlt} object representing the date and time of each \code{prec}
value.}

\item{period}{a single value defining the number of days for which total
precipitation is calculated for.}
}
\value{
a single numeric value of total precipitation in the driest period
of the year.
}
\description{
\code{bio14} is used to calculate the total precipitation in the
driest period of the year.
}
\details{
The total precipitation in a given period starting at each data
point is calculated, and the driest returned. By default the period is set
to 7 days. If data span more than one year, data across the whole period are
considered and a single value returned.
}
\examples{
tme <- tmecreate(2010, 1)
plot(hourly_precip~as.POSIXct(tme), type = "l", xlab = "Month",
ylab = "Precipitation")
bio14(hourly_precip, tme)
bio14(hourly_precip, tme, period = 30)
}
\seealso{
the \code{\link[=tmecreate]{tmecreate()}} function can be used to create a POSIXlt object.
}
