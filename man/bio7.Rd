% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bio7.R
\name{bio7}
\alias{bio7}
\title{bio7: Calculate annual temperature range.}
\usage{
bio7(temps, tme, min_percentile = 0.01, max_percentile = 0.99)
}
\arguments{
\item{temps}{a vector of temperatures, normally for one year (see details).}

\item{tme}{a \code{POSIXlt} object representing the date and time of each \code{temps}
value.}

\item{min_percentile}{a value between 0-1 used to define the minimum
percentile.}

\item{max_percentile}{a value between 0-1 used to define the maximum
percentile.}
}
\value{
a single numeric value of annual temperature range (maximum-minimum
temperature values).
}
\description{
\code{bio7} is used to calculate the annual range in temperature.
}
\details{
The range is calculated by subtracting the minimum from the maximum
temperature values for a year. By default, percentiles are set at 1\% and 99\%
for the minimum and maximum values respectively. If data spans more than one
year then a single value considering all data is returned.
}
\examples{
tme <- tmecreate(2010, 1)
plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
bio7(hourly_temps, tme)
bio7(hourly_temps, tme, min_percentile = 0.05, max_percentile = 0.95)
}
\seealso{
the \code{\link[=tmecreate]{tmecreate()}} function can be used to create a POSIXlt object.
}
