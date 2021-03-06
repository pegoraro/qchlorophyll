% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filter_dates_and_get_filenames}
\alias{filter_dates_and_get_filenames}
\title{Filter a list of dates within a given interval:
If the date is within the interval, return the date
If not, return NULL.
Do this filtering for all the dates and return a list with only the names of the
dates within that interval. Note that the names are the names of the files
which the date is coming from.}
\usage{
filter_dates_and_get_filenames(dates, from, to)
}
\arguments{
\item{dates}{a list of dates to be filtered. Each date must be a Date object.}

\item{from}{Initial date (included). A Date object.}

\item{to}{Final date (included). A Date object.}
}
\value{
Returns a list with the files whose date is within the given interval
}
\description{
Filter a list of dates within a given interval:
If the date is within the interval, return the date
If not, return NULL.
Do this filtering for all the dates and return a list with only the names of the
dates within that interval. Note that the names are the names of the files
which the date is coming from.
}
