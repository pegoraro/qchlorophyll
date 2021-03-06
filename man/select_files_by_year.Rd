% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-data-with-time-variable.R
\name{select_files_by_year}
\alias{select_files_by_year}
\title{Select files to load from a given range of years.}
\usage{
select_files_by_year(file_names, from, to)
}
\arguments{
\item{file_names}{names of the files to filter}

\item{from}{starting year (included). Either a numeric or a character. Example: 2009}

\item{to}{ending year(included). Either a numeric or a character. Example: 2010}
}
\value{
a vector of filenames filtered by year
}
\description{
Select files to load from a given range of years.
}
