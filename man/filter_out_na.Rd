% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics-calculation-functions.R
\name{filter_out_na}
\alias{filter_out_na}
\title{Filter out data with more than n missing periods}
\usage{
filter_out_na(reshaped_data_list, max_missing_periods)
}
\arguments{
\item{reshaped_data_list}{a list of reshaped statistics obtained with the function reshape_statistics}

\item{max_missing_periods}{Maximum number of missing periods. (All observations with more missing periods will be removed)}
}
\description{
Filter out data with more than n missing periods
}
