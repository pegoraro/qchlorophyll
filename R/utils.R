################################################################################
#' Recover dates from file name and select which files to choose.
#'
#' Work in progress.
#'
#' @param
#' @importFrom
#' @return
#' @examples
#'
#' @export
#'

library(stringr)
library(lubridate)

################################################################################
# Parameters
################################################################################
path <- "/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D/"
#selected_date_from
selected_date <- "1998-02-25"
#selected_date_to <- "fsdfsd"
date_format <- "ymd"

# Parse the selected date according to the selected format
current_date <- parse_date_time(selected_date, date_format)
# Get the appropriate
date_fun <- match.fun(date_format)

# List files in the selected path
a <-list.files(path = path)
# This fun extracts the dates

### Option 1. Parameters: start and end. (A vector)
fun <- function(x) str_sub(x, start = 5, end = 12)
### Option 2. Parameters: regex to find
#fun <- function(x) regmatches(x, gregexpr("[0-9]{8}",ll))[[1]][1]

# Apply the function to each file to extract dates
b <- sapply(a, fun)
# Apply to each date the ymd function
d <- sapply(b, ymd)

## New function.
# This function gets the d vector and selects the files.

# Get  dates between the interval indicated. To fix.
e <- d[d < current_date]
length(e)
e
# Filenames are the names of the vector e
filestoget <- names(e)
filestoget
