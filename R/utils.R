################################################################################
#' Extract variables from a .nc file
#'
#' @param var variable to be extracted
#' @param nc file with extension nc
#' @importFrom ncdf get.var.ncdf
#' @return The requested variable
#' @export
#'
load_data_from_nc <- function(var, nc)
{
    get.var.ncdf(nc, var)
}

################################################################################
#' Make path function
#'
#' Work in progress.
#'
#' @param filename name of the file
#' @param  path path to folder
#' @return full path to a file
#' @export
#'
make_path <- function(filename, path)
{
    paste(path, filename, sep = "/" )
}

################################################################################
#' Extract date from filepath
#'
#' Assumptions:
#' 1. Date is in the following format: dddddddd where d is an integer.
#' 2. Date is the first 8 digits matched in the filename.
#'
#' Notes:
#' Date format can be set
#'
#' @param path path to file
#' @param date_format Date format. Should match a lubridate
#' @importFrom lubridate parse_date_time
#' @return full path to a file
#' @export
#'
extract_date_from_filepath <- function(path, date_format = "ymd")
{
    # Splits the path and gets the filename (last string character in the path)
    file_name <- strsplit(path, "/")[[1]][length(strsplit(path, "/")[[1]])]
    # Matches a regex for a date in the filename
    file_date <- regmatches(file_name, gregexpr("[0-9]{8}", file_name))[[1]][1]
    # Parses the date
    file_date_lubridate <- parse_date_time(file_date, date_format)
    # Return
    file_date_lubridate
}


# library(stringr)
# library(lubridate)
#
# ################################################################################
# # Parameters
# ################################################################################
# path <- "/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D/"
# #selected_date_from
# selected_date <- "1998-02-25"
# #selected_date_to <- "fsdfsd"
# date_format <- "ymd"
#
# # Parse the selected date according to the selected format
# current_date <- parse_date_time(selected_date, date_format)
# # Get the appropriate
# date_fun <- match.fun(date_format)
#
# # List files in the selected path
# a <-list.files(path = path)
# # This fun extracts the dates
#
# ### Option 1. Parameters: start and end. (A vector)
# fun <- function(x) str_sub(x, start = 5, end = 12)
# ### Option 2. Parameters: regex to find
# #fun <- function(x) regmatches(x, gregexpr("[0-9]{8}",ll))[[1]][1]
#
# # Apply the function to each file to extract dates
# b <- sapply(a, fun)
# # Apply to each date the ymd function
# d <- sapply(b, ymd)
#
# ## New function.
# # This function gets the d vector and selects the files.
#
# # Get  dates between the interval indicated. To fix.
# e <- d[d < current_date]
# length(e)
# e
# # Filenames are the names of the vector e
# filestoget <- names(e)
# filestoget
