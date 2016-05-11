################################################################################
#' Extract a single variable from a .nc file
#'
#' This function extracts the variable var from a given nc file.
#'
#' @param variable variable to be extracted. Character.
#' @param nc file with nc extension
#' @importFrom ncdf get.var.ncdf
#' @return The requested variable. Data structure depends on which variable is recovered.
#' @export
#'
load_variable_from_nc <- function(variable, nc)
{
    get.var.ncdf(nc, variable)
}

################################################################################
#' Make path function
#'
#' Given a path to a folder and the name of a file in that folder, this function
#' builds the path to that file.
#'
#' Example:
#' path = "/home/user/date"
#' filename = "data.txt"
#' Return = "/home/user/date/data.txt"
#'
#' @param file_name name of the file. Character
#' @param  path path to folder. Character
#' @return full path to a file. Character
#' @export
#'
make_path <- function(file_name, path)
{
    paste(path, file_name, sep = "/" )
}

################################################################################
#' Extract date from filepath
#'
#' This function extracts a date from a full path to a file. The date is assumed to
#' be in the name of the file.
#'
#' Assumptions:
#' 1. Date is in the name of the file.
#' 2. Date is in the following format: dddddddd where d is an integer.
#' 3. Date is the first 8 digits matched in the filename. Optionally you can set this to
#' be the ith match by setting the match_position parameter to i. See notes below.
#'
#' Example:
#' path = "/home/user/data/file1_20150202_312314342_20150802.nc"
#' date_format = "ymd"
#' Returns "20150202"
#'
#' Notes:
#' 1. Note that the function by default returns the first 8 digits sequence it encounters in the filename
#' (starting left to right). The argument match_position can be used to access other
#' dates. For instance match_position = 2 matches the second date (or, better, 8 digits sequence)
#' it finds.
#' 2. Note that date format can be set (should it vary).
#' 3. If the date_format is not correct a NA is returned
#'
#' @param path path to file. Character
#' @param date_format Date format. Should match a lubridate standard format. Character
#' @param date_match_position See notes above.
#' @importFrom lubridate parse_date_time
#' @return an object of class Date.
#' @export
#'
extract_date_from_filepath <- function(path, date_format = "ymd", date_match_position = 1)
{
    # Splits the path and gets the filename (last string character in the path)
    file_name <- strsplit(path, "/")[[1]][length(strsplit(path, "/")[[1]])]
    # Matches a regex for a date in the filename
    file_date <- regmatches(file_name, gregexpr("[0-9]{8}", file_name))[[1]][date_match_position]
    # Parses the date
    file_date_lubridate <- parse_date_time(file_date, date_format)
    # Return
    return(file_date_lubridate)
}

# #' Select .nc files only in the current path
# #'
# #' @param file_name name of the file. Character
# #' @param  path path to folder. Character
# #' @return full path to a file. Character
# #' @export
# #' Da sistemare
# select_nc_files_only <- function(file_name)
# {
#     file_name_filtered <- regmatches(file_name, gregexpr(".*.nc$", file_name))[[1]]
#     if( length(file_name_filtered) == 0 )
#     {
#         return(NULL)
#     }else{
#         return(file_name_filtered)
#     }
# }
