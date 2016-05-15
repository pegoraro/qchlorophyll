################################################################################
#' Extract a single variable from a .nc file
#'
#' This function extracts the variable var from a given nc file.
#'
#' @param variable variable to be extracted. Character.
#' @param nc file with .nc extension
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
    file_date_lubridate <- as.Date(parse_date_time(file_date, date_format))
    # Return
    return(file_date_lubridate)
}

################################################################################
#' Extract date from filename
#'
#' This function extracts a date from the name of a file.
#'
#' Assumptions:
#' 1. Date is in the name of the file.
#' 2. Date is in the following format: dddddddd where d is an integer.
#' 3. Date is the first 8 digits matched in the filename. Optionally you can set this to
#' be the ith match by setting the match_position parameter to i. See notes below.
#'
#' Example:
#' file_name = "file1_20150202_312314342_20150802.nc"
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
#' @param file_name name of the file. Character
#' @param date_format Date format. Should match a lubridate standard format. Character
#' @param date_match_position See notes above.
#' @importFrom lubridate parse_date_time ymd
#' @return an object of class Date.
#' @export
#'
extract_date_from_file_name <- function(file_name, date_format = "ymd", date_match_position = 1)
{
    # Matches a regex for a date in the filename
    file_date <- regmatches(file_name, gregexpr("[0-9]{8}", file_name))[[1]][date_match_position]
    # Parses the date
    file_date_lubridate <- as.Date(parse_date_time(file_date, date_format))
    # Assign to the date the name of the file (as name)
    names(file_date_lubridate) <- file_name
    # Return
    return(file_date_lubridate)
}

################################################################################
#' Check existence of a variable inside a given .nc file.
#' If the variable does not exist, use the replacement one.
#'
#' @param nc file with extension .nc already loaded in the R environment.
#' @param coordinates Grid variables (for instance, longitude and latitude).
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates.
#' @return Returns existing coordinates (either coordinates or spare_coordinates)
#' @export
#'
fix_coordinates <- function(nc, coordinates, spare_coordinates)
{
    # Check existence of coordinates. Returns boolean vector
    condition_1 <- sapply(coordinates, function(x) exists(x, get("dim", nc)), USE.NAMES = F)
    # Check existance of spare_coordinates. Returns boolean vector
    condition_2 <- sapply(spare_coordinates, function(x) exists(x, get("dim", nc)), USE.NAMES = F)
    # Select coordinates names according to their existance inside the .nc file
    existing_coordinates <- c(coordinates[condition_1], coordinates[condition_2])
    #print(condition_1)
    #print(condition_2)
    #print(existing_coordinates)

    return(existing_coordinates)
}

################################################################################
#' Filter a given date within a given interval:
#' If the date is within the interval, return the date
#' If not, return NULL.
#'
#' @param current_date date to be filtered. A Date object.
#' @param from Initial date (included). A Date object.
#' @param to Final date (included). A Date object.
#' @return Returns NULL if the date is out of the given range, otherwise returns the current_date parameter
#' @export
#'
filter_single_date <- function(current_date, from, to)
{
    if( (current_date < from) | (current_date > to))
    {
        # Date outside the given range: no date is returned
        return(NULL)
    }else
    {
        # Date inside the given range, return the name (the name here is the name of the file which the date is coming from)
        # (Since the extract_date_from_file_name returns dates with names.)
        return(names(current_date))
    }
}

################################################################################
#' Filter a list of dates within a given interval:
#' If the date is within the interval, return the date
#' If not, return NULL.
#' Do this filtering for all the dates and return a list with only the names of the
#' dates within that interval. Note that the names are the names of the files
#' which the date is coming from.
#'
#' @param dates a list of dates to be filtered. Each date must be a Date object.
#' @param from Initial date (included). A Date object.
#' @param to Final date (included). A Date object.
#' @return Returns a list with the files whose date is within the given interval
#' @export
#'
filter_dates_and_get_filenames <- function(dates, from, to)
{
    # Filter out dates outside the given range
    selected_dates_with_null <- sapply(dates, filter_single_date, from, to)
    # Remove NULL elements from the list selected_dates_with_null
    selected_dates <- Filter(Negate(is.null), selected_dates_with_null)
    # Return list of filenames with selected dates
    return(selected_dates)
}

################################################################################
#' This function basically handles how to proceed when selecting filenames by date
#'
#' Select names of the file to be loaded according to boundary conditions.
#' 4 possible cases:
#' 1. to = from = NULL. No dates have been provided, all the file_names in file_names are returned
#' 2. to = NULL, from = given. All files from the given date onwards are selected
#' 3. to = given, from = NULL. All files up to the given date are selected
#' 4. to = from = given. Files within the dates interval are selected
#'
#' @param file_names a list of file names
#' @param from Initial date. A character or NULL
#' @param to Ending date. A character or NULL
#' @param date_format date format
#' @importFrom dplyr parse_date_time
#' @return Returns a list of selected filenames to be loaded
#'
select_filenames_by_date <- function(file_names, from, to, date_format)
{
    # Case 1. Both from and to are NULL. The original list of file_names is returned
    if( is.null(from) && is.null(to) )
    {
        selected_file_names <- file_names
    }else
    {
        # Case 2, 3 and 4.

        # If from is NULL then set it to a very far away date.
        from <- if( is.null(from) ){ ymd("1950-01-01") }else{ as.Date(parse_date_time(from, date_format)) }
        # Same here but for to.
        to <- if( is.null(to) ){ ymd("2100-01-01") }else{ as.Date(parse_date_time(to, date_format)) }
        # Get the available dates from each filename
        available_dates <- lapply(file_names, extract_date_from_file_name)
        # Filter dates and return the selected filenames
        selected_file_names <- filter_dates_and_get_filenames(available_dates, from = from, to = to)
        # Verbose. Debug
        #print(selected_file_names)
    }
    return(selected_file_names)
}
