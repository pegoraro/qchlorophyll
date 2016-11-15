# Cose da fare:
# 2. Permettere all'utente di scegliere l'area da ritagliare (load_data_to_resize)
# 3. Testare su shf
# 4. Funzione interpolante

################################################################################
#' Load all .nc files to be resized as a list in a given local path
#'
#' Note: these files contain a time variable. They represent observations of a
#' certain variable with a given frequency. The variable "time" defines the name of
#' the sampling frequency variable. The function assumes that the data has a daily
#' frequency. If the frequency is monthly, set the monthy variable to TRUE.
#'
#' @param path Path where to .nc files are located. Example: /home/data. Character.
#' @param from starting year (included). Either a numeric or a character. Example: 2009
#' @param to ending year(included). Either a numeric or a character. Example: 2010
#' @param variables variables to be extracted from .nc file
#' @param coordinates longitude and latitude names
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
#' to be: c("longitude","latitude").
#' @param time_variable variable representing the frequency of the observations. Character.
#' @param monthly whether data has a monthly or an annual frequency. TRUE if data has a monthly frequency.
#' FALSE if frequency is annual. FALSE by default.
#' @importFrom stringi stri_extract_last_regex
#' @return a list of dplyr dataframes
#' @export
#'
load_nc_to_resize <- function(path, from = NULL, to = NULL, variables = c("qnet"), coordinates = c("lon", "lat"), spare_coordinates = c("longitude", "latitude"), time_variable = "time", monthly=FALSE)
{
    # Load file names from path. Select only .nc files
    file_names <- list.files(path = path, pattern = "\\.nc$")
    # Select only files within the given year range
    files_to_load <- select_by_year(file_names, from = from, to = to)
    # Build path to each file
    files_to_load_path <- lapply(files_to_load, make_path, path)
    # Load each .nc file as a list of dataframe
    files_loaded <- lapply(files_to_load_path, recover_nc_data, variables = variables, coordinates = coordinates, spare_coordinates = spare_coordinates, time_variable = time_variable, monthly=monthly)
    # Set names
    names(files_loaded) <- stri_extract_last_regex(files_to_load, "\\d{4}")
    # Return a list of dataframes ready to be manipulated
    return(files_loaded)
}

################################################################################
#' Load nc files to be resized. Subfunction
#'
#' @param file_path Path where the file is located. Example: /home/data. Character.
#' @param variables variables to be extracted from .nc file
#' @param coordinates longitude and latitude names
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
#' to be: c("longitude","latitude").
#' @param time_variable variable representing the frequency of the observations. Character.
#' @param monthly whether data has a monthly or an annual frequency. TRUE if data has a monthly frequency.
#' FALSE if frequency is annual. FALSE by default.
#' @importFrom stringi stri_extract_last_regex
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom lubridate dmy yday month year
#' @importFrom dplyr %>% mutate tbl_df bind_cols filter select_ mutate_
#' @importFrom lazyeval interp
#' @return a dplyr dataframe
#' @export
#'
recover_nc_data <- function(file_path, variables, coordinates, spare_coordinates, time_variable, monthly)
{
    # Get year
    year <- stri_extract_last_regex(file_path, "\\d{4}")

    # Open file
    current_nc_file <- nc_open(file_path)
    # Check existance of coordinates and replace them with spare ones if needed.
    coordinates <- fix_coordinates(nc = current_nc_file, coordinates = coordinates, spare_coordinates = spare_coordinates)
    # Total variables to retrieve
    variables_to_get <- c(coordinates, variables, time_variable)
    # Load each variable in a list
    raw_data <- lapply(variables_to_get, load_variable_from_nc, nc = current_nc_file)
    # Set names for each variable
    names(raw_data) <- variables_to_get
    # Close file
    nc_close(current_nc_file)

    # Expand coordinates (lon and lat)
    data_grid <- raw_data[c(coordinates, time_variable)] %>% expand.grid()
    # Convert data.frame object to a tbl_df
    data_grid <- tbl_df(data_grid)

    # Add variables to data grid. Old add variables mode: data_grid$qnet <- c(raw_data[[variables]])
    fun <- function(x) data.frame(as.vector((x), mode = "numeric"))
    variables_data <- lapply(raw_data[variables], fun)
    variables_df <- do.call(cbind, variables_data)
    names(variables_df) <- variables
    variables_df <- tbl_df(variables_df)
    data <- data_grid %>% bind_cols(variables_df)

    # Fix longitude. GENERALIZZARE.
    temp <- which(data$lon > 180)
    data$lon[temp] <- data$lon[temp] - 360

    # Devono poter essere definiti dall'utente
    lower_left_lat_lon <- c(52.00, -65.00)
    upper_right_lat_lon <- c(67.00, -42.00)

    # Select only the area you're interested in
    data <- data %>%
        filter(lat >= lower_left_lat_lon[1] & lat <= upper_right_lat_lon[1] & lon >= lower_left_lat_lon[2] & lon <= upper_right_lat_lon[2])

    # Referemce date
    ref_date <- dmy(paste("31-12", year, sep = "-"))

    # Add id_date, month, year. Mutate and select calls
    month_mutate_call <- interp(~ month(ref_date + time), ref_date=ref_date, time=as.name("time"))
    select_call <- list(coordinates[1], coordinates[2], variables, "id_date", "month", "year")
    if(monthly)
    {
        month_mutate_call <- interp(~ time, time=as.name("time"))
        select_call <- list(coordinates[1], coordinates[2], variables, "month", "year")
    }

    mutate_call <- list(id_date = interp(~ yday(ref_date + time), ref_date=ref_date, time=as.name("time") ),
                        month = month_mutate_call,
                        year = interp(~ year(ref_date), ref_date=ref_date))

    # Mutate and select variables
    data <- data %>%
                mutate_(.dots = mutate_call) %>%
                    select_(.dots = select_call)

    # Print status info
    print(paste("Loaded: ", strsplit(file_path, "/")[[1]][ length(strsplit(file_path, "/")[[1]]) ] ))

    # Return data
    return(data)
}

################################################################################
#' Select files to load from a given range of years
#'
#' @param file_names names of the files to filter
#' @param from starting year (included). Either a numeric or a character. Example: 2009
#' @param to ending year(included). Either a numeric or a character. Example: 2010
#' @importFrom stringi stri_extract_last_regex
#' @return a vector of filenames filtered by year
#' @export
#'
select_by_year <- function(file_names, from, to)
{
    # Extract year from file name and convert it to a numeric
    years <- sapply(file_names, function(x) stri_extract_last_regex(x, "\\d{4}"))
    years <- sapply(years, as.numeric)

    # Select files from year range

    # If neither from or to are supplied, select the whole range
    if(is.null(from) && is.null(to))
    {
        return(names(years))
    }
    # If to is supplied, load all files up to the selected year (included)
    if(is.null(from) && !is.null(to))
    {
        selected_years <- years[years <= to]
        files_picked <- names(selected_years)
        return(files_picked)
    }
    # If from is supplied, load all files from the selected year(included)
    if(!is.null(from) && is.null(to))
    {
        selected_years <- years[years >= from]
        files_picked <- names(selected_years)
        return(files_picked)
    }
    # Else, (if both are supplied), load the files in the supplied range of years
    else
    {
        selected_years <- years[years <= to]
        selected_years <- selected_years[selected_years >= from]
        files_picked <- names(selected_years)
        return(files_picked)
    }
}
