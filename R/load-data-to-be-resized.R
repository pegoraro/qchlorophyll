################################################################################
#' Load all .nc files to be resized as a list in a given local path
#'
#' @param path Path where to .nc files are located. Example: /home/data. Character.
#' @param from starting year (included). Either a numeric or a character. Example: 2009
#' @param to ending year(included). Either a numeric or a character. Example: 2010
#' @param variable variable to be extracted from .nc file
#' @param coordinates longitude and latitude names
#' @return
#' @export
#'
load_nc_to_resize <- function(path, from = NULL, to = NULL, variables = c("qnet", "time"), coordinates = c("lon", "lat"), spare_coordinates = c("longitude", "latitude"))
{
    # Load file names from path. Select only .nc files
    file_names <- list.files(path = path, pattern = "\\.nc$")
    # Select only files within the given year range
    files_to_load <- select_by_year(file_names, from = from, to = to)
    # Build path to each file
    files_to_load <- lapply(files_to_load, make_path, path)
    # Load each .nc file as a list of dataframe
    # lapply...
    files_to_load <- recover_nc_data(files_to_load[[1]], variables = variables, coordinates = coordinates, spare_coordinates = spare_coordinates)
    return(files_to_load)
}

recover_nc_data <- function(file_path, variables, coordinates, spare_coordinates)
{
    # Open file
    current_nc_file <- nc_open(file_path)
    # Check existance of coordinates and replace them with spare ones if needed.
    coordinates <- fix_coordinates(nc = current_nc_file, coordinates = coordinates, spare_coordinates = spare_coordinates)
    # Total variables to retrieve
    variables_to_get <- c(coordinates, variables)
    # Load each variable in a list
    raw_data <- lapply(variables_to_get, load_variable_from_nc, nc = current_nc_file)
    # Set names for each variable
    names(raw_data) <- variables_to_get
    # Close file
    nc_close(current_nc_file)

    # Expand coordinates (lon and lat)
    data_grid <- raw_data[c("lon","lat","time")] %>% expand.grid()
    # Convert data.frame object to a tbl_df
    data_grid <- tbl_df(data_grid)
    # Add variable
    print(class(raw_data["qnet"]))

    data_grid %>% mutate_(qnet = unlist(raw_data["qnet"]))

    return(data_grid)
}

# Cut the area of interest
#
# Interpolate

################################################################################
#' Select file names from year
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

    #
    if(is.null(from) && is.null(to))
    {
        return(names(years))
    }
    if(is.null(from) && !is.null(to))
    {
        selected_years <- years[years <= to]
        files_picked <- names(selected_years)
        return(files_picked)
    }
    if(!is.null(from) && is.null(to))
    {
        selected_years <- years[years >= from]
        files_picked <- names(selected_years)
        return(files_picked)
    }else
    {
        selected_years <- years[years <= to]
        selected_years <- selected_years[selected_years >= from]
        files_picked <- names(selected_years)
        return(files_picked)
    }
}
