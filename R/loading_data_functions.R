################################################################################
#' Load a bunch of .nc files in a local path
#'
#' @param path Path where to .nc files are located. Eg: /home/data
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates longitude and latitude
#' @return A list of all the .nc files loaded
#' @examples
#'
#' @export
#'
load_all <- function(path, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), date_format = "ymd")
{
    # Elenca i file nel percorso
    file_names <- list.files(path = path)
    # Genera il percorso al singolo file (cfr utils.R)
    nc_files_path <- sapply(file_names, make_path, path = path)
    # Custom loading function. Potrebbe essere messo nel lapply evitando la definizione della funzione
    loading_function <- function(file){ load_nc_file(file, variables = variables, coordinates = coordinates, date_format = date_format) }
    # Carica tutti i file in una singola lista
    nc_files <- lapply(nc_files_path, loading_function)
    # Ritorna la lista di tutti i file caricati
    return(nc_files)
}

################################################################################
#' Load a single .nc file
#'
#' @param file_path the path of the file to load. Character.
#' @param current_date the date of the observations. A lubridate object
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates longitude and latitude
#' @importFrom ncdf open.ncdf get.var.ncdf close.ncdf
#' @return A dplyr dataframe
#' @examples
#'
#' @export
#'
load_nc_file <- function(file_path, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), date_format = "ymd")
{
    # Variabili totali recuperare.
    variables_to_get <- c(coordinates, variables)

    ###################
    # Retrieving data
    ###################

    # Extract current date (cfr utils.R)
    current_date <- extract_date_from_filepath(file_path, date_format = date_format)
    print(current_date)

    # Open file
    nc_file <- open.ncdf(file_path)
    # Load variables into a list. (cfr utils.R)
    # Memo: after FUN, in lapply, arguments are passed to FUN
    raw_data <- lapply(variables_to_get, load_data_from_nc, nc = nc_file)
    # Set names for each variable
    names(raw_data) <- variables_to_get
    # Close file
    close.ncdf(nc_file)

    # Reshape data. Basically: melt everything in a single dplyr dataframe.
    # Notes: each pixel is uniquely identified by latitude and longitude. For
    # each pixel a single measurement is performed.
    # e.g.
    # pixel 1, 2, 3 are identified by l1 lg1, l2 lg2, l3 lg3. Then measurements
    # m1, m2 and m3 are sampled.
    #
    # The melted (reshaped) df should look like this
    # lat long id.pixel meas
    # l1  lg1  1        m1
    # l2  lg2  2        m2
    # ...
    #
    data_reshaped <- reshape_data(raw_data = raw_data,
                                  variables = variables,
                                  expand_variables = coordinates,
                                  current_date = current_date)

    return(data_reshaped)
}


################################################################################
#' Reshape the raw data and output a dataframe.
#'
#' @param raw_data raw data extracted using load_nc_file. A list.
#' @param expand_variables variables to be used as x and y reference of the image. These variables must be included in the raw_data list.
#' @param current_date The date of the observation.
#' @importFrom lubridate day month year yday
#' @importFrom dplyr mutate bind_cols tbl_df
#' @return A dplyr dataframe
#' @examples
#'
#' @export
#'
reshape_data <- function(raw_data, variables, expand_variables, current_date)
{
    ####################################
    # Reshaping.
    ####################################

    # Expand coordinates (lon and lat)
    data <- raw_data[expand_variables] %>% expand.grid()

    # Convert data.frame object to a tbl_df
    data <- tbl_df(data)

    # Melt variables into a single dataframe.
    #
    # Note: variables (measurements) are retrieved as 60*92 matrices
    # fun melts each 60x92 matrix into a dataframe.
    # Then the dataframes are binded by column
    #
    # TidyR?
    fun <- function(x) data.frame(as.vector(t(x), mode = "numeric"))
    var_data <- lapply(raw_data[variables], fun)
    var_df <- do.call(cbind, var_data)
    names(var_df) <- variables

    # Assign variables to df
    data <- data %>%
        mutate(date = current_date) %>%
        bind_cols(var_df) %>%
        mutate(id.date = yday(date), # Julian date. CHECK! differs from posixlt()$yday
               month = month(date),
               year = year(date))

    return(data)
}


# #-------------------------------------------------------------------------------
## TEST: LOAD the first x files in the path getwd()/aaa
#setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D")
#cwd <- paste(getwd(), "aaa" ,sep="/")
#ddd <- load_all(cwd)
