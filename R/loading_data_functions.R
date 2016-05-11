################################################################################
#' Load all .nc files in a given local path
#'
#' @param path Path where to .nc files are located. Eg: /home/data. Character.
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates Grid variables (for instance, longitude and latitude).
#' @param date_match_position An integer. If the name of each file contains one or more dates, chose which one will be used as the
#' current date for the file. Example: if a file is named "20150202file2_20150706.nc", if date_match_position is set
#' to 1 (default), the first date, 20150202 will be used. If you'd like to use the second one, set the parameter to 2.
#' @return A list of all the .nc files loaded
#' @examples
#' # Load all .nc files in /home/data, extract the variable "CHL1_mean" and use longitude and latitude to uniquely identify each observation
#' # loaded_files_list <- load_all(path = /home/data, variables = c("CHL1_mean"), coordinates = c("lon","lat"))
#'
#' @export
#'
load_all <- function(path, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), date_format = "ymd", date_match_position = 1)
{
    # List files in the given path
    file_names <- list.files(path = path)

    # Seleziona solo i file con estensione .nc
    # Funzione che fa quel lavoro ()

    # Genera il percorso al singolo file (cfr utils.R)
    nc_files_path <- sapply(file_names, make_path, path = path)

    # Custom loading function. Potrebbe essere messo nel lapply evitando la definizione della funzione, volendo.
    # Ma forse è più chiaro cosi'.
    loading_function <- function(file){ load_nc_file(file, variables = variables, coordinates = coordinates, date_format = date_format, date_match_position = date_match_position) }

    # Carica tutti i file in una singola lista.
    # Ogni elemento della lista contiene un dataframe dplyr
    nc_files <- lapply(nc_files_path, loading_function)

    # Assign id and bind rows together
    output <- assign_id_and_melt(nc_files, coordinates = coordinates)

    # Ritorna la lista di tutti i file caricati
    return(output)
}

################################################################################
#' Load a single .nc file
#'
#' This function loads a single .nc files and returns a dplyr dataframe of the extracted variables.
#'
#' @param file_path the path of the file to load. Character.
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates longitude and latitude
#' @param date_format the format of the dates for each file.
#' @param date_match_position An integer. If the name of each file contains one or more dates, chose which one will be used as the
#' current date for the file. Example: if a file is named "20150202file2_20150706.nc", if date_match_position is set
#' to 1 (default), the first date, 20150202 will be used. If you'd like to use the second one, set the parameter to 2.
#' @importFrom ncdf open.ncdf get.var.ncdf close.ncdf
#' @return A dplyr dataframe
#' @examples
#' # Load "data02022015.nc"
#' # data_02022015 <- load_nc_file("~path/data02022015.nc", date_format = "dmy")
#' @export
#'
load_nc_file <- function(file_path, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), date_format = "ymd", date_match_position = 1)
{
    # Total variables to retrieve
    variables_to_get <- c(coordinates, variables)

    ###################
    # Retrieving data
    ###################

    # Extract current date from filepath(cfr utils.R)
    current_date <- extract_date_from_filepath(file_path,
                                               date_format = date_format,
                                               date_match_position = date_match_position)
    print(current_date)

    # Open file
    current_nc_file <- open.ncdf(file_path)

    # Load each variable into a list. (cfr utils.R)             # Memo: after FUN, in lapply, arguments are passed to FUN
    raw_data <- lapply(variables_to_get, load_variable_from_nc, nc = current_nc_file)

    # Set names for each variable
    names(raw_data) <- variables_to_get

    # Close file
    close.ncdf(current_nc_file)

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
#' Reshape the raw data and output into a dplyr dataframe.
#'
#' @param raw_data raw data extracted using load_nc_file. A list.
#' @param expand_variables variables to be used as x and y reference of the image grid. These variables must be included in the raw_data list.
#' @param current_date The date of the observation.
#' @importFrom lubridate day month year yday
#' @importFrom dplyr mutate bind_cols tbl_df %>%
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
    data_grid <- raw_data[expand_variables] %>% expand.grid()

    # Convert data.frame object to a tbl_df
    data_grid <- tbl_df(data_grid)

    # Melt variables into a single dataframe.
    #
    # Note: variables (measurements) are retrieved as 60*92 matrices
    # fun melts each 60x92 matrix into a dataframe.
    # Then the dataframes are binded by column
    #
    # TidyR?
    fun <- function(x) data.frame(as.vector((x), mode = "numeric"))
    variables_data <- lapply(raw_data[variables], fun)
    variables_df <- do.call(cbind, variables_data)
    names(variables_df) <- variables

    # Assign variables to a reshaped
    reshaped_data <- data_grid %>%
        mutate(date = current_date) %>%
        bind_cols(variables_df) %>%
        mutate(
               #id.pixel = row_number(),
               id.date = yday(date) - 1, # Julian date. CHECK! differs from posixlt()$yday hence the -1
               month = month(date),
               year = year(date))

    return(reshaped_data)
}


################################################################################
#' Add an id for each pixel and bind all the rows together
#'
#' @param data_list
#' @param current_date The date of the observation.
#' @importFrom dplyr rbind_all %>% select mutate row_number full_join
#' @return A dplyr dataframe
#' @examples
#'
#' @export
#'
assign_id_and_melt <- function(data_list, coordinates)
{

    # Bind all rows in a single dataframe
    data <- data_list %>% rbind_all()
    # Calculate id.pixel
    id <- data %>% select_(coordinates[2], coordinates[1]) %>% unique() %>% mutate(id.pixel = row_number())
    # Add id.pixel
    data <- full_join(id, data)

    return(data)
}

# #-------------------------------------------------------------------------------
# Note:
#sistemare la questione lon, longitude nella funzione utils per caricare nc files.
#dare la possibilità di scegliere la finestra di date dei file da caricare
#fare in modo, editando una regex, che la lista dei file nel path contenga solo file con estensione .nc
