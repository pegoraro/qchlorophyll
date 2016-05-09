################################################################################
#' Load a bunch of .nc file given a time-window
#'
#' Work in progress
#'
#' @param
#' @importFrom
#' @return
#' @examples
#'
#' @export
#'
load_observations <- function(path, from, to = NULL, vars = NULL)
{
    # Function 1. Parse the date window.
    # Function 2. Get the list of files to load and the list of dates for each file
    # Use load_nc_file on each file.

    # Return a list of loaded files as dataframes.
    # or return a single stacked dataframe?

    ### Function 4. Checks?
}

################################################################################
#' Load a single .nc file
#'
#' @param file_path the path of the file to load. Character.
#' @param current_date the date of the observations. A lubridate object
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates longitude and latitude
#' @importFrom dplyr mutate, bind_cols, tbl_df
#' @importFrom lubridate day, month, year, ymd
#' @importFrom ncdf open.ncdf, get.var.ncdf, close.ncdf
#' @return A dplyr dataframe
#' @examples
#'
#' @export
#'
#' # NOTE: Probabilmente le current_date sono gia' incluse nel file. Si possono recuperare da quello.
#'
load_nc_file <- function(file_path, current_date, variables = c("CHL1_mean"), coordinates = c("lon", "lat"))
{

    # Variabili totali recuperare.
    # Note: se assumiamo che lui lavori sempre su immagini satellitari uguali.
    # Lat e lon ci son sempre e potrei toglierle da variables
    variables_to_get <- c(coordinates, variables)

    ###################
    # Retrieving data
    ###################

    # Open file
    nc <- open.ncdf(file_path)
    # This function extracts the var variable from the .nc file
    load_data_from_nc <- function(var) get.var.ncdf(nc, var)
    # Load variables into a list
    raw_data <- lapply(variables_to_get, load_data_from_nc)
    # Set names for each variable
    names(raw_data) <- variables_to_get
    # Close file
    close.ncdf(nc)

    # Reshape data. Basically melt everything in a single dplyr dataframe.
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
#' @importFrom lubridate day, month, year
#' @importFrom dplyr mutate, %>%, bind_cols, tbl_df
#' @return A dplyr dataframe
#' @examples
#'
#' @export
#'
## Note: Stesso discorso per lat e lon. In particolare qui il reshape avverrebbe sempre su lat e lon
reshape_data <- function(raw_data, variables, expand_variables, current_date)
{
    ####################################
    # Reshaping
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
    fun <-function(x) data.frame(as.vector(t(x), mode = "numeric"))
    var_data <- lapply(raw_data[variables], fun)
    var_df <- do.call(cbind, var_data)
    names(var_df) <- variables

    # Assign variables to df
    data <- data %>%
        mutate(date = current_date) %>%
        bind_cols(var_df) %>%
        mutate(id.pixel = row_number(),
               #id.date <- as.POSIXlt(date)$yday, # Days from the beginning of the year
               month = month(date),
               year = year(date))

    return(data)
}


#-------------------------------------------------------------------------------
setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D")
library(ncdf)
library(lubridate)
library(dplyr)

# OLD
#file <- caricafile()
#file <- reshape_picture(file)
#head(file)

# NEW
file_test <- "L3m_19980125-19980201__589791848_25_GSM-SWF_CHL1_8D_00.nc"
date_test <- ymd("19980125")
file2 <- load_nc_file(file_path = file_test, current_date = date_test ,variables = c("CHL1_mean"))
# TEST
#sum(file != file2, na.rm = T)
