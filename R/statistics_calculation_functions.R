################################################################################
#' Calculate a series of stastistics on aggregated data
#'
#' This function groups the observations and calculates a series of statistics.
#'
#' @param data a dplyr data frame
#' @param variable variable to use in the calculation of the statistics
#' @param stat_funs a list of functions to calculate the statistics. Each function must be expressed either as a
#' formula or as a character. For instance, if I would like to calculate the mean, I could set stat_funs = list(avg = "mean(x)").
#' You can calculate multiple statistics too, for instance, to calculate both the mean and the standard deviation, just set
#' stat_funs = list(avg = "mean(x)", sd = "sd(x)"). Note that:
#' 1. The name of the function inside the list will be the name of the column of the statistic calculated.
#' 2. The variable inside each function should be expressed as "x".
#' 3. Each function must accept a vector as an input and output a single number.
#' @param groups variables to group by (ie to aggregate by). A list.
#' @param unique_id A list of unique identifiers.
#' @importFrom dplyr group_by_ summarise_ select_
#' @importFrom stringr str_replace
#' @return A dplyr data frame.
#' @export
#'
aggregate_statistics <- function(data, variable = "CHL1_mean", stat_funs = list(avg = "mean(x, na.rm=TRUE)"), groups = list("id_pixel", "id_date"), unique_id = list("lat", "lon", "id_pixel"))
{
    # .dots to get around NSE in dplyr
    dots_groups <- groups
    # Add NA count
    stat_funs$NAs_count <- "sum(is.na(x))"
    # Add group numerosity
    stat_funs$n_count <- "n()"

    dots_summarise <- lapply(stat_funs, function(x) str_replace(x, "x", variable))
    # Stats
    stats <- data %>% group_by_(.dots = dots_groups) %>% summarise_(.dots = dots_summarise)
    # Keep unique id
    stats <- data %>% select_(.dots = unique_id) %>% unique() %>% full_join(stats)
    # Return
    return(stats)
}

# Funzione che fa il reshape su una variabile (value)
################################################################################
#' Reshape a dataframe (from wide to long)
#'
#' This function reshapes a dataframe from the following format:
#' lat lon id_pixel id_date avg
#'
#' to the following format
#' lat lon id_pixel id_date_1 id_date_2 ...
#'                  avg_1     avg_2     ...
#'
#'
#' @param data dplyr dataframe to be reshaped
#' @param value value argument in tidyr::spread
#' @param id unique identifier for each pixel. Must be a character. If more than one unique identifier is available, then
#' place the other unique identifiers in the other_id list.
#' @param key key argument in tidyr::spread
#' @param other_id other unique identifiers (for instance: longitude and latitude)
#' @importFrom dplyr select_ %>% distinct full_join
#' @importFrom tidyr spread_
#' @export
#'
reshape_stats_df <- function(data, value, id = "id_pixel", key = "id_date" , other_id = list("lon", "lat"))
{
    # Select relevant columns
    data_long <- data %>% select_(id, key, value)
    # Convert from long to wide
    data_wide <- data_long %>% spread_(key = key, value = value)
    # Get back all the ids
    data_wide <- data %>% select_(.dots = other_id, id) %>% distinct() %>% full_join(data_wide)
    # Set names to data wide
    starting_index <- length( c(key, unlist(other_id)) ) + 1
    other_index <- starting_index - 1
    jdays <- sprintf("%03d", as.integer(names(data_wide[starting_index:40])))
    jdays_names <- sapply(jdays, function(x) paste("d_", x, sep = ""))
    names(data_wide) <- c(names(data_wide)[1:other_index], jdays_names)
    # Return
    return(data_wide)
}

# Funzione che fa il reshape su lista di variabili
################################################################################
#' Given a list of variables and a dataframe, make a reshaped (from wide to long)
#' dataframe for each variable and output a list of dataframes
#'
#' @param data dplyr dataframe to be reshaped containing the variables to be used in the reshaping process
#' @param var_list list of variables to be used in the reshaping process. These variables will be used as the
#' "value" argument in the function tidyr::spread
#' @param id unique identifier for each pixel
#' @param key key argument in tidyr::spread
#' @param other_id other unique identifiers (for instance: longitude and latitude)
#' @export
#'
reshape_statistics <- function(data, stat_list = list("avg"), id = "id_pixel", key = "id_date", other_id = list("lon", "lat"))
{
    # Define custom reshaping function
    reshaping_function <- function(value){ reshape_stats_df(data = data,
                                            value = value,
                                            id = id,
                                            key = key,
                                            other_id = other_id)}
    # Apply the reshaping function to each statistic. Note: the output is a list with a long dataframe for each statistic
    # in the var_list
    reshaped_data <- lapply(stat_list, reshaping_function)
    # Set names
    names(reshaped_data) <- unlist(stat_list)
    # Return
    return(reshaped_data)
}

#' Filter out data with more than n missing periods
#'
#' @param stats_dataframe a dplyr dataframe containing statistics about NAs calculated using the function aggregate_statistics
#' @param reshaped_data_list a list of reshaped statistics obtained with the function reshape_statistics
#' @param max_missing_periods Maximum number of missing periods. (All observations with more missing periods will be removed)
#' @param  unique_id A unique identifier of the observations such as "id_pixel"
#' @importFrom dplyr select_ %>% distinct filter_
#' @export
#'
filter_out_na <- function(stats_dataframe, reshaped_data_list, max_missing_periods = 2, unique_id = "id_pixel")
{
    # Select which pixels to keep based on missing data
    id_values_to_keep <- stats_dataframe %>%
        filter_( .dots = list(paste("NAs_count <=", max_missing_periods, sep=" "))) %>%
        select_(unique_id) %>% distinct()
    # For each reshaped dataframe, keep only the pixel selected above
    reshaped_no_na <- lapply(reshaped_data_list, function(x){ id_values_to_keep %>% inner_join(x) })
    # Return
    return(reshaped_no_na)
}
