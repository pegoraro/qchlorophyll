################################################################################
#' Calculate a stastistic on aggregated data
#'
#' This function groups the observations and calculates a given statistic.
#' Note: By default the function calculates the mean.
#'
#' @param data a dplyr data frame
#' @param variable variable to use in the calculation of the statistic
#' @param stat_fun Function to calculate the statistic. This parameter must be a function that accepts a vector as an argument
#' and returns a single value. For instance: mean, var, sd.
#' @param groups variables to group by. A list.
#' @param unique_id unique id
#' @importFrom dplyr group_by_ summarise_ select_
#' @importFrom lazyeval interp
#' @return A dplyr data frame.
#' @export
#'
calculate_aggregate_stat <- function(data, variable = "CHL1_mean", stat_fun = "mean", groups = list("id.pixel", "id.date"), unique_id = list("lat", "lon", "id.pixel"))
{
    # Example
    # d <- calculate_aggregate_stat(provola)

    # .dots to get around NSE in dplyr
    dots_groups <- groups
    # Formula. Come passare parametri a f???
    stat_fun_formula <- interp( ~f(var, na.rm=TRUE), var = as.name(variable), f = as.name(stat_fun))
    # .dots to get around NSE in dplyr
    dots_summarise <- list(stat_fun_formula)
    # Stats
    stats <- data %>% group_by_(.dots = dots_groups) %>% summarise_(.dots = setNames(dots_summarise, c(paste(stat_fun, variable, sep = "_"))))

    # Keep unique id?
    stats <- data %>% select_(.dots = unique_id) %>% unique() %>% full_join(stats)

    return(stats)
}

################################################################################
#' Calculate multiple statistics on aggregated data
#'
#' This function groups the observations and calculates a given number of statistics.
#' The function returns a list of dataframes (ie a dataframe per statistic)
#'
#' @param data a dplyr data frame
#' @param variables variable to use in the calculation of the statistic. A list with names.
#' @param stat_funs A list of functions to calculate the statistics with.
#' @param groups variables to group by. A list.
#' @importFrom dplyr group_by_ summarise_
#' @return A list of dplyr dataframes.
#' @export
#'
aggregate_stats <- function(data, variable = "CHL1_mean", stat_funs = list(avg = "mean", sdev = "sd"), groups = list("id.pixel", "id.date"))
{
    # Define custom function to calculate stats.
    calc_stat <- function(stat_fun)
    {
        calculate_aggregate_stat(data = data,
                                 variable = variable,
                                 groups = groups,
                                 stat_fun = stat_fun)
    }

    # Set names to the list (should the list have no names, the names of the functions will be used)
    names(stat_funs) <- if( is.null(names(stat_funs)) ){ as.character(stat_funs) }else{ names(stat_funs) }

    # Calculate the statistics with each stat_fun
    statistics <- lapply(stat_funs, calc_stat)

    return(statistics)
}

################################################################################
#' Reshape a dataframe (from wide to long)
#'
#' This function reshapes a dataframe from the following format:
#' lat lon id.pixel id.date avg
#'
#' to the following format
#' lat lon id.pixel id.date_1 id.date_2 ...
#'                  avg_1     avg_2     ...
#'
#'
#' @param data dplyr dataframe to be reshaped
#' @param key key argument in tidyr::spread
#' @param id unique identifier for each pixel
#' @param other_id other unique identifiers (for instance: longitude and latitude)
#' @importFrom tidyr spread
#' @export
#'
reshape_stats <- function(data, key = "id.date", id = "id.pixel", other_id = list("lon", "lat"))
{
    # Soluzione temporanea. Di fatto value è il nome della colonna con le statistiche. (Ultima colonna)
    value <- names(data)[dim(data)[2]]

    # Select long dataframe
    data_long <- data %>% select_(id, key, value)
    # Use tidyr::spread
    data_wide <- data_long %>% spread_(key = key, value = value)
    # Add other_id using a full join
    data_wide_out <- data %>% select_(.dots = other_id, id) %>% distinct() %>% full_join(data_wide)
    # Return
    return(data_wide_out)
}

################################################################################
#' Reshape each dataframe in a list
#'
#' This function reshapes each dataframe in the list data_list.
#' The input list (data_list) must be obtained from the aggregate_stats function
#'
#' Each dataframe is reshaped as follows:
#' id.pixel lon lat jday_1 jday_2 ...
#'
#' Where jday_n is the nth julian day.
#'
#' The output list takes the names from the input list
#'
#' @param data_list a list of dplyr dataframes obtained from the aggregate_stats function.
#' @return A list of dplyr dataframes. This list has the same length as the input list.
#' @export
#'
reshape_all_stats <- function(data_list)
{
    # Apply reshape_stats to each element of the input list
    out_list <- lapply(data_list, reshape_stats)
    # Set names
    names(out_list) <- names(data_list)
    # Return
    return(out_list)
}
