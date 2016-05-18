################################################################################
#' Calculate a stastistic on aggregated data
#'
#' This function groups the observations and calculates a given statistic.
#' Note: By default the function calculates the mean.
#'
#' @param data a dplyr data frame
#' @param variable variable to use in the calculation of the statistic
#' @param groups variables to group by. A list.
#' @param stat_fun Function to calculate the statistic. This parameter must be a function that accepts a vector as an argument
#' and returns a single value. For instance: mean, var, sd.
#' @param unique_id unique id
#' @importFrom dplyr group_by_ summarise_ select_
#' @return A dplyr data frame.
#' @export
#'
calculate_aggregate_stat <- function(data, variable = "CHL1_mean", groups = list("id.pixel", "id.date"), stat_fun = "mean", unique_id = list("lat", "lon", "id.pixel"))
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
#' @param groups variables to group by. A list.
#' @param stat_funs A list of functions to calculate the statistics with.
#' and returns a single value. For instance: mean, var, sd.
#' @importFrom dplyr group_by_ summarise_
#' @return A list of dplyr dataframes.
#' @export
#'
aggregate_stats <- function(data, variable = "CHL1_mean", groups = list("id.pixel", "id.date"), stat_funs = list(avg = "mean", sdev = "sd"))
{
    # Example
    # aggregate_stats(provola, stat_funs = list("mean","sd"))

    # Define function to calculate stats
    calc_stat <- function(stat_fun)
    {
        calculate_aggregate_stat(data = data,
                                 variable = variable,
                                 groups = groups,
                                 stat_fun = stat_fun)
    }

    # Set names to the list (should the list have no names, the names of the functions are used)
    names(stat_funs) <- if( is.null(names(stat_funs)) ){ as.character(stat_funs) }else{ names(stat_funs) }

    # Calculate the statistics
    statistics <- lapply(stat_funs, calc_stat)

    return(statistics)
}

#' Reshape stats
#'
#' This function reshapes the list of statistics calculated using aggregate stats in the following way:
#' 1. The list of dataframe (each dataframe contains a statistic) is
#'
#' @param
#' @importFrom
#' @return
#' @export
#'
reshape_stats <- function()
{
    ##
}
