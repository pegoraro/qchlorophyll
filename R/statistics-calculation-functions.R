################################################################################
#' Calculate a series of stastistics on aggregated data
#'
#' This function groups the observations and calculates a series of statistics.
#'
#' @param data a dplyr data frame
#' @param variable variable to use in the calculation of the statistics
#' @param stat_funs a list of functions to calculate the statistics. Each function must be expressed either as a
#' formula or as a character. For instance, if I would like to calculate the mean, I could set stat_funs = list(avg = "mean").
#' You can calculate multiple statistics too, for instance, to calculate both the mean and the standard deviation, just set
#' stat_funs = list(avg = "mean", sd = "sd"). Note that:
#' 1. The name of the function inside the list will be the name of the column of the statistic calculated in the output dataframe.
#' 2. Should you need to pass arguments to each function such as "na.rm=TRUE" you can do this by setting list(avg = "mean(. , na.rm=TRUE)").
#' You can pass all the arguments needed to the functions inside stat_fun, provided you remember to add the dot as the first argument.
#' 3. Each function must accept a vector as an input and output a single number.
#' 4. By default the number of missing data per group (NAs_count = sum(is.na(.)))) and the number of observations in each group (n_count = n())
#' is calculated. Note that in case you need to filter out missing data with the function "filter_out_na", the filtering function expects the data
#' to contain a variable named NAs_count containing the information on missing data.
#' @param groups variables to group by (ie to aggregate by). A list.
#' @param id The main unique identifier (id_pixel for instance). A character.
#' @param unique_id list of all the unique identifiers in the data you would like to keep. (By default: list("lat","lon", "id_pixel")). A list
#' @importFrom dplyr group_by_ summarise_at select_ %>% distinct full_join funs_
#' @return A dplyr data frame.
#' @export
#'
#'
aggregate_statistics <- function(data, variable = "CHL1_mean", stat_funs = list(avg = "mean(., na.rm = TRUE)", NAs_count = "sum(is.na(.))", n_count = "n()"), groups = list("id_pixel", "id_date"), id = "id_pixel", unique_id = list("lat", "lon", "id_pixel"))
{
    # .dots to get around NSE in dplyr
    dots_groups <- groups
    # New names. Newly added
    #############################################
    new_names <- c(as.vector(groups), names(stat_funs))
    #############################################
    # Stats calculation.
    stats <- data %>% group_by_(.dots = dots_groups) %>% summarise_at(.vars = variable, .funs = funs_(dots = stat_funs)) %>% setNames(new_names)
    # Keep unique id
    stats <- data %>% select_(.dots = unique_id) %>% distinct() %>% full_join(stats, by = id)
    # Set statistics names as attributs
    attr(stats, "statistics_names") <- names(stat_funs)
    # Return
    return(stats)
}

################################################################################
#' Reshape a single dataframe (from wide to long)
#'
#' This function reshapes a dataframe from the following format:
#' lat lon id_pixel id_date avg
#'
#' to the following format
#' lat lon id_pixel id_date_1 id_date_2 ...
#'                  avg_11    avg_12     ...
#'
#'
#' @param value value argument in tidyr::spread
#' @param data dplyr dataframe to be reshaped
#' @param id unique identifier for each pixel. Must be a character. If more than one unique identifier is available, then
#' place the other unique identifiers in the other_id list.
#' @param key key argument in tidyr::spread
#' @param other_id other unique identifiers (for instance: longitude and latitude)
#' @importFrom dplyr select_ %>% distinct inner_join
#' @importFrom tidyr spread_
#' @export
#'
reshape_stats_df <- function(value, data, id = "id_pixel", key = "id_date" , other_id = list("lon", "lat"))
{
    # Select relevant columns
    data_long <- data %>% select_(id, key, value)
    # Convert from long to wide
    data_wide <- data_long %>% spread_(key = key, value = value)

    # Set names to data wide
    jdays <- sprintf("%03d", as.integer(names(data_wide[2:dim(data_wide)[2]])))
    jdays_names <- sapply(jdays, function(x) paste("d_", x, sep = ""))
    names(data_wide) <- c(names(data_wide)[1], jdays_names)

    # Get back all the ids
    data_wide <- data %>% select_(.dots = other_id, id) %>% distinct() %>% inner_join(data_wide, by = id)

    # Return
    return(data_wide)
}

################################################################################
#' Reshape a list of dataframes from wide to long
#'
#' Given a list of variables and a dataframe, make a reshaped (from wide to long)
#' dataframe for each variable and output a list of dataframes
#'
#' @param data dplyr dataframe to be reshaped containing the variables to be used in the reshaping process
#' @param id unique identifier for each pixel
#' @param key key argument in tidyr::spread
#' @param other_id other unique identifiers (for instance: longitude and latitude)
#' @export
#'
reshape_statistics <- function(data, id = "id_pixel", key = "id_date", other_id = list("lon", "lat"))
{
    # Get the name of the statistics to reshape
    stat_names <- attr(data, "statistics_names")
    # Apply the reshaping function to each statistic.
    # Note: the output is a list with a long dataframe for each statistic in the stat_list
    reshaped_data <- lapply(stat_names, reshape_stats_df, data, id, key, other_id)
    # Set names
    names(reshaped_data) <- stat_names
    # Return
    return(reshaped_data)
}

################################################################################
#' Filter out data with more than n missing periods
#'
#'
#' @param reshaped_data_list a list of reshaped statistics obtained with the function reshape_statistics
#' @param max_missing_periods Maximum number of missing periods. (All observations with more missing periods will be removed)
#' @export
#'
filter_out_na <- function(reshaped_data_list, max_missing_periods)
{
    # Temporary dataframe
    temp_df <- NULL

    # Reshape each statistics
    for(i in 1:length(reshaped_data_list))
    {
        # Assign tempororay df
        temp_df <- reshaped_data_list[[i]]
        # Calculate number of NAs for each row
        NA_number_each_row <- apply(temp_df, 1, function(x) sum(is.na(x)))
        # Dataframe without NAs
        less_NAs_df <- temp_df[ NA_number_each_row <= max_missing_periods, ]
        # Number of NAs within each time series
        NA_number <- apply(less_NAs_df, 1, function(x) sum(is.na(x)))
        # Reassign new df to old place in the list
        reshaped_data_list[[i]] <- less_NAs_df
    }
    return(reshaped_data_list)
}
