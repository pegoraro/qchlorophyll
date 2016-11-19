################################################################################
#' Interpolate a list of dplyr dataframes
#'
#' @param data_list a list of dplyr dataframe to interpolate
#' @param reference_df a reference dataframe containing at least the following variables: longitude, latitude, id_pixel
#' @param variable variable to interpolate
#' @param unique_id unique id of the observation (usually id_date or id_month)
#' @param step step in the longitude-latitude grid. Set to 0.25 by default.
#' @param coordinates_names names of the coordinates (longitude and latitude)
#' @return a dplyr dataframe
#' @export
#'
interpolate_grid <- function(data_list, reference_df, variable, unique_id = "id_date", step = 0.25, coordinates_names = c("lon", "lat"))
{
    # Determine lon and lat range and step
    lon_range <- as.numeric(c(min(reference_df[coordinates_names[1]]), max(reference_df[coordinates_names[1]])))
    lat_range <- as.numeric(c(min(reference_df[coordinates_names[2]]), max(reference_df[coordinates_names[2]])))

    # Print status info
    print(paste("Starting interpolation process. Interpolating", length(data_list), "file(s). This may take a while...", sep = " "))

    # Interpolate each dataframe in the list
    fitted_df_list <- lapply(data_list, interpolate_single_grid,
                             unique_id = unique_id,
                             variable = variable,
                             coordinates_names = coordinates_names,
                             lon_range = lon_range,
                             lat_range = lat_range,
                             step = step)

    # Print status
    print("Done interpolating data frames. Stacking data frames row wise...")
    # Stack all the dataframes in a single dataframe
    fitted_df <- bind_rows(fitted_df_list)
    # Done
    print("Done!")
    # Return
    return(fitted_df)
}

################################################################################
#' Interpolate each data grid.
#' This function interpolates every datagrid day by day (or month by month in case
#' of monthly observations)
#'
#' @param df a dplyr dataframe to interpolate
#' @param unique_id unique id of the observation (usually id_date or id_month)
#' @param variable variable to interpolate
#' @param coordinates_names names of the coordinates (longitude and latitude)
#' @param lon_range longitude range (min, max)
#' @param lat_range latitude range (min, max)
#' @param step step between longitude and latitude vectors
#' @importFrom dplyr %>% select_ distinct filter mutate_ tbl_df bind_rows
#' @importFrom sp coordinates gridded
#' @importFrom gstat idw
#' @importFrom lazyeval interp
#' @return a dplyr dataframe
#' @export
#'
interpolate_single_grid <- function(df, unique_id = "id_date", variable, coordinates_names, lon_range, lat_range, step)
{
    # Count the number of days/month to interpolate
    days <- df %>% select_(unique_id) %>% distinct() %>% nrow()
    # Output list
    out_lst <- list()

    # Set new grid, coordinates and gridded attribute
    grd <- expand.grid(lon = seq(from = lon_range[1], to = lon_range[2], by = step), lat = seq(from = lat_range[1], to = lat_range[2], by = step))
    sp::coordinates(grd) <- ~ lon + lat
    sp::gridded(grd) <- TRUE
    # idw formula
    idw_formula <- as.formula(paste(variable, 1, sep=" ~ "))

    # Start to interpolate each date
    for(i in 1:days)
    {
        # Select data from the ith date
        day_data <- df %>% filter_(interp(~x == y, x = as.name(unique_id), y = i)) %>% na.omit()

        # Set aside variables common to each date (id_date, month, year)
        other_vars <- day_data %>%
            select_(.dots = as.list(setdiff(names(df), c(coordinates_names, variable)) )) %>%
            distinct() %>%
            as.list()
        # Set coordinates
        sp::coordinates(day_data) = ~ lon + lat
        # Interpolate using idw
        idw <- idw(formula = idw_formula, locations = day_data, newdata = grd, debug.level = 0)
        idw_output <- as.data.frame(idw)
        # Set names
        names(idw_output)[1:3] <- c(coordinates_names[1], coordinates_names[2], variable)
        # Format output
        idw_output <- tbl_df(idw_output) %>%
            select_(coordinates_names[1], coordinates_names[2], variable) %>%
            mutate_(.dots = other_vars)
        # Assign output to list
        out_lst[[i]] <- idw_output
    }
    # Bind all the dataframes in a single dataframe
    df_out <- bind_rows(out_lst)
    # Print status
    print("Done interpolating current file.")
    # Return
    return(df_out)
}
