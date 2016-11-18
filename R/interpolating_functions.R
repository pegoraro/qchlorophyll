################################################################################
#' Load nc files to be resized. Subfunction
#'
#' @param data_list a list of dplyr dataframe to interpolate
#' @param reference_df a reference dataframe containing at least the following variables: longitude, latitude, id_pixel
#' @param observation_id id of the observation: id_date for daily observations, month for monthly observations.
#' @importFrom dplyr %>% tbl_df
#' @return a dplyr dataframe
#' @export
#'
interpolate_grid <- function(data_list, reference_df, observation_id="id_date")
{
    # Lon-lat ranges
    x_range <- as.numeric(c(min(reference_df$lon), max(reference_df$lon)))
    y_range <- as.numeric(c(min(reference_df$lat), max(reference_df$lat)))
    step <- 0.1

    # Either: interpolate each observation for each day, or?
    # Generate grid
    grd <- expand.grid(x = seq(from = x_range[1], to = x_range[2], by = step), y = seq(from = y_range[1], to = y_range[2], by = step))
    # Set coordinates
    coordinates(grd) <- ~x + y
    gridded(grd) <- TRUE

    unique_obs <- data_list[[1]] %>% select_("id_date") %>% distinct() %>% dim()
    unique_obs <- unique_obs[1]
    print(unique_obs)

    fitted_df <- lapply(data_list, interpolate, grd = grd, unique_obs = unique_obs)
    return(fitted_df)

}

interpolate <- function(df, grd, unique_obs)
{
    fitted_data <- list()

    unused <- df %>% select(id_date, month, year)
    id_date_ <- as.numeric(unused["id_date"][1,])
    month_ <- as.numeric(unused["month"][1,])
    year_ <- as.numeric(unused["year"][1,])
    print(paste("id_date", id_date_))

    for(i in 1:unique_obs)
    {
        data <- df %>% filter(id_date == i) %>% na.omit()
        data$x <- data$lon
        data$y <- data$lat
        coordinates(data) = ~x + y
        idw <- idw(formula = qnet ~ 1, locations = data, newdata = grd)
        idw.output <- as.data.frame(idw) %>% tbl_df()
        names(idw.output)[1:3] <- c("lon", "lat", "qnet_interp")
        idw_out <- tbl_df(idw.output) %>% mutate(id_date = id_date_, month = month_, year = year_)
        fitted_data <- c(fitted_data, idw_out)
        print(i)
    }

    out <- do.call(rbind, fitted_data)
    return(out)
}


