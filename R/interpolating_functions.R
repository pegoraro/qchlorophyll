################################################################################
#' Load nc files to be resized. Subfunction
#'
#' @param data_list
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

    idw <- idw(formula = qnet ~ 1, locations = area_db, newdata = grd)
    idw.output <- as.data.frame(idw) %>% tbl_df()
    names(idw.output)[1:3] <- c("lon", "lat", "qnet_interp")

}
