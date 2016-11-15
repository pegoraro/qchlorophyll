################################################################################
#' Load nc files to be resized. Subfunction
#'
#' @param data_list
#' @param reference_df a reference dataframe containing at least the following variables: longitude, latitude, id_pixel
#' @param observation_id id of the observation: id_date for daily observations, month for monthly observations.
#' @return a dplyr dataframe
#' @export
#'
interpolate_grid <- function(data_list, reference_df, observation_id="id_date")
{
    ##
}
