################################################################################
#' Load a bunch of csv files into a single list
#'
#'
#' @param main_folder_path path to the main folder
#' @param folder name of the folder containing the .csv files to load. A character.
#' @param starts_with initial part of the name of the .csv file. Each file is assumed to have a recurrent pattern
#' in its name where the only changing part is assumed to be the year. A character
#' @param years years to load. A character vector.
#' @param must_have_variables a vector of variables that must be included in each .csv file. Character vector.
#' @importFrom dplyr tbl_df mutate %>% bind_rows
#' @export
#'
load_all_csv <- function(main_folder_path, folder, starts_with, years = c("2011", "2012"), npixels = 5520, must_have_variables = c("lon", "lat"))
{
    #####
    # Building path
    #####
    # Build folder path
    folder_path <- paste(main_folder_path, folder, sep = "/")
    # Build partial file path
    partial_file_path <- paste(folder_path, starts_with, sep = "/")
    # Attach .csv at the end of each year
    years_csv_ending <- lapply(years, function(x) paste(x, ".csv", sep = ""))
    # Build full path to each file
    file_paths <- lapply(years_csv_ending, function(x) paste(partial_file_path, x, sep = ""))

    ####
    # Loading files into a list
    ####

    # Load list of df
    csv_files <- lapply(file_paths, function(x) read.csv(x))
    # Convert each df into a tbl_df
    tbls <- lapply(csv_files, function(x) tbl_df(x))

    ####
    # Checks
    ####

    # Count number of rows for each dataframe
    nrows <- lapply(tbls, function(x) nrow(x))
    # Check that each dataframe has 5520 rows (pixels)
    if(any(nrows != npixels)){ stop(paste("Some dataframes contain more or less pixels than the selected value of", npixels, sep = " ")) }
    # Check that each dataframe contains "lon" and "lat" (must_have_variables)
    names_df <- lapply(tbls, function(x) names(x))
    names_bool <- lapply(names_df, function(x) must_have_variables %in% x)
    if(any(!unlist(names_bool))){ stop("Some of the required variables are not in some of the input .csv files") }

    ####
    # Adding year information
    ####

    # Add year to each dataframe
    for(i in 1:length(years))
    {
        tbls[[i]] <- tbls[[i]] %>% mutate(year = as.numeric(years[i]))
    }

    # Stack all the dataframes into a single df
    #tbl <- bind_rows(tbls)

    # Return
    return(tbls)
}


#' Add id_pixel and kmeans group from reference dataframe. Make a left join
#'
#' @param dataframe_list a list of dplyr dataframes
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @param by variables to join by
#' @export
#'
add_id_pixel_and_groups <- function(dataframe_list, reference_dataframe, by = c("lon","lat"))
{
    out <- lapply(dataframe_list, function(x) left_join(x, reference_dataframe, by = by))
    return(out)
}

#' Fix latitude and longitude
#'
#'
#' @param df_list a list of dplyr dataframe
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @param reformat Should lon and lat be fixed? Boolean.
#' @export
#'
format_lon_lat_list <- function(df_list, reference_df, variable=NULL, reformat = TRUE)
{
    if(reformat)
    {
        out <- lapply(df_list, format_lon_lat, reference_df, variable)
    }else
    {
        out <- df_list
    }
    return(out)
}

#' Fix latitude and longitude
#'
#' @param df_list a list of dplyr dataframe
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @export
#'
format_lon_lat <- function(dataframe, reference_df, variable)
{
    # Number of rows in the dataframe
    number_of_rows <- nrow(dataframe)

    # Reference lat and lon
    lon_ok <- reference_df$lon
    lat_ok <- reference_df$lat

    # Lat and lon to be fixed
    lon_strana <- dataframe$lon
    lat_strana <- dataframe$lat

    valore <- dataframe[[variable]]

    #
    df <- data.frame(misto = paste(lon_strana,lat_strana, sep=""), valore = valore)
    print(head(df))
    # New values
    nuova_lon <- NULL
    nuova_lat <- NULL
    nuovo_val <- NULL

    for(i in 1:number_of_rows)
    {
        lon_true <- lon_ok[i]

        differ <- which(abs(lon_strana-lon_true)==min(abs(lon_strana-lon_true)))
        nuova_lon[i] <- lon_strana[differ][1]

        lat_true <- lat_ok[i]
        differ <- which(abs(lat_strana-lat_true)==min(abs(lat_strana-lat_true)))
        nuova_lat[i] <- lat_strana[differ][1]

        stringa <- paste(lon_true, lat_true, sep = "")
        index <- which(df$misto == stringa)
        #print(index)
        nuovo_val[i] <- df$valore[index]
        #print(nuovo_val[i])
    }

    new_dataframe <- data.frame(lon = nuova_lon, lat = nuova_lat, valore = nuovo_val, year = dataframe$year)
    names(new_dataframe) <- names(dataframe)
    new_dataframe <- tbl_df(new_dataframe)
    return(new_dataframe)
}
