################################################################################
#' Load a single .csv file and output a dplyr dataframe
#'
#' @param file_path path to .csv file
#' @param ... other arguments to read.csv
#' @importFrom dplyr tbl_df %>%
#'
load_a_single_csv <- function(file_path, ...)
{
    # Load .csv file
    data <- read.csv(file = file_path, ...)
    # Convert dataframe into a dplyr dataframe
    data <- tbl_df(data)
    # Return
    return(data)
}

################################################################################
#' Load a bunch of csv files into a single list
#'
#' This function loads a bunch of .csv files with identical structure and recurrent pattern
#' into a single list. Each .csv file is assumed to have a recurrent pattern in its name.
#'
#' @param main_folder_path path to the main folder
#' @param folder name of the folder containing the .csv files to load. A character.
#' @param starts_with initial part of the name of the .csv file. Each file is assumed to have a recurrent pattern
#' in its name where the only changing part is assumed to be the year. A character
#' @param npixels Number of pixels in each dataframe. (number of rows of data)
#' @param years years to load. A character vector.
#' @param must_have_variables a vector of variables that must be included in each .csv file. Character vector.
#' @importFrom dplyr tbl_df mutate %>%
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
    nrows <- sapply(tbls, function(x) nrow(x))
    # Check that each dataframe has 5520 rows (pixels)
    if(any(nrows != npixels)){ stop(paste("Some dataframes contain more or less pixels than the selected value of", npixels, sep = " ")) }
    # Check that each dataframe contains "lon" and "lat" (must_have_variables)
    names_df <- lapply(tbls, function(x) names(x))
    names_bool <- lapply(names_df, function(x) must_have_variables %in% x)
    if(any(!unlist(names_bool))){ stop("Some of the required variables are missin from the input .csv files") }

    ####
    # Adding year information
    ####

    # Add year to each dataframe
    for(i in 1:length(years))
    {
        tbls[[i]] <- tbls[[i]] %>% mutate(year = as.numeric(years[i]))
    }

    # Return
    return(tbls)
}

################################################################################
#' Add id_pixel and kmeans groups from a reference dataframe. Make a left join.
#'
#' For each dataframe in the data_frame list, id_pixel and the group from the kmeans analysis
#' is added. Note that the reference dataframe must contain longitude and latitude (lon and lat).
#'
#' @param dataframe_list a list of dplyr dataframes
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @param join_by variables to join by
#' @importFrom dplyr left_join
#' @export
#'
add_id_pixel_and_groups <- function(dataframe_list, reference_dataframe, join_by = c("lon","lat"))
{
    # left_join on each dataframe and the reference_dataframe
    out <- lapply(dataframe_list, function(x) left_join(x, reference_dataframe, by = join_by))
    # Return
    return(out)
}

################################################################################
#' Fix latitude and longitude
#'
#' Align longitude and latitude values.
#'
#' If reformat = TRUE a "brute force" reformat is performed to match those in the reference_df.
#' If shift = TRUE the values of longitude and latitude are assumed to be shifted by
#' a negligible amount and replaced with those in the reference dataframe.
#'
#' @param df_list a list of dplyr dataframe
#' @param reference_df reference dataframe containing lon, lat, id_pixel, and groups.
#' @param variable variable to be used in the reformatting process
#' @param reformat Should lon and lat be fixed from scratch? Boolean.
#' @param shift Should lon and lat be shifted? Boolean
#' @export
#'
format_lon_lat_list <- function(df_list, reference_df, variable=NULL, reformat = FALSE, shift = FALSE)
{
    # Do lon and lat in the df list need to be reformatted?
    if(reformat)
    {
        # Reformat
        out <- lapply(df_list, format_lon_lat, reference_df, variable)
    }
    # Do they need to be shifted? (Fix if else)
    else if(shift)
    {
        # Shift
        out <- lapply(df_list, shift_lon_lat, reference_df)
    }
    else
    {
        # Do nothing
        out <- df_list
    }
    # Return
    return(out)
}

################################################################################
#' Shift lon and lat
#'
#' Simply replace lon and lat with those in the reference dataframe
#'
#' @param dataframe a dplyr dataframe
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @export
#'
shift_lon_lat <- function(dataframe, reference_dataframe)
{
    # Replace lon and lat with the appropriate one from the reference df
    # Shiftare per shiftare forse conviene mettere già quelle finali...
    dataframe$lon <- reference_dataframe$lon
    dataframe$lat <- reference_dataframe$lat
    # Return
    return(dataframe)
}

################################################################################
#' Fix latitude and longitude
#'
#' A brute force approach is used to match latitude and longitude pairs and the corrisponding value of
#' variable.
#'
#' @param dataframe a dplyr dataframe
#' @param reference_df reference dataframe containing lon, lat, id_pixel, and groups.
#' @param variable variable to be matched for each pair of longitude and latitude coordinates
#' @importFrom dplyr tbl_df
#' @export
#'
format_lon_lat <- function(dataframe, reference_df, variable)
{
    # Number of rows in the current dataframe. Should always equal 5520.
    number_of_rows <- nrow(dataframe)

    # Reference lat and lon
    lon_riferimento <- reference_df$lon
    lat_riferimento <- reference_df$lat

    # Lat and lon to be fixed
    lon_disordinata <- dataframe$lon
    lat_disordinata <- dataframe$lat

    # Variable to be matched
    valore <- dataframe[[variable]]

    # Idea: siccome ogni combinazione lon-lat e' unica, combino le coppie lon e lat in un'unica variabile.
    # df: dataframe con termine misto: lonlat valore
    # In df lonlat e' un character.
    df <- data.frame(misto = paste(lon_disordinata, lat_disordinata, sep = ""),
                     valore = valore)

    # New values
    nuovo_val <- NULL

    # Estremamente lento
    for(i in 1:number_of_rows)
    {
        #
        lon_true <- lon_riferimento[i]
        # Quale dei valori della nuova longitudine differisce di meno in valore assoluto da
        # quella di riferimento?
        differ_index <- which( abs(lon_disordinata - lon_true) == min( abs(lon_disordinata - lon_true)) )
        nuova_lon <- lon_disordinata[differ_index][1]

        lat_true <- lat_riferimento[i]
        differ_index <- which( abs(lat_disordinata - lat_true) == min( abs(lat_disordinata - lat_true)) )
        nuova_lat <- lat_disordinata[differ_index][1]

        # Incolla le lat e lon trovate, poi trova indice della riga dove ho la stessa coppia di valori
        # lon-lat nel dataframe originale. Assegna valore della variabile corrispondente al nuovo vettore.
        stringa <- paste(nuova_lon, nuova_lat, sep = "")
        index <- which(df$misto == stringa)
        nuovo_val[i] <- df$valore[index]
    }

    # Dataframe reformatted
    new_dataframe <- data.frame(lon = lon_riferimento,
                                lat = lat_riferimento,
                                valore = nuovo_val,
                                year = dataframe$year)
    # Set names
    names(new_dataframe) <- names(dataframe)
    # Convert to a dplyr dataframe
    new_dataframe <- tbl_df(new_dataframe)
    # Return
    return(new_dataframe)
}
