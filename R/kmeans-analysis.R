################################################################################
#' Approximate NAs
#'
#' This function is used to fill missing NA values using common imputation methods.
#'
#' @param data a dplyr dataframe containing the data to be standardized
#' @param exclude_variables variables to be excluded from the standardization process. A list of characters.
#' @importFrom dplyr select_ bind_cols tbl_df %>%
#' @importFrom zoo zoo na.spline
#' @return A dplyr dataframe
#' @export
#'
approximate_NAs <- function(data, exclude_variables = list("lon", "lat", "id_pixel"))
{
    # Drop columns not needed
    variables_to_drop <- lapply(exclude_variables, function(x) paste("-", x, sep = ""))
    data_to_fill <- data %>% select_(.dots = variables_to_drop)

    # Spline missing data
    for(i in 1:nrow(data))
    {
        #print(na.spline(zoo(as.numeric(a[i, 4:40]))))
        data_to_fill[i, ] <- as.numeric(na.spline(zoo(as.numeric(data_to_fill[i, ]))))
    }

    # Bind excluded columns
    filled_data <- data %>% select_(.dots = exclude_variables) %>% bind_cols(data_to_fill)

    # Return
    return(filled_data)
}

################################################################################
#' Standardize data before running kmeans
#'
#' This function is used to standardize the data before running the kmeans analysis function.
#' The output of this function is a dataframe ready to be used for the kmeans analysis.
#'
#' @param data a dplyr dataframe containing the data to be standardized
#' @param exclude_variables variables to be excluded from the standardization process. A list of characters.
#' @importFrom dplyr select_ %>% tbl_df bind_cols
#' @return A matrix
#' @export
#'
standardize_data <- function(data, exclude_variables = list("lon", "lat", "id_pixel"))
{
    # Drop columns not needed
    variables_to_drop <- lapply(exclude_variables, function(x) paste("-", x, sep = ""))
    data_to_scale <- data %>% select_(.dots = variables_to_drop)

    # Standardize variables by their range
    rge <- sapply(data_to_scale, function(x) diff(range(x)))
    scaled_data <- sweep(x = data_to_scale, MARGIN = 2, STATS = rge, FUN = "/")
    scaled_data <- tbl_df(scaled_data)

    # Bind columns
    scaled_data <- data %>% select_(.dots = exclude_variables) %>% bind_cols(scaled_data)

    # Attach rge for later use
    attr(scaled_data, "rge") <- rge

    # Return
    return(scaled_data)
}

################################################################################
#' Run kmeans analysis
#'
#' Note: this function takes as argument all the arguments used by the stats::kmeans function. Simply
#' pass the extra arguments to the function.
#'
#' @param x a dataframe or a matrix containing the data. The data should be standardized for better results
#' @param n_centers Number of clusters to be used. A vector such as 2:5
#' @param nstart how many random sets should be chosen?
#' @param seed Random seed set for reproducibility purposes. Numeric, NULL by default. (Integer)
#' @param exclude_variables variables to exclude from kmeans analysis. A list of characters
#' @param ... Other arguments accepted by the stats::kmeans function.
#' @importFrom clusterSim index.G1
#' @importFrom dplyr select_
#' @return An object of class kmeans
#'
#' @export
#'
kmeans_analysis <- function(x, n_centers, nstart = 1, seed = NULL, exclude_variables = list("lon", "lat", "id_pixel"), ...)
{
    # Drop columns not needed
    variables_to_drop <- lapply(exclude_variables, function(x) paste("-", x, sep = ""))
    x <- x %>% select_(.dots = variables_to_drop)

    # Set random seed if seed is not NULL
    if( ! is.null(seed) ){ set.seed(seed) }

    # Output list
    results <- list()

    # Run the model for each value in n_centers
    k <- 1
    for(i in n_centers)
    {
        # Apply kmeans
        model <- kmeans(x = x, centers = i, nstart = nstart, ...)
        # Add index to model results
        model$calinski_harabasz_index <- index.G1(x, model$cluster)
        # Add non-scaled centers
        model$centers_not_scaled <- model$centers * attr(x, "rge")
        # Add results to output list.
        results[[k]] <- model

        k <- k +1
    }

    # Set names to output list
    names(results) <- as.character(n_centers)

    # Return
    return(results)
}

################################################################################
#' Plot results.
#'
#' This function is used to plot the number of clusters against the Calinski-Harabasz index
#'
#' @param kmeans_results a list of results obtained from the function kmeans_analysis
#' @importFrom ggplot2 ggplot geom_point geom_line xlab ylab aes
#' @export
#'
plot_results <- function(kmeans_results)
{
    # Get the number of clusters used
    clusters <- as.numeric(names(kmeans_results))
    # Get the Calinski-Harabasz index for each number of clusters as a vector
    c_a_index <- sapply(kmeans_results, function(x) x$calinski_harabasz_index)

    # Plot results
    ggp <- ggplot(data = data.frame(x = clusters, y = c_a_index), mapping = aes(x = x, y = y)) +
            geom_point() +
            geom_line() +
            xlab("Number of clusters") +
            ylab("Calinski-Harabasz pseudo F-statistic")
    print(ggp)
}

################################################################################
#' Extract results from the kmeans analysis
#'
#' This function is used to extract the best results from the kmeans analysis.
#' The best result is considered to be the result characterized by the highest
#' Calinski-Harabasz index.
#'
#' @param kmeans_results a list of results obtained from the function kmeans_analysis
#' @return A list with the best results (as defined above) of the kmeans analysis.
#' @export
#'
extract_results <- function(kmeans_results)
{
    # Get the names of the list (number of clusters used)
    clusters <- names(kmeans_results)
    # Get the Calinski-Harabasz index for each number of clusters
    c_a_index <- sapply(kmeans_results, function(x) x$calinski_harabasz_index)
    # Select the number of clusters that maximizes the index
    selected_number <- clusters[which.max(c_a_index)]
    # Get the results
    best_results <- kmeans_results[selected_number][[1]]
    # Return
    return(best_results)
}
