################################################################################
#' Standardize data before running kmeans
#'
#' This function is used to standardize the data before running the kmeans analysis function.
#' This function removes NAs too.
#' The output of this function is a dataframe ready to be used for the kmeans analysis.
#'
#' @param data a dplyr dataframe containing the data to be standardized
#' @param exclude_variables variables to be excluded from the standardization process. A list of characters.
#' @importFrom dplyr select_ %>% tbl_df
#' @return A matrix
#' @export
#'
standardize_data <- function(data, exclude_variables = list("lon", "lat", "id_pixel"))
{
    # Drop columns not needed
    variables_to_drop <- lapply(exclude_variables, function(x) paste("-", x, sep = ""))
    data <- data %>% select_(.dots = variables_to_drop)
    # Remove NA. Not sure.. it removes too many rows..
    data <- na.omit(data)
    # Standardize variables by their range
    rge <- sapply(data, function(x) diff(range(x)))
    scaled_data <- sweep(x = data, MARGIN = 2, STATS = rge, FUN = "/")
    scaled_data <- tbl_df(scaled_data)
    # Bind columns
    #scaled_data <- data %>% select_(.dots = exclude_variables) %>% bind_cols(scaled_data)
    # Attach rge
    attr(scaled_data, "rge") <- rge
    # Return
    return(scaled_data)
}

################################################################################
#' Select the optimal number of clusters using the Calinski-Harabasz index
#'
#' This function is used to standardize the data before running the kmeans analysis function.
#' The output of this function is a dataframe ready to be used for the kmeans analysis.
#'
#' @param x a dataframe or a matrix containing the data. The data should be standardized for better results
#' @param minC minumum number of clusters Numeric. (Integer)
#' @param maxC maximum number of clusters. Numeric. (Integer)
#' @param plot_show plot the results. Boolean.
#' @importFrom clusterSim index.G1
#' @importFrom ggplot2 ggplot geom_point geom_line xlab ylab aes
#' @return The optimal number of clusters. (The number of clusters for which the index calculated is maximum)
#' @export
#'
optimal_clusters_number <- function(x, minC = 2, maxC = 10, plot_show = FALSE)
{
    # Range of cluster numbers
    res <- numeric(maxC - minC)
    # Calculate Calinski-Harabasz for each number of cluster in the selected range.
    for (nc in minC:maxC)
    {
        res[nc - minC + 1] <- index.G1(x, kmeans(x,centers = nc)$cluster)
    }

    # Plot results if needed
    if(plot_show)
    {
        ggp <- ggplot(data = data.frame(x = 2:(length(res) + 1), y = res), mapping = aes(x = x, y = y)) +
            geom_point() +
            geom_line() +
            xlab("Number of clusters") +
            ylab("Calinski-Harabasz pseudo F-statistic")
        print(ggp)
    }

    # Optimal number of clusters
    optimal_clusters <- which.max(res) + 1
    # Return
    return(optimal_clusters)
}

################################################################################
#' Run kmeans analysis
#'
#' Note: this function takes as argument all the arguments used by the stats::kmeans function. Simply
#' pass the extra arguments after the x, n_ceters and random_seed arguments.
#'
#' @param x a dataframe or a matrix containing the data. The data should be standardized for better results
#' @param n_centers Number of clusters to be used. A vector such as 2:5
#' @param seed Random seed set for reproducibility purposes. Numeric, NULL by default. (Integer)
#' @param ... Other arguments accepted by the stats::kmeans function.
#' @importFrom clusterSim index.G1
#' @return An object of class kmeans
#' @examples
#' # mod <- kmeans_analysis(data_, n_centers = 2:3, seed = 100)
#' @export
#'
kmeans_analysis <- function(x, n_centers, seed = NULL, ...)
{
    # Set random seed if seed is not NULL
    if( ! is.null(seed) ){ set.seed(seed) }

    # Output list
    results <- list()

    # Run the model for each value in n_centers
    k = 1

    for(i in n_centers)
    {
        # Apply kmeans
        model <- kmeans(x = x, centers = i, ...)
        # Calculate Calinski-Harabasz index
        calinski_harabasz_index <- index.G1(x, model$cluster)
        # Add index to model results
        model$calinski_harabasz_index <- calinski_harabasz_index
        # Scale back data?? Chiedere.
        model$centers <- model$centers * attr(x, "rge")
        # Add results to output list
        results[[k]] <- model

        k = k + 1
    }

    # Set names to output list
    names(results) <- sapply(n_centers, function(x) paste("n_centers_", x, sep=""))

    # Return
    return(results)
}
