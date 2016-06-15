################################################################################
#' Fit a random forest model
#'
#' @param formula A formula for the random forest such as y~x.
#' @param data Data to be used.
#' @param ntree Number of trees to grow. This should not be set to too small.
#' @param na.action A function to specify the action to be taken if NAs are found
#' @param importance Should importance of predictors be assessed? Boolean
#' @param seed Random seed for reproducibility purposes.
#' @param ... Other arguments passed to the randomForest::randomForest function
#' @importFrom randomForest randomForest
#' @export
#'
fit_random_forest <- function(formula, data, ntree = 1000, na.action = na.omit, importance = TRUE, seed = NULL, ...)
{
    # Set random seed
    if( !is.null(seed) ){ set.seed(seed) }

    # Fit a random forest model
    model_fit <- randomForest(formula = formula,
                              data = data,
                              ntree = ntree,
                              na.action = na.action,
                              importance = importance,
                              ...)
    # Return
    return(model_fit)
}

################################################################################
#' Get variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @importFrom randomForest importance
#' @export
#'
get_variable_importance <- function(rf_model)
{
    # Get variable importance
    var_imp <- importance(rf_model)
    # Return
    return(var_imp)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param title plot title
#' @param sort sort variables in order of variable importance? Boolean
#' @param ... other arguments
#' @importFrom randomForest varImpPlot
#' @export
#'
variable_importance_plot <- function(rf_model, title = "Variable importance plot", sort = TRUE, ...)
{
    # Plot variable importance
    varImpPlot(rf_model, main = title, sort = sort, ...)
}

################################################################################
#' Plot model error vs number of trees
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param title plot title
#' @export
#'
plot_error_vs_trees <- function(rf_model, title)
{
    # Model error vs number of trees
    plot(rf_model, main = title)
}






# Non va
################################################################################
#' Partial dependence plot
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used to fit the random forest model
#' @importFrom randomForest partialPlot importance
#' @export
#'
partial_dependence_plot <- function(rf_model, data)
{
    data <- as.data.frame(data)
    imp <- importance(rf_model)
    impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]

    data_out <- list()

    var.env <- new.env()

    for (i in seq_along(impvar))
    {
        var <- impvar[i]
        print(eval(var, envir = environment()))
        partial_dep_data <- partialPlot(rf_model,
                    pred.data = data,
                    x.var = eval(var, envir = environment()), #eval(var, envir = environment()),
                    n.pt = 10,
                    plot=F)

        data_out[var] <- partial_dep_data
    }

    return(data_out)
}

# Non va
################################################################################
#' Plot predictive map
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used to fit the random forest model
#' @importFrom randomForest partialPlot importance
#' @importFrom ggplot2 ggplot aes geom_tile stat_contour
#' @export
#'
predictive_map <- function(rf_model, data)
{
    x <- unique(data$lon)
    y <- unique(data$lat)
    #z__ <- predict(rf_model, data)# Bisognerebbe fare una griglia
    grid <- expand.grid(x,y)
    grid$value <- as.numeric(1:754)
    print(head(grid))
    print(dim(grid))

    ggp <- ggplot(grid, aes(x = x, y = y, z = value)) +
        geom_tile(aes(fill = value)) +
        stat_contour()

    print(ggp)
}
