################################################################################
#' Fit a random forest model
#'
#' @param formula A formula for the random forest such as y~x.
#' @param data Data to be used.
#' @param ntree Number of trees to grow. This should not be set to too small .
#' @param na.action taken if the data contains NA's. Possible values are na.omit or na.impute.
#' The default na.omit removes the entire record if even one of its entries is NA
#'  (for x-variables this applies only to those specifically listed in 'formula').
#'  Selecting na.impute imputes the data. See documentation of randomForestSRC::rfsrc for
#'  more details regarding missing data imputation.
#' @param importance Method for computing variable importance (VIMP).
#' Calculating VIMP can be computationally expensive when the number of variables is high.
#' The defalut action is importance="none".
#' @param seed Random seed for reproducibility purposes.
#' @param ... Other arguments passed to the randomForestSRC::rfsrc
#' @importFrom randomForestSRC rfsrc
#' @export
#'
fit_random_forest <- function(formula, data, ntree = 1000, na.action = "na.impute", importance = "none", seed = NULL, ...)
{
    # Set random seed
    if(! is.null(seed)){ set.seed(seed) }

    # Convert data to a suitable format for rfsrc
    data <- as.data.frame(data)

    # Fit a random forest model
    model_fit <- rfsrc(formula = formula,
                              data = data,
                              ntree = ntree,
                              na.action = na.action,
                              importance = importance,
                              ...)
    # Return
    return(model_fit)
}

################################################################################
#' Plot OOB prediction estimates
#'
#' Extract the OOB prediction estimates from the random forest and plot them.
#'
#' @param rf_model random forest model obtained from the function fit_random_forest
#' @importFrom ggRandomForests gg_rfsrc
#' @export
#'
raw_prediction_plot <- function(rf_model)
{
    pred_estimates <- gg_rfsrc(rf_model)
    plot(pred_estimates, alpha = .5)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @importFrom ggRandomForests gg_vimp
#' @export
#'
variable_importance_plot <- function(rf_model)
{
    var_imp_p <- gg_vimp(rf_model)
    plot(var_imp_p)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @importFrom ggRandomForests gg_minimal_depth
#' @export
#'
minimal_depth_plot <- function(rf_model)
{
    gg_md <- gg_minimal_depth(rf_model)
    plot(gg_md)
}

################################################################################
#' Partial dependence plot
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used in fitting the random forest model
#' @importFrom randomForestSRC plot.variable
#' @export
#'
partial_dependence_plot <- function(rf_model, data)
{
    ## Da sistemare
    xvar <- names(data)
    xvar <- c("sst", "sic", "gruppo")
    partial_plot <- plot.variable(model,
                                    xvar = names(final_df_no_na),
                                    partial=TRUE,
                                    sorted=FALSE,
                                    show.plots = TRUE,
                                    smooth.lines = TRUE)
}
