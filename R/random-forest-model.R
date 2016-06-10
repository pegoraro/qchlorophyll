#randomForest()
#varImpPlot()

################################################################################
#' Fit a random forest model
#'
#' @param formula A formula for the random forest such as y~x.
#' @param data data to be used
#' @param na.action what action should be performed on missing data?
#' @param ntree
#' @param ... other arguments passed to the randomForest::random
#' @importFrom randomForest randomForest
#' @export
#'
fit_random_forest <- function(formula, data, na.action = na.action = na.fail, ntree = 500, ...)
{
    # Fit a random forest model
    model_fit <- randomForest(formula = formula,
                              data = data,
                              na.action = na.action,
                              ntree = ntree,
                              ...)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest
#' @param ... other arguments to the function varImpPlot
#' @importFrom randomForest varImpPlot
#' @export
#'
variable_importance_plot <- function(rf_model, ...)
{
    varImpPlot(x = rf_model, ...)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data dataframe used for contructing the plot (the training data used to contruct the random forest).
#' @param var name of the variable for which partial dependence is to be examined.
#' @param ... other arguments to the function varImpPlot.
#' @importFrom randomForest partialPlot
#' @export
#'
marginal_effects_plot <- function(rf_model, data, var, ...)
{
    partialPlot(x = rf_model, pred.data = data, x.var = var, ...)
}
