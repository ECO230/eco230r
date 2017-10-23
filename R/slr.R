#' Generate output for a Simple Linear Regression
#'
#'This function performs a simple linear regression and returns reporting information on the model.
#'
#' @param formula A formula dependent varialbe ~ independent variable.
#' @param data A data frame, necessary if data frame is not referenced in formula.
#'
#' @return A list of output for reporting $analysis_type, $results, $linear_regression_model, $predictors, $coefficients
#' @export
#'
#' @examples slr_results <- slr(sales ~ adverts, sales)
#' slr_results < slr(sales$sales ~ sales$adverts)
slr <- function(formula, data = NULL) {
  #Build Model
  mod <- lm(formula = formula, data = data, na.action = na.exclude)

  #descriptives
  res_list <- report_lm(mod,deparse(formula))
  res_list
}
