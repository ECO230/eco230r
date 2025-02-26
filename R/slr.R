#' Generate output for a Simple Linear Regression
#'
#'This function performs a simple linear regression and returns reporting information on the model.
#'
#' @param x A formula dependent variable ~ independent variable.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in.
#'
#' @return A list of output for reporting $analysis_type, $results, $linear_regression_model, $predictors, $coefficients
#' @export
#'
#' @examples slr_results <- sales_data %>% slr(sales ~ adverts)
#' slr_results <- slr(sales ~ adverts, sales_data)
#' slr_results <- slr(sales_data, sales ~ adverts)
#' slr_results < slr(sales_data$sales ~ sales_data$adverts)
slr <- function(x, y = NULL) {

  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  fn <- is.formula(x) * missing(y)

  if (fd) {
    formula <- x
    data <- y
    mf <- model.frame(formula,data)
  }
  else if (df) {
    formula <- y
    data <- x
    mf <- model.frame(formula,data)
  }
  else if (fn) {
    formula = x
    mf <- model.frame(formula)
    colnames(mf) <- sapply(strsplit(colnames(mf),"\\$"),tail,1) #rename by splitting $ if present
    formula <- eval(parse(text=paste0(colnames(mf)[[1]],'~',colnames(mf)[[2]])))
    data <- mf
    mf <- model.frame(formula,data)

  }

  raw_rows = nrow(data)

  #remove any infinite numbers any remaining nans
  mf <- mf[is.finite(rowSums(mf)),]

  model_rows = nrow(mf)

  diff_rows = raw_rows - model_rows

  if(diff_rows > 0)
  {print(paste(as.character(diff_rows),'rows removed due to NA/Nan/Inf values in data.'))}

  #Build Model
  mod <- lm(formula = formula, data = mf, na.action = na.exclude)

  tryCatch({
    bf <- '--'
    bf <- BayesFactor::regressionBF(formula = formula, data = mf)
  },error=function(e) {
    print(e)
  }
  )

  #descriptives
  res_list <- report_lm(mod,deparse(formula),bayes_factor=bf)
  res_list
}
