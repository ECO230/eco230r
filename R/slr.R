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
#' @examples slr_results <- napData %>% slr(naptime ~ timestamp)
#' slr_results <- slr(naptime ~ timestamp, napData)
#' slr_results <- slr(napData, naptime ~ timestamp)
#' slr_results <- slr(napData$naptime ~ napData$timestamp)
slr <- function(x, y = NULL) {
  fd <- is.formula(x) && is.data.frame(y)
  df <- is.data.frame(x) && is.formula(y)
  fn <- is.formula(x) && missing(y)

  if (fd) {
    formula <- x
    data <- y
    mf <- stats::model.frame(formula, data)
  } else if (df) {
    formula <- y
    data <- x
    mf <- stats::model.frame(formula, data)
  } else if (fn) {
    formula <- x
    mf <- stats::model.frame(formula)
    colnames(mf) <- sapply(strsplit(colnames(mf), "\\$"), utils::tail, 1)
    formula <- eval(parse(text = paste0(colnames(mf)[[1]], "~", colnames(mf)[[2]])))
    data <- mf
    mf <- stats::model.frame(formula, data)
  } else {
    stop(
      "Unsupported input. Use slr(formula, data), slr(data, formula), or slr(y ~ x).",
      call. = FALSE
    )
  }

  raw_rows <- nrow(data)

  keep <- stats::complete.cases(mf)
  for (j in seq_along(mf)) {
    if (is.numeric(mf[[j]])) {
      keep <- keep & is.finite(mf[[j]])
    }
  }

  mf <- mf[keep, , drop = FALSE]

  if (ncol(mf) != 2) {
    stop("slr() requires exactly one dependent variable and one independent variable.")
  }

  model_rows <- nrow(mf)
  diff_rows <- raw_rows - model_rows

  if (diff_rows > 0) {
    print(paste(as.character(diff_rows), "rows removed due to NA/Nan/Inf values in data."))
  }

  mod <- stats::lm(formula = formula, data = mf, na.action = stats::na.exclude)

  bf10 <- NA_real_

  tryCatch({
    bf_obj <- BayesFactor::regressionBF(formula = formula, data = mf)
    bf10 <- BayesFactor::extractBF(bf_obj)$bf[1]
  }, error = function(e) {
    message("Bayes factor could not be computed: ", e$message)
  })

  report_lm(mod, deparse(formula), bayes_factor = bf10)
}
