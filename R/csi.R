#' Generate output for a Chi Square Test of Association
#'
#'This function performs a Chi Sqare test for association on two factors and returns reporting information on the model.
#'
#' @param x A formula dependent variable ~ independent variable.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in.
#' @param z A data frame, necessary if data frame is not referenced in formula or piped in to support form var,var,df.
#'
#' @return A list of output for reporting $analysis_type, $results, $odds_ratio, $observed, $expected, $standardized_residuals, $contribution, $table_percentages, $column_percentages, $row_percentages
#' @export
#'
#' @examples x2_results <- tea_tab %>% csi(cool_time ~ tea_type)
#' x2_results <- csi(cool_time ~ tea_type, tea_tab)
#' x2_results <- csi(tea_tab$cool_time, tea_tab$tea_type)
csi <- function(x, y = NULL, z = NULL) {
  fdn <- is.formula(x) && is.data.frame(y) && missing(z)
  dfn <- is.data.frame(x) && is.formula(y) && missing(z)
  ffd <- is.formula(x) && is.formula(y) && is.data.frame(z)
  dff <- is.data.frame(x) && is.formula(y) && is.formula(z)

  # direct vector input: csi(var1, var2) or csi(var1, var2, df)
  vvn <- !is.null(x) && !is.null(y) && !is.data.frame(x) && !is.data.frame(y) &&
    !is.formula(x) && !is.formula(y) && missing(z)
  vvd <- !is.null(x) && !is.null(y) && !is.data.frame(x) && !is.data.frame(y) &&
    !is.formula(x) && !is.formula(y) && is.data.frame(z)

  if (fdn) {
    mf <- stats::model.frame(x, y)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
    formula <- x
  } else if (dfn) {
    mf <- stats::model.frame(y, x)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
    formula <- y
  } else if (ffd) {
    mf <- stats::model.frame(x, z)
    mf1 <- stats::model.frame(y, z)
    mf_x <- mf[[1]]
    mf_y <- mf1[[1]]
    formula <- paste(
      c(sub("~", "", sub("~1", "", x)), "~", sub("~", "", sub("~1", "", y))),
      collapse = ""
    )
  } else if (dff) {
    mf <- stats::model.frame(y, x)
    mf1 <- stats::model.frame(z, x)
    mf_x <- mf[[1]]
    mf_y <- mf1[[1]]
    formula <- paste(
      c(sub("~", "", sub("~1", "", y)), "~", sub("~", "", sub("~1", "", z))),
      collapse = ""
    )
  } else if (vvn || vvd) {
    mf_x <- as.factor(x)
    mf_y <- as.factor(y)
    formula <- paste(c(deparse(substitute(x)), "~", deparse(substitute(y))), collapse = "")
  } else {
    stop(
      "Unsupported input. Use csi(formula, data), csi(data, formula), ",
      "csi(var1, var2), or csi(var1, var2, data).",
      call. = FALSE
    )
  }

  mod <- stats::chisq.test(mf_x, mf_y)

  cram_v <- effectsize::cramers_v(mod)
  cv <- cram_v$Cramers_v

  bf <- "--"
  tryCatch({
    bf <- BayesFactor::contingencyTableBF(
      mod$observed,
      sampleType = "indepMulti",
      fixedMargin = "cols"
    )
  }, error = function(e) {
    print(e)
  })

  report_csi(mod, deparse(formula), bayes_factor = bf, cram_v = cv)
}
