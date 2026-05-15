#' Generate Output for a One Sample t-Test
#'
#'This function will generate the output for a one sample t test.
#'
#' @param x a formula ~Dependent Variable (or DV~1) or a data frame column containing the Dependent Variable
#' @param y optional, a data frame when data frame is not referenced and a formula is passed to x or piped in
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#' @param mu a double representing the value you want to comare to, the default for this parameter is 0.0
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples ost_results <- t_tall %>% ost(~scones,mu = 44)
#' ost_results <- ost(t_tall$scones, tails = 1, mu = 44)
#' ost_results <- ost(t_tall$scones ~ 1, tails = 1, mu = 44)
#' ost_results <- ost(scones ~ 1, t_tall, tails = 1, mu = 44)
#' ost_results <- ost(~scones, t_tall, mu = 44)
#' ost_results <- ost(t_tall, ~scones, mu = 44)
#' ost_results <- ost(t_tall$scones, mu = 44)
ost <- function(x, y = NULL, tails = 2, mu = 0.0) {
  fd <- is.formula(x) && is.data.frame(y)
  df <- is.data.frame(x) && is.formula(y)
  fn <- is.formula(x) && missing(y)
  vd <- is.vector(x) && is.data.frame(y)
  vn <- is.vector(x) && missing(y)

  if (fd) {
    # formula in x, data in y
    data <- y
    formula <- x
    mf <- stats::model.frame(formula, data)

  } else if (df) {
    # data in x, formula in y
    data <- x
    formula <- y
    mf <- stats::model.frame(formula, data)

  } else if (fn) {
    # formula in x, no data supplied
    formula <- x
    mf <- stats::model.frame(formula)
    data <- mf

  } else if (vd || vn) {
    # vector input
    nam <- sapply(strsplit(deparse(substitute(x)), "\\$"), utils::tail, 1)
    data <- data.frame(x)
    colnames(data) <- nam
    formula <- eval(parse(text = paste0("~", nam)))
    mf <- stats::model.frame(formula, data)

  } else {
    stop(
      "Unsupported input. Use ost(formula, data), ost(data, formula), ",
      "ost(vector), or ost(vector, data).",
      call. = FALSE
    )
  }

  nf <- mf
  nf$mu <- mu

  raw_rows <- nrow(data)

  # remove incomplete rows
  nf <- nf[stats::complete.cases(nf), , drop = FALSE]

  model_rows <- nrow(nf)
  diff_rows <- raw_rows - model_rows

  if (diff_rows > 0) {
    print(paste(as.character(diff_rows), "rows removed due to NA/Nan/Inf values in data."))
  }

  mod <- stats::t.test(nf[, 1], paired = FALSE, mu = mu)

  d <- effectsize::cohens_d(nf[, 1], mu = mu)$Cohens_d

  bf_obj <- "--"
  bf10 <- "--"

  tryCatch({
    bf_obj <- BayesFactor::ttestBF(x = nf[, 1], mu = mu)
    bf10 <- BayesFactor::extractBF(bf_obj)$bf
  }, error = function(e) {
    print(e)
  })

  if (tails == 2) {
    an <- "One Sample t-Test, Two Tailed test"
  } else {
    an <- "One Sample t-Test, One Tailed test"
  }

  res_list <- report_t(mod, tails = tails, an, bayes_factor = bf10, cohen_d = d)

  dsc <- desc_e(formula, nf[1], "ost", colnames(nf)[1], deparse(substitute(y)))

  list(
    analysis_type = res_list[[1]],
    results = res_list[[2]],
    descriptive_statistics = dsc
  )
}
