#' Generate Output for Paired Samples t-test
#'
#'This function will generate the output for a paired samples t-test.
#'
#' @param x A data frame column with the first variable to compare, a formula can also be passed in (~Var or Var~1)
#' @param y A data frame column with the second variable to compare (~Var or Var~1), if a formula is passed in a data frame will be accepted
#' @param z A data frame if formulas are used in x and y and data is not piped in
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples pst_results <- t_wide %>% pst(~black_tea, ~green_tea)
#' pst_results <- pst(t_wide$black_tea, t_wide$green_tea, tails = 1)
#' pst_results <- pst(t_wide, ~black_tea, ~green_tea)
#' pst_results <- pst(~black_tea, ~green_tea, t_wide)
#' pst_results <- pst(t_wide$black_tea, t_wide$green_tea)
#' pst_results <- pst(black_tea ~ green_tea, t_wide)
pst <- function(x, y = NULL, z = NULL, tails = 2) {
  fdn <- is.formula(x) && is.data.frame(y) && missing(z)
  dfn <- is.data.frame(x) && is.formula(y) && missing(z)
  ffd <- is.formula(x) && is.formula(y) && is.data.frame(z)
  dff <- is.data.frame(x) && is.formula(y) && is.formula(z)
  vvn <- is.vector(x) && is.vector(y) && missing(z)
  vvd <- is.vector(x) && is.vector(y) && is.data.frame(z)

  if (fdn) {
    mf <- stats::model.frame(x, y, na.action = NULL)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
  } else if (dfn) {
    mf <- stats::model.frame(y, x, na.action = NULL)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
  } else if (ffd) {
    mf_x <- stats::model.frame(x, z, na.action = NULL)[[1]]
    mf_y <- stats::model.frame(y, z, na.action = NULL)[[1]]
  } else if (dff) {
    mf_x <- stats::model.frame(y, x, na.action = NULL)[[1]]
    mf_y <- stats::model.frame(z, x, na.action = NULL)[[1]]
  } else if (vvn || vvd) {
    mf_x <- x
    mf_y <- y
  } else {
    stop(
      "Unsupported input. Use pst(data, ~x, ~y), pst(~x, ~y, data), or pst(var1, var2).",
      call. = FALSE
    )
  }

  djoin <- data.frame(mf_x, mf_y)
  raw_rows <- nrow(djoin)

  djoin <- djoin[stats::complete.cases(djoin), , drop = FALSE]

  model_rows <- nrow(djoin)
  diff_rows <- raw_rows - model_rows

  if (diff_rows > 0) {
    print(paste(as.character(diff_rows), " non-matching rows removed due to NA/Nan/Inf values in data."))
  }

  mf_x <- djoin[[1]]
  mf_y <- djoin[[2]]

  mod <- stats::t.test(mf_x, mf_y, paired = TRUE)

  tval <- as.numeric(mod$statistic)
  n <- length(mf_x)
  d <- tval / sqrt(n)

  bf10 <- NA_real_
  tryCatch({
    bf_obj <- BayesFactor::ttestBF(x = mf_x, y = mf_y, paired = TRUE)
    bf10 <- BayesFactor::extractBF(bf_obj)$bf[1]
  }, error = function(e) {
    message("Bayes factor could not be computed: ", e$message)
  })

  if (tails == 2) {
    an <- "Paired Samples t-Test, Two Tailed test"
  } else {
    an <- "Paired Samples t-Test, One Tailed test"
  }

  res_list <- report_t(mod, tails = tails, an, bayes_factor = bf10, cohen_d = d)

  if (fdn || dfn) {
    dsc <- desc_e(mf_x, mf_y, "pst", names(mf)[1], names(mf)[2])
  } else if (ffd) {
    dsc <- desc_e(
      mf_x, mf_y, "pst",
      names(stats::model.frame(x, z))[1],
      names(stats::model.frame(y, z))[1]
    )
  } else if (dff) {
    dsc <- desc_e(
      mf_x, mf_y, "pst",
      names(stats::model.frame(y, x))[1],
      names(stats::model.frame(z, x))[1]
    )
  } else if (vvn || vvd) {
    dsc <- desc_e(mf_x, mf_y, "pst", deparse(substitute(x)), deparse(substitute(y)))
  }

  list(
    analysis_type = res_list[[1]],
    results = res_list[[2]],
    descriptive_statistics = dsc
  )
}
