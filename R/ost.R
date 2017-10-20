#' Generate Output for a One Sample t-Test
#'
#'This function will generate the output for a one sample t test.
#'
#' @param x a formula Dependent Variable ~ 1 or a data frame column containing the Dependent Variable
#' @param y optional, a data frame when data frame is not referenced when a formula is passed to x
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#' @param mu a double representing the value you want to comare to, the default for this parameter is 0.0
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples ost_results <- ost(idt_data$scones, tails = 1, mu = 44)
#' ost_results <- ost(idt_data$scones ~ 1, tails = 1, mu = 44)
#' ost_results <- ost(scones ~ 1, idt_data, tails = 1, mu = 44)
ost <- function(x, y = NULL, tails = 2, mu = 0.0) {
  form <- is.formula(x)

  if (form == TRUE) {
    #formula passed in for x
    if (missing(y)) {
      mf <- model.frame(x)
    } else {
      mf <- model.frame(x, y)
    }
  }
  #build model
  if (form == TRUE) {
    mod <- t.test(mf, paired = FALSE, mu = mu)
  } else {
    mod <- t.test(x, y, paired = FALSE, mu = mu)
  }
  if (tails == 2) {
    an <- 'One Sample t-Test, Two Tailed test'
  } else {
    an <- 'One Sample t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  if (form == TRUE) {
  dsc <-desc_e(x, y, 'ost', colnames(mf)[1],deparse(substitute(y)))
  } else {
  dsc <- desc_e(x, y, 'ost',deparse(substitute(x)),deparse(substitute(y)))
  }

  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
