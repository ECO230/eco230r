#' Generate Output for Paired Samples t-test
#'
#'This function will generate the output for a one sample t test.
#'
#' @param x A data frame column with the first variable to compare, a formula can also be passed in
#' @param y A data frame column with the second variable to compare, if a formula is passed in a data frame will be accepted
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples pst_results <- pst(pst_data$black_tea, pst_data$green_tea, tails = 1)
#' pst_results <- pst(pst_data$black_tea, pst_data$green_tea, tails = 2)
#' pst_results <- pst(scones ~ tea, pst_data_tall, tails = 1)
pst <- function(x, y = NULL, tails = 2) {

  #build model
  mod <- t.test(x, y, paired = TRUE)
  if (tails ==2) {
    an <- 'Paired Samples t-Test, Two Tailed test'
    } else {
    an <- 'Paired Samples t-Test, One Tailed test'
    }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  dsc <- desc_e(x, y, 'pst',deparse(substitute(x)),deparse(substitute(y)))
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
