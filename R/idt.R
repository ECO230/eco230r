#' Generate Output for an Independent t-test
#'
#'This function will generate the output for an independent t-test.
#'
#' @param x A formula dependent variable ~ dependent variable, a data frame variable can also be passed in.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in, if a data frame variable is passed in to x, a data frame variable will also be accepted.
#' @param tails An integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples idt_results <- idt_data %>% idt(scones ~ tea)
#' idt_results <- idt(idt_data$scones ~ idt_data$tea, tails = 1)
#' idt_results <- idt(scones ~ tea, idt_data)
#' idt_results <- idt(idt_data$scones_black, idt_data$scones_green, tails = 1)
idt <- function(x, y = NULL, tails = 2) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)

  if (df) {
    m_x = y
    m_y = x
  }
  else if (fd) {
    m_x = x
    m_y = y
  }

  #build model
  mod <- t.test(m_x, m_y, paired = FALSE)
  bf <- ttestBF(formula = m_x, data = m_y)
  if (tails ==2) {
    an <- 'Independent t-Test, Two Tailed test'
  } else {
    an <- 'Independent t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an, bayes_factor=bf)
  dsc <- desc_e(m_x, m_y, 'idt',deparse(substitute(m_x)),deparse(substitute(m_y)))
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
