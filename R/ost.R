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
#' @examples ost_results <- idt_data %>% ost(~scones,mu = 44)
#' ost_results <- ost(idt_data$scones, tails = 1, mu = 44)
#' ost_results <- ost(idt_data$scones ~ 1, tails = 1, mu = 44)
#' ost_results <- ost(scones ~ 1, idt_data, tails = 1, mu = 44)
ost <- function(x, y = NULL, tails = 2, mu = 0.0) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  vd <- is.vector(x) * is.data.frame(y)
  vn <- is.vector(x) * missing(y)

  if (fd) {
    #formula in x data in y
    mf <- model.frame(x,y)
  }
  else if (df) {
    #data in x formula in y
    mf <- model.frame(y,x)
  }

  #build model
  if (fd | df) {
    mod <- t.test(mf, paired = FALSE, mu = mu)
  } else if (vd | vn) {
    mod <- t.test(x, paired = FALSE, mu = mu)
  }

  if (tails == 2) {
    an <- 'One Sample t-Test, Two Tailed test'
  } else {
    an <- 'One Sample t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  if (fd) {
    dsc <-desc_e(x, y, 'ost', colnames(mf)[1],deparse(substitute(y)))
  }
  else if (df) {
    dsc <-desc_e(y, x, 'ost', colnames(mf)[1],deparse(substitute(x)))
  }
  else if (vd){
    dsc <- desc_e(x, NULL, 'ost',deparse(substitute(x)),'NULL')
  }
  else if (vn){
    dsc <- desc_e(x, y, 'ost',deparse(substitute(x)),deparse(substitute(y)))
  }

  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)

}
