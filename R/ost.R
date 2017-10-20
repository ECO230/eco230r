ost <- function(x, y = NULL, tails = 2, mu = 0.0) {

  #build model
  mod <- t.test(x, y, paired = FALSE, mu = mu)
  if (tails ==2) {
    an <- 'One Sample t-Test, Two Tailed test'
  } else {
    an <- 'One Sample t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  dsc <- desc_e(x, y, 'ost',deparse(substitute(x)),deparse(substitute(y)))

  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
