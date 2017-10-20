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
