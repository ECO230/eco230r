idt <- function(x, y = NULL, tails = 2) {

  #build model
  mod <- t.test(x, y, paired = FALSE)
  if (tails ==2) {
    an <- 'Independent t-Test, Two Tailed test'
  } else {
    an <- 'Independent t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  dsc <- desc_e(x, y, 'idt',deparse(substitute(x)),deparse(substitute(y)))
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
