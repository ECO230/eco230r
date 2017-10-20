idt <- function(formula, data = NULL, tails = 2) {

  #build model
  mod <- t.test(formula = formula, data = data, paired = FALSE)
  if (tails ==2) {
    an <- 'Independent t-Test, Two Tailed test'
  } else {
    an <- 'Independent t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an)
  dsc <- desc_e(x = formula, y = data, 'idt',deparse(substitute(formula)),deparse(substitute(data)))
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
