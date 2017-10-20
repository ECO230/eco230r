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
  if (tails ==2) {
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
