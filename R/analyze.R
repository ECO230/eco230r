analyze <- function(x, y = NULL, analysis = c("ost", "pst", "idt","aov", "lrm"), tails = 2, mu = 0, tm = 0.1) {
  if (analysis == 'pst') {
    mod <- t.test(x, y, paired = TRUE)
    if (tails ==2) {an <- 'Paired Samples t-Test, Two Tailed test'} else {an <- 'Paired Samples t-Test, One Tailed test'}
    type <- 't'
  } else if (analysis == 'idt') {
    mod <- t.test(x, y, paired = FALSE)
    if (tails ==2) {an <- 'Independent t-Test, Two Tailed test'} else {an <- 'Independent t-Test, One Tailed test'}
    type <- 't'
  } else if (analysis =='ost') {
    mod <- t.test(x, y, paired = FALSE, mu = mu)
    if (tails ==2) {an <- 'One Sample t-Test, Two Tailed test'} else {an <- 'One Sample t-Test, One Tailed test'}
    type <- 't'
  } else if (analysis == 'aov') {
    res_list <- aov_e(x)
    type <- 'a'
  } else if (analysis == 'lrm') {
    mod <- lm(x,y, na.action = na.exclude)
    res_list <- report_lm(mod,deparse(x))
    type <- 'l'
  }

  # descriptives

  if (analysis != 'lrm') {dsc <- desc_e(x, y, analysis,deparse(substitute(x)),deparse(substitute(y)))}

  #results
  if (type == 't') {
    res_list <- report_t(mod, tails = tails, an)

    list('analysis_type' = res_list[[1]],'results' = res_list[[2]],'descriptive_statistics' = dsc)

  } else if (type == 'a') {
    list('analysis_type' = res_list[[1]],'results' = res_list[[2]],'descriptive_statistics' = dsc,'post_hoc_analysis' = res_list[[3]])
  } else if (type == 'l') {
    res_list
  }
}



