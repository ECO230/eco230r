report_t <- function(model_t,tails,analysis_desc,bayes_factor) {
  t <- model_t$statistic
  p <- model_t$p.value
  Df <- model_t$parameter
  r <- sqrt(t^2/(t^2+Df))
  if (tails == 1) {p <- p/2}
  byfact <- unname(as.vector(bayes_factor))
  res <- paste(c('t(', round(Df,2), ') = ', round(t,3),', p = ', round(p,3),', r = ',round(r,3),' bf = ',round(byfact,3)), collapse = '')
  list('analysis_type' = analysis_desc,'results' = res)
}
