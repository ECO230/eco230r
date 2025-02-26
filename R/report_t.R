report_t <- function(model_t,tails,analysis_desc,bayes_factor) {
  t <- model_t$statistic
  p <- model_t$p.value
  Df <- model_t$parameter
  r <- sqrt(t^2/(t^2+Df))

  if(typeof(bayes_factor)=='character'){
    #error in bayes factor calculation should return bf <- '-'
    byfct <- bayes_factor
  }
  else{
    byfct <- unname(as.vector(bayes_factor))
    byfct <- round(byfct,3)
  }

  if (tails == 1) {p <- p/2}

  res <- paste(c('t(', round(Df,2), ') = ', round(t,3),', p = ', round(p,3),', r = ',round(r,3),', bf10 = ',byfct), collapse = '')
  list('analysis_type' = analysis_desc,'results' = res)
}
