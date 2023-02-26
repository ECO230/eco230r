#' Generate Output for an Wilcoxon Rank-Sum Test
#'
#'This function will generate the output for an Wilcoxon rank-sum test for independent non-parametric samples
#'
#' @param x A formula dependent variable ~ dependent variable, a data frame variable can also be passed in.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in, if a data frame variable is passed in to x, a data frame variable will also be accepted.
#' @param tails An integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples idw_results <- idt_data %>% idt(scones ~ tea)
#' idt_results <- idt(scones ~ tea, idt_data, tails = 1)
idw <- function(x, y = NULL, tails = 2) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)

  if (df) {
    m_x = y
    m_y = x
    mf <- model.frame(y,x)
  }
  else if (fd) {
    m_x = x
    m_y = y
    mf <- model.frame(x.y)
  }

  #build model
  mod <- wilcox.test(m_x, m_y, paired = FALSE)

  tryCatch({
    bf <- '--'
    library(bayesWilcoxTest)
    bf <- bayes.wilcox.test(m_x, m_y, paired = FALSE)
  },error=function(e) {
    print(e)
  }
  )


  if (tails ==2) {
    an <- 'Wilcoxon Rank-Sum Test (Independent Samples), Two Tailed test'
  } else {
    an <- 'Wilcoxon Rank-Sum Test (Independent Samples), One Tailed test'
  }

  if(typeof(bf)=='character'){
    #error in bayes factor calculation should return bf <- '-'
    byfct <- bf
  }
  else{
    byfct <- summary(bf)[7]
    byfct <- round(byfct,3)
  }

  W <- mod$statistic
  p <- mod$p.value
  N <- length(mf[[1]])
  z <- abs(qnorm(p/2))
  r <- z/sqrt(N)
  if (tails == 1) {p <- p/2}
  res <- paste(c('W = ', round(W,3),', p = ', round(p,3),', r = ',round(r,3),', bf10 = ',byfct), collapse = '')
  #descriptives
  colnames(mf) <- c('dv','iv')
  tmp <- as.matrix(aggregate(. ~ iv, mf, function(x) c(N = signif(length(x),3),
                                                       Median = signif(median(x),3))))
  dsc <- as.data.frame(tmp)
  colnames(dsc) <- c('Group','N','Median')
  list('analysis_type' = an, 'results' = res, 'descriptive_statistics' = dsc)
}
