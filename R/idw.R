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
#' @examples idw_results <- t_tall %>% idw(scones ~ tea)
#' idw_results <- idw(scones ~ tea, t_tall, tails = 1)
#' idw_results <- idw(t_tall, scones ~ tea)
#' idw_results <- idw(t_tall$scones ~ t_tall$tea)
idw <- function(x, y = NULL, tails = 2) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  fn <- is.formula(x) * missing(y)

  if (df) {
    formula = y #m_x
    data = x #m_y
    mf <- model.frame(formula,data)
  }
  else if (fd) {
    formula = x #m_x
    data = y #m_y
    mf <- model.frame(formula,data)
  }
  else if (fn) {
    formula = x
    mf <- model.frame(formula)
    colnames(mf) <- sapply(strsplit(colnames(mf),"\\$"),tail,1) #rename by splitting $ if present
    formula <- eval(parse(text=paste0(colnames(mf)[[1]],'~',colnames(mf)[[2]])))
    data <- mf
    mf <- model.frame(formula,data)

  }

  raw_rows = nrow(data)

  #convert independent variable to a factor
  if(!(is.factor(mf[[2]])))
  {
    mf[[2]] <- as.factor(mf[[2]])
    print('Independent variable converted to a factor using as.factor()')
  }

  #remove any infinite numbers any remaining nans
  mf <- mf[complete.cases(mf),]

  model_rows = nrow(mf)

  diff_rows = raw_rows - model_rows

  if(diff_rows > 0)
  {print(paste(as.character(diff_rows),'rows removed due to NA/Nan/Inf values in data.'))}

  #build model
  mod <- wilcox.test(formula=formula, data=mf)

  rb <- effectsize::rank_biserial(mf[[1]], mf[[2]])$r_rank_biserial

  # approximate Bayes factor using independent-samples t-test
  bf10 <- NA_real_
  tryCatch({
    bf_obj <- BayesFactor::ttestBF(formula = formula, data = mf)
    bf10 <- BayesFactor::extractBF(bf_obj)$bf[1]
  }, error = function(e) {
    message("Approximate Bayes factor could not be computed: ", e$message)
  })


  if (tails ==2) {
    an <- 'Wilcoxon Rank-Sum Test (Independent Samples), Two Tailed test'
  } else {
    an <- 'Wilcoxon Rank-Sum Test (Independent Samples), One Tailed test'
  }

  W <- mod$statistic
  p <- mod$p.value
  N <- length(mf[[1]])
  z <- abs(qnorm(p/2))
  r <- z/sqrt(N)
  if (tails == 1) {p <- p/2}
  res <- paste0(
    "W = ", round(W,3),
    ", ", format_p(p),
    ", r_rb = ", round(rb,3),
    ", bf10 \u2248 ", format_bf(bf10)
  )
  #descriptives
  colnames(mf) <- c('dv','iv')
  tmp <- as.matrix(aggregate(. ~ iv, mf, function(x) c(N = signif(length(x),3),
                                                       Median = signif(median(x),3))))
  dsc <- as.data.frame(tmp)
  colnames(dsc) <- c('Group','N','Median')
  list('analysis_type' = an, 'results' = res, 'descriptive_statistics' = dsc)
}
