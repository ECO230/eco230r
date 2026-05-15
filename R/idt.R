#' Generate Output for an Independent t-test
#'
#'This function will generate the output for an independent t-test.
#'
#' @param x A formula dependent variable ~ dependent variable, a data frame variable can also be passed in.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in, if a data frame variable is passed in to x, a data frame variable will also be accepted.
#' @param tails An integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples idt_results <- t_tall %>% idt(scones ~ tea)
#' idt_results <- idt(t_tall$scones ~ t_tall$tea, tails = 1)
#' idt_results <- idt(scones ~ tea, t_tall)
#' idt_results <- idt(t_tall, scones ~ tea)
idt <- function(x, y = NULL, tails = 2) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  fn <- is.formula(x) * missing(y)

  if (df) {
    formula = y
    data = x
    mf <- model.frame(formula,data)
  }
  else if (fd) {
    formula = x
    data = y
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
  mod <- t.test(formula = formula, data = mf)

  # group sizes
  grp <- mf[[2]]
  group_levels <- levels(grp)

  if (length(group_levels) != 2) {
    stop("Independent t-test requires exactly 2 groups.")
  }

  g1 <- mf[[1]][grp == group_levels[1]]
  g2 <- mf[[1]][grp == group_levels[2]]

  n1 <- length(g1)
  n2 <- length(g2)

  m1 <- mean(g1, na.rm = TRUE)
  m2 <- mean(g2, na.rm = TRUE)
  sd1 <- sd(g1, na.rm = TRUE)
  sd2 <- sd(g2, na.rm = TRUE)

  sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  d <- (m1 - m2) / sp

  # Bayes factor: BF10
  bf10 <- NA_real_
  tryCatch({
    bf_obj <- BayesFactor::ttestBF(formula = formula, data = mf)
    bf10 <- BayesFactor::extractBF(bf_obj)$bf[1]
  }, error = function(e) {
    message("Bayes factor could not be computed: ", e$message)
  })

  if (tails ==2) {
    an <- 'Independent t-Test, Two Tailed test'
  } else {
    an <- 'Independent t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an, bayes_factor=bf10,cohen_d=d)
  dsc <- desc_e(formula, mf, 'idt',deparse(substitute(formula)),deparse(substitute(mf)))
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
