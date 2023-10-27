#' Generate Output for a One Sample t-Test
#'
#'This function will generate the output for a one sample t test.
#'
#' @param x a formula ~Dependent Variable (or DV~1) or a data frame column containing the Dependent Variable
#' @param y optional, a data frame when data frame is not referenced and a formula is passed to x or piped in
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#' @param mu a double representing the value you want to comare to, the default for this parameter is 0.0
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples ost_results <- ost_data %>% ost(~scones,mu = 44)
#' ost_results <- ost(ost_data$scones, tails = 1, mu = 44)
#' ost_results <- ost(ost_data$scones ~ 1, tails = 1, mu = 44)
#' ost_results <- ost(scones ~ 1, ost_data, tails = 1, mu = 44)
#' ost_results <- ost(~scones, ost_data, mu = 44)
#' ost_results <- ost(ost_data, ~scones, mu = 44)
#' ost_results <- ost(ost_data$scones, mu = 44)
ost <- function(x, y = NULL, tails = 2, mu = 0.0) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  vd <- is.vector(x) * is.data.frame(y)
  vn <- is.vector(x) * missing(y)

  if (fd) {
    #formula in x data in y
    data <- y
    formula <- x
    mf <- model.frame(formula,data)
    nf <- mf
    nf$mu = mu
  }
  else if (df) {
    #data in x formula in y
    data <- x
    formula <- y
    mf <- model.frame(formula,data)
    nf <- mf
    nf$mu = mu
  }
  else if (vd|vn) {
    nam <- sapply(strsplit(deparse(substitute(x)),"\\$"),tail,1) #Get name from x split $ if present
    data <- data.frame(x)
    colnames(data) <- nam
    formula <- formula <- eval(parse(text=paste0('~',nam)))  #Build formula from name
    mf <- model.frame(formula,data)
    nf <- mf
    nf$mu = mu
  }

  raw_rows <- nrow(data)

  #remove any infinite numbers any remaining nans
  nf <- nf[complete.cases(nf),]

  model_rows <- nrow(nf)

  diff_rows <- raw_rows - model_rows

  if(diff_rows > 0)
  {print(paste(as.character(diff_rows),'rows removed due to NA/Nan/Inf values in data.'))}

  #build model
  mod <- t.test(nf[,1], paired = FALSE, mu = mu)

  tryCatch({
    bf <- '--'
    bf <- BayesFactor::ttestBF(nf[,1],nf[,2],paired=TRUE)
  },error=function(e) {
    print(e)
  }
  )

  if (tails == 2) {
    an <- 'One Sample t-Test, Two Tailed test'
  } else {
    an <- 'One Sample t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an,bayes_factor = bf)

  dsc <-desc_e(formula, nf[1], 'ost', colnames(nf)[1],deparse(substitute(y)))

  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)

}
