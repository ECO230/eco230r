#' Generate Output for Wilcoxon Signed-Rank Test
#'
#'This function will generate the output for a Wilcoxon Signed-Rank Test for non-parametric paired samples
#'
#' @param x A data frame column with the first variable to compare, a formula can also be passed in (~Var or Var~1)
#' @param y A data frame column with the second variable to compare (~Var or Var~1), if a formula is passed in a data frame will be accepted
#' @param z A data frame if formulas are used in x and y and data is not piped in
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples psw_results <- psw_data %>% psw(~scones,~tea)
#' psw_results <- psw(psw_data$black_tea, psw_data$green_tea, tails = 1)
#' psw_results <- psw(psw_data, ~scones, ~tea)
#' psw_results <- psw(~scones, ~tea, psw_data)
#' psw_results <- psw(psw_data$black_tea, psw_data$green_tea)
#' psw_results <- psw(scones ~ tea, psw_data_tall)
psw <- function(x, y = NULL, z = NULL, tails = 2) {

  fdn <- is.formula(x) * is.data.frame(y) * missing(z)
  dfn <- is.data.frame(x) * is.formula(y) * missing(z)
  ffd <- is.formula(x) * is.formula(y) * is.data.frame(z)
  dff <- is.data.frame(x) * is.formula(y) * is.formula(z)
  vvn <- is.vector(x) * is.vector(y) * missing(z)
  vvd <- is.vector(x) * is.vector(y) * is.data.frame(z)

  if (fdn) {
    #formula in x data in y
    mf <- model.frame(x,y,na.action=NULL)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
  }
  else if (dfn) {
    #data in x formula in y
    mf <- model.frame(y,x,na.action=NULL)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
  }
  else if (ffd) {
    mf_x <- model.frame(x,z,na.action=NULL)[[1]]
    mf_y <- model.frame(y,z,na.action=NULL)[[1]]
  }
  else if (dff) {
    mf_x <- model.frame(y,x,na.action=NULL)[[1]]
    mf_y <- model.frame(z,x,na.action=NULL)[[1]]
  }
  else if (vvn | vvd) {
    mf_x <- x
    mf_y <- y
  }

  djoin <- data.frame(mf_x,mf_y)

  raw_rows = nrow(djoin)

  #remove any infinite numbers any remaining nans
  djoin <- djoin[complete.cases(djoin),]

  model_rows = nrow(djoin)

  diff_rows = raw_rows - model_rows

  if(diff_rows > 0)
  {print(paste(as.character(diff_rows),' non-matching rows removed due to NA/Nan/Inf values in data.'))}

  mf_x <- djoin[[1]]
  mf_y <- djoin[[2]]

  #build model
  mod <- wilcox.test(mf_x, mf_y,paired=TRUE,correct=FALSE)

  tryCatch({
    bf <- '--'
    bf <- DFBA::dfba_wilcoxon(mf_x,mf_y,samples=10000)
  },error=function(e) {
    print(e)
  }
  )

  if (tails ==2) {
    an <- 'Wilcoxon Signed-Rank Test (Paired Samples), Two Tailed test'
  } else {
    an <- 'Wilcoxon Signed-Rank Test (Paired Samples), One Tailed test'
  }

  if(typeof(bf)=='character'){
    #error in bayes factor calculation should return bf <- '-'
    byfct <- bf
  }
  else{
    byfct <- bf$BF10
    byfct <- round(byfct,3)
  }


  V <- mod$statistic
  p <- mod$p.value
  N <- length(mf_x)
  Z <- abs(qnorm(p/2))
  r <- Z/sqrt(N)
  if (tails == 1) {p <- p/2}
  res <- paste(c('p = ', round(p,3),', r = ',round(r,3),', bf10 = ',byfct), collapse = '')

  #descriptives

  if (fdn | dfn) {
    x_name <- names(mf)[1]
    y_name <- names(mf)[2]
  }
  else if (ffd) {
    x_name <- names(model.frame(x,z))[1]
    y_name <- names(model.frame(y,z))[1]
  }
  else if (dff) {
    x_name <- names(model.frame(y,x))[1]
    y_name <- names(model.frame(z,x))[1]
  }
  else if (vvn | vvd) {
    x_name <- sapply(strsplit(deparse(substitute(x)),"\\$"),tail,1) #Split by $ if present return tail
    y_name <- sapply(strsplit(deparse(substitute(y)),"\\$"),tail,1) #Split by $ if present return tail
  }

  dsc1 <- data.frame(length(mf_x),median(mf_x))
  colnames(dsc1) <- c('N','Median')
  dsc2 <- data.frame(length(mf_x),median(mf_y))
  colnames(dsc2) <- c('N','Median')
  dsc <- rbind(dsc1,dsc2)
  row.names(dsc) <- c(x_name, y_name)

  list('analysis_type' = an, 'results' = res, 'descriptive_statistics' = dsc)

}
