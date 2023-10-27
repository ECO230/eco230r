#' Generate Output for Paired Samples t-test
#'
#'This function will generate the output for a paired samples t-test.
#'
#' @param x A data frame column with the first variable to compare, a formula can also be passed in (~Var or Var~1)
#' @param y A data frame column with the second variable to compare (~Var or Var~1), if a formula is passed in a data frame will be accepted
#' @param z A data frame if formulas are used in x and y and data is not piped in
#' @param tails an integer indicating whether the test is one-tailed '1' or two-tailed '2'
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics
#' @export
#'
#' @examples pst_results <- pst_data %>% pst(~scones, ~tea)
#' pst_results <- pst(pst_data$black_tea, pst_data$green_tea, tails = 1)
#' pst_results <- pst(pst_data, ~scones, ~tea)
#' pst_results <- pst(~scones, ~tea, pst_data)
#' pst_results <- pst(pst_data$black_tea, pst_data$green_tea)
#' pst_results <- pst(scones ~ tea, pst_data_tall)
pst <- function(x, y = NULL, z = NULL, tails = 2) {
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
  mod <- t.test(mf_x, mf_y, paired = TRUE)

  tryCatch({
    bf <- '--'
    bf <- BayesFactor::ttestBF(mf_x,mf_y,paired=TRUE)
  },error=function(e) {
    print(e)
  }
  )

  if (tails ==2) {
    an <- 'Paired Samples t-Test, Two Tailed test'
  } else {
    an <- 'Paired Samples t-Test, One Tailed test'
  }

  #descriptives
  res_list <- report_t(mod, tails = tails, an,bayes_factor = bf)
  if (fdn | dfn) {
    dsc <- desc_e(mf_x, mf_y, 'pst',names(mf)[1],names(mf)[2])
  }
  else if (ffd) {
    dsc <- desc_e(mf_x, mf_y, 'pst',names(model.frame(x,z))[1],names(model.frame(y,z))[1])
  }
  else if (dff) {
    dsc <- desc_e(mf_x, mf_y, 'pst',names(model.frame(y,x))[1],names(model.frame(z,x))[1])
  }
  else if (vvn | vvd) {
    dsc <- desc_e(mf_x, mf_y, 'pst',deparse(substitute(x)),deparse(substitute(y)))
  }
  list('analysis_type' = res_list[[1]], 'results' = res_list[[2]], 'descriptive_statistics' = dsc)
}
