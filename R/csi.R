#' Generate output for a Chi Square Test of Association
#'
#'This function performs a Chi Sqare test for association on two factors and returns reporting information on the model.
#'
#' @param x A formula dependent variable ~ independent variable.
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in.
#'
#' @return A list of output for reporting $analysis_type, $results, $odds_ratio, $observed, $expected, $standardized_residuals, $contribution, $table_percentages, $column_percentages, $row_percentages
#' @export
#'
#' @examples x2_results <- x2_data %>% csi(class_rank ~ gender)
#' x2_results <- csi(class_rank ~ gender, x2_data)
#' x2_results <- csi(x2_data$class_rank, x2_data$gender)
csi <- function(x,y=NULL,z=NULL) {
  fdn <- is.formula(x) * is.data.frame(y) * missing(z)
  dfn <- is.data.frame(x) * is.formula(y) * missing(z)
  ffd <- is.formula(x) * is.formula(y) * is.data.frame(z)
  dff <- is.data.frame(x) * is.formula(y) * is.formula(z)
  vvn <- is.factor(x) * is.factor(y) * missing(z)
  vvd <- is.factor(x) * is.factor(y) * is.data.frame(z)

  if (fdn) {
    #formula in x data in y
    mf <- model.frame(x,y)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
    formula <- x
  }
  else if (dfn) {
    #data in x formula in y
    mf <- model.frame(y,x)
    mf_x <- mf[[1]]
    mf_y <- mf[[2]]
    formula <- y
  }
  else if (ffd) {
    mf = model.frame(x,z)
    mf1 = model.frame(y,z)
    mf_x <- mf[[1]]
    mf_y <- mf1[[1]]
    formula <- paste(c(sub('~','',sub('~1','',x)),'~',sub('~','',sub('~1','',y))),collapse = '')
  }
  else if (dff) {
    mf = model.frame(y,x)
    mf1 = model.frame(z,x)
    mf_x <- mf[[1]]
    mf_y <- mf1[[1]]
    formula <- paste(c(sub('~','',sub('~1','',y)),'~',sub('~','',sub('~1','',z))),collapse = '')
  }
  else if (vvn | vvd) {
    mf_x <- x
    mf_y <- y
    formula <- paste(c(deparse(substitute(x)),'~',deparse(substitute(y))),collapse = '')
  }

  #build model
  mod = chisq.test(mf_x,mf_y)

  #descriptives
  res_list <- report_csi(mod,deparse(formula))
  res_list

}
