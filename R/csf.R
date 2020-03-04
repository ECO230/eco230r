#' Generate output for a Chi Square Test for Goodness of Fit
#'
#'This function performs a Chi Sqare test for Goodness of Fit on one factor and returns reporting information on the model.
#' @param x A formula ~Categorical Variable .
#' @param y A data frame, necessary if data frame is not referenced in formula or piped in.
#' @param probs A vector to test goodness of fit, must have same number of categories and add to 1 if percentages
#'
#' @return A list of output for reporting $analysis_type, $results, $fit_model, $observed, $expected, $standardized_residuals, $contribution, $table_percentages
#' @export
#'
#' @examples x2_results <- x2_data %>% csf(~class_rank)
#' x2_results <- x2_data %>% csf(~class_rank,probs = c(500,147,24,39,111))
#' x2_results <- x2_data %>% csf(~class_rank,probs = c(.61, .18, .03, .05, .13))
#' x2_results <- csf(x2_data$class_rank)
#' x2_results <- csf(x2_data$class_rank,probs = c(500,147,24,39,111))
csf <- function(x,y=NULL,probs=NULL) {
  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  td <- is.table(x) * is.formula(y)
  dt <- is.data.frame(x) * is.table(y)
  tn <- is.table(x) * missing(y)
  ta <- is.table(x)
  at <- is.table(y)
  va <- is.vector(x)
  av <- is.vector(y)
  fa <- is.factor(x)
  af <- is.factor(y)

  if(fd) {
    tab <- table(model.frame(x,y))
    x_name <- sub('~','',sub('~1','',deparse(substitute(x))))
  }
  else if(df) {
    tab <- table(model.frame(y,x))
    x_name <- sub('~','',sub('~1','',deparse(substitute(y))))
  }
  else if(td | tn | ta) {
    tab <- x
    x_name <- 'Category'
  }
  else if(dt | at ) {
    tab <- y
    x_name <- 'Category'
  }
  else if (va | fa){
    tab <- table(x)
    x_name <- deparse(substitute(x))
  }
  else if (av | af){
    tab <- table(y)
    x_name <- deparse(substitute(y))
  }


  if (!missing(probs)){
    px <- is.formula(probs) * is.data.frame(x)
    py <- is.formula(probs) * is.data.frame(y)
    pt <- is.table(probs)
    pv <- is.vector(probs)
    pf <- is.factor(probs)

    if(px) {
      pab <- model.frame(probs,x)
      colnames(pab) <- c(x_name,'Perc')
    }
    else if(py) {
      pab <- model.frame(probs,y)
      colnames(pab) <- c(x_name,'Perc')
    }
    else if(pt) {
      pab <- data.frame(probs)
      colnames(pab) <- c(x_name,'Perc')
    }
    else if ( pv) {
      pab <- data.frame(c(1:length(probs)),probs)
      colnames(pab) <- c('Group','Perc')
    }
    else if (pf) {
      pab <- table(probs)
      pab <- data.frame(pab)
      colnames(pab) <- c('Group','Perc')
    }

    #Convert to proportions if sum is not one
    if (sum(pab$Perc) != 1) {
      pab$Perc <- pab$Perc/sum(pab$Perc)
    }

    print(pab)

    ppc <- pab
    ppc$Perc <- round(ppc$Perc*100,2)
    ppc$Perc <- paste(ppc$Perc,'%')
    colnames(ppc) <- c('Fit_Model','Perc')

  } else {
    ppc = 'No Goodness of Fit model included'
  }


  if (!missing(probs)){
    print(pab)
    mod <- chisq.test(tab,p=pab[[2]])
  } else {
    mod <- chisq.test(tab)
  }

  Df <- mod$parameter[[1]]
  p <- mod$p.value
  Xv <- mod$statistic
  mrows <- nrow(mod$observed)

  an <- paste(c('Chi Square Test of Goodness of Fit model on x=',x_name,' (',mrows,' levels)'), collapse = '')

  obs <- data.frame(mod$observed)
  colnames(obs) <- c(x_name,'Freq')

  tab <- round(prop.table(mod$observed)*100,2)
  tab <- paste(tab,'%')
  attributes(tab) <- attributes(mod$observed)
  tab <- data.frame(tab)
  colnames(tab) <- c(x_name,'Perc')

  exp <- data.frame(mod$expected)
  colnames(exp) <- c('Freq')

  std <- data.frame(mod$stdres)
  colnames(std) <- c(x_name,'Freq')

  con <- round(100*mod$residuals^2/mod$statistic,2)
  con <- paste(con,'%')
  attributes(con) <- attributes(mod$residuals)
  con <- data.frame(con)
  colnames(con) <- c(x_name,'Perc')

  res <- paste(c('X2(', round(Df,2), ') = ', round(Xv,3),', p = ', round(p,3)), collapse = '')

  list('analysis_type' = an,'results' = res,'fit_model' = ppc, 'observed' = obs, 'expected' = exp,
       'standardized_residuals' = std, 'contribution' = con, 'table_percentages' = tab)

}
