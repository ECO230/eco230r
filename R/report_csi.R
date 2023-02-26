report_csi <- function(mod,vars,bayes_factor) {
  names <- strsplit(vars,'\\~')
  y_name <- strsplit(names[[1]][[1]],'\\$')
  x_name <- strsplit(names[[1]][[2]],'\\$')
  y_name <- y_name[[1]][[length(y_name[[1]])]]
  x_name <- x_name[[1]][[length(x_name[[1]])]]

  Df <- mod$parameter[[1]]
  p <- mod$p.value
  Xv <- mod$statistic
  mcols <- ncol(mod$observed)
  mrows <- nrow(mod$observed)

  an <- paste(c('Chi Square Test of Association between ',y_name,' and ',x_name,' (',mrows,'x',mcols,' contingency)'), collapse = '')

  obs <- data.frame(mod$observed)
  colnames(obs) <- c(y_name,x_name,'Freq')
  obs <- obs %>% pivot_wider(names_from=x_name,values_from = Freq)

  if (mcols ==2 & mrows == 2) {
    mod$observed[1]
    #odds Ratio row 1
    Dv1 <- levels(droplevels((obs[[1]][[1]])))
    ORover <- colnames(obs[2])
    ORunder <- colnames(obs[3])
    OR1 <- (obs[[2]][[1]]/obs[[2]][[2]])/(obs[[3]][[1]]/obs[[3]][[2]])
    lOR1 = log(OR1)
    seOR = sqrt((1/obs[[2]][[1]]) + (1/obs[[2]][[2]]) + (1/obs[[3]][[1]]) + (1/obs[[3]][[2]]))
    OR1posLOG = lOR1 + (1.96*seOR)
    OR1negLOG = lOR1 - (1.96*seOR)
    OR1pos = exp(OR1posLOG)
    OR1neg = exp(OR1negLOG)

    #odds Ratio row 2
    Dv2 <- levels(droplevels((obs[[1]][[2]])))
    OR2 <- (obs[[2]][[2]]/obs[[2]][[1]])/(obs[[3]][[2]]/obs[[3]][[1]])
    lOR2 = log(OR2)
    OR2posLOG = lOR2 + (1.96*seOR)
    OR2negLOG = lOR2 - (1.96*seOR)
    OR2pos = exp(OR2posLOG)
    OR2neg = exp(OR2negLOG)


    OR <- data.frame(c(Dv1,Dv2)
                     ,c(OR1,OR2)
                     ,c(paste(c('(',round(OR1neg,2),',',round(OR1pos,2),')'), collapse = ''),
                        paste(c('(',round(OR2neg,2),',',round(OR2pos,2),')'), collapse = ''))
                     ,c(ORover,ORover)
                     ,c(ORunder,ORunder))

    colnames(OR) <- c(y_name,'Odds_Ratio','OR_CI','With','Than')

  } else {
    OR <- "Odd Ratio only calculated for 2x2 contingency tables."
  }


  tab <- round(prop.table(mod$observed)*100,2)
  tab <- paste(tab,'%')
  attributes(tab) <- attributes(mod$observed)
  tab <- data.frame(tab)
  colnames(tab) <- c(y_name,x_name,'Freq')
  tab <- tab %>% pivot_wider(names_from=x_name,values_from = Freq)

  col <- round(prop.table(mod$observed,2)*100,2)
  col <- paste(col,'%')
  attributes(col) <- attributes(mod$observed)
  col <- data.frame(col)
  colnames(col) <- c(y_name,x_name,'Freq')
  col <- col %>% pivot_wider(names_from=x_name,values_from = Freq)

  roe <- round(prop.table(mod$observed,1)*100,2)
  roe <- paste(roe,'%')
  attributes(roe) <- attributes(mod$observed)
  roe <- data.frame(roe)
  colnames(roe) <- c(y_name,x_name,'Freq')
  roe <- roe %>% pivot_wider(names_from=x_name,values_from = Freq)

  exp <- data.frame(mod$expected)

  std <- data.frame(mod$stdres)
  colnames(std) <- c(y_name,x_name,'Freq')
  std <- std %>% pivot_wider(names_from=x_name,values_from = Freq)


  con <- round(100*mod$residuals^2/mod$statistic,2)
  con <- paste(con,'%')
  attributes(con) <- attributes(mod$residuals)
  con <- data.frame(con)
  colnames(con) <- c(y_name,x_name,'Freq')
  con <- con %>% pivot_wider(names_from=x_name,values_from = Freq)

  if(typeof(bayes_factor)=='character'){
    #error in bayes factor calculation should return bf <- '-'
    byfct <- bayes_factor
  }
  else{
    byfct <- as.data.frame(bayes_factor)[1,'bf']
    byfct <- round(byfct,3)
  }

  res <- paste(c('X2(', round(Df,2), ') = ', round(Xv,3),', p = ', round(p,3), ', bf10 = ',byfct), collapse = '')

  list('analysis_type' = an,'results' = res,'odds_ratio' = OR, 'observed' = obs, 'expected' = exp,
       'standardized_residuals' = std, 'contribution' = con, 'table_percentages' = tab,
       'column_percentages' = col, 'row_percentages' = roe)

}
