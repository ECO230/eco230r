library(dplyr)
desc_e <- function(x,y,analysis = c("ost", "pst", "idt", "lrm"), x_name, y_name){

  if (is.formula(x)) {
    #formula passed in for x
      if (missing(y)) {
      mf <- model.frame(x)
    } else {
      mf <- model.frame(x, y)
    }
      colnames(mf) <- c('dv','iv')
  } else {
    #data passed in for x
    if (missing(y)) {
      #only x passed in
      tn <- strsplit(x_name,'\\$')
      x_name <- tn[[1]][[2]]
      tn <- strsplit(y_name,'\\$')
      y_name <- tn[[1]][[2]]
    }

  }



  if (analysis == 'pst') {
    dsc1 <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc1) <- c('N','Mean','Standard Error')
    dsc2 <- data.frame(length(y),mean(y),sqrt(var(y)/length(y)))
    colnames(dsc2) <- c('N','Mean','Standard Error')
    dsc <- rbind(dsc1,dsc2)
    row.names(dsc) <- c(x_name, y_name)
  } else if (analysis == 'ost') {
    dsc <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc) <- c('N','Mean','Standard Error')
    row.names(dsc) <- c(x_name)
  } else {
    dsc <- mf %>%
      group_by(iv) %>%
      summarize(N = signif(length(dv),3),
                Mean = signif(mean(dv),3),
                SE = signif(sqrt(var(dv)/length(dv)),3)) %>%
      data.frame
    colnames(dsc) <- c('Group','N','Mean','Standard.Error')
  }


  dsc
}

