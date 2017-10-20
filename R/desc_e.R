desc_e <- function(x,y,analysis = c("ost", "pst", "idt","ano", "lrm"), x_name, y_name){

  if (is.formula(x)) {
    #formula passed in for x
      if (missing(y)) {
      mf <- model.frame(x)

    } else {
      mf <- model.frame(x, y)
    }
    if (length(colnames(mf)) == 1) {
      colnames(mf) <- c('dv')
    } else if (length(colnames(mf)) == 2) {
      colnames(mf) <- c('dv','iv')
    }

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



  if (analysis == 'pst' && !is.formula(x)) {
    dsc1 <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc1) <- c('N','Mean','Standard Error')
    dsc2 <- data.frame(length(y),mean(y),sqrt(var(y)/length(y)))
    colnames(dsc2) <- c('N','Mean','Standard Error')
    dsc <- rbind(dsc1,dsc2)
    row.names(dsc) <- c(x_name, y_name)
  } else if (analysis == 'ost' && !is.formula(x)) {
    dsc <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc) <- c('N','Mean','Standard Error')
    tn <- strsplit(x_name,'\\$')
    x_name <- tn[[1]][[2]]
    row.names(dsc) <- c(x_name)
  } else if (analysis =='ost' && is.formula(x)) {
    dsc <- data.frame(length(mf$dv),mean(mf$dv),sqrt(var(mf$dv)/length(mf$dv)))
    colnames(dsc) <- c('N','Mean','Standard Error')
    tn <- strsplit(x_name,'\\$')
    x_name <- tn[[1]][[2]]
    row.names(dsc) <- c(x_name)
  } else {
    tmp <- as.matrix(aggregate(. ~ iv, mf, function(x) c(N = signif(length(x),3),
                Mean = signif(mean(x),3),
                SE = signif(sqrt(var(x)/length(x)),3))))
    dsc <- as.data.frame(tmp)
    colnames(dsc) <- c('Group','N','Mean','Standard.Error')
  }

  dsc
}


