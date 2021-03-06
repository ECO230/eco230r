desc_e <- function(x,y,analysis = c("ost", "pst", "idt","ano", "lrm"), x_name, y_name){

  if (is.formula(x)) {
    #formula passed in for x
    if (missing(y)) {
      mf <- model.frame(x)
      #Ignore Null Values
      mf <- mf[complete.cases(mf),]

    } else {
      mf <- model.frame(x, y)
      #Ignore Null Values
      mf <- mf[complete.cases(mf),]
    }
    if (length(colnames(mf)) == 1) {
      colnames(mf) <- c('dv')
    } else if (length(colnames(mf)) == 2) {
      colnames(mf) <- c('dv','iv')
    }

  } else {
    #data passed in for x
    tn <- strsplit(x_name,'\\$')
    x_name <- tn[[1]][[length(tn[[1]])]]
    tn <- strsplit(y_name,'\\$')
    y_name <- tn[[1]][[length(tn[[1]])]]

    if (is.null(y)) {
      #Ignore Null Values
      x <- x[complete.cases(x)]
    } else {
      #Ignore Null Values
      x <- x[complete.cases(x)]
      y <- y[complete.cases(y)]
    }

  }



  if (analysis == 'pst' && !is.formula(x)) {
    dsc1 <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc1) <- c('N','Mean','Standard_Error')
    dsc2 <- data.frame(length(y),mean(y),sqrt(var(y)/length(y)))
    colnames(dsc2) <- c('N','Mean','Standard_Error')
    dsc <- rbind(dsc1,dsc2)
    row.names(dsc) <- c(x_name, y_name)
  } else if (analysis == 'ost' && !is.formula(x)) {
    dsc <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc) <- c('N','Mean','Standard_Error')
    tn <- strsplit(x_name,'\\$')
    x_name <- tn[[1]][[length(tn[[1]])]]
    row.names(dsc) <- c(x_name)
  } else if (analysis =='ost' && is.formula(x)) {
    dsc <- data.frame(length(mf),mean(mf),sqrt(var(mf)/length(mf)))
    colnames(dsc) <- c('N','Mean','Standard_Error')
    tn <- strsplit(x_name,'\\$')
    x_name <- tn[[1]][[length(tn[[1]])]]
    row.names(dsc) <- c(x_name)
  } else if (analysis == 'idt' && !is.formula(x)) {
    dsc1 <- data.frame(length(x),mean(x),sqrt(var(x)/length(x)))
    colnames(dsc1) <- c('N','Mean','Standard_Error')
    dsc2 <- data.frame(length(y),mean(y),sqrt(var(y)/length(y)))
    colnames(dsc2) <- c('N','Mean','Standard_Error')
    dsc <- rbind(dsc1,dsc2)
    row.names(dsc) <- c(x_name, y_name)
  } else {
    tmp <- as.matrix(aggregate(. ~ iv, mf, function(x) c(N = signif(length(x),3),
                                                         Mean = signif(mean(x),3),
                                                         SE = signif(sqrt(var(x)/length(x)),3))))
    dsc <- as.data.frame(tmp)
    colnames(dsc) <- c('Group','N','Mean','Standard_Error')
  }

  dsc
}
