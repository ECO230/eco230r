#' Generate output for ANOVA
#'
#'This function will generate output for Analysis of Variance. Homogeneity of Variance (HOV) assumption is tested.  If AOV is assumed aov() is performed and Tukey's HSD are used for post hoc analysis. If HOV cannot be assumed robust ANOVA t1way() from WRS2 is used to perform analysis and linear constraints are used for post hoc analysis.
#'
#' @param x A formula dependent variable ~ dependent variable.
#' @param y Optional A data frame, necessary if data frame is not referenced in formula or piped in.
#' @param tr Optional, a double indicating percentage of means to trim for robust ANOVA, default value is 0.1
#' @param min_n Optional, an int indicating minimum group size, anything smaller will be folded into an Other category, default value is 5
#' @param max_cat Optional, an int indicating maximum number of categories additional categories will be folded into an Other category, default value is 10
#'
#' @return A list of output for reporting $analysis_type, $results, $descriptive_statistics, $post_hoc_analysis
#' @export
#'
#' @examples ano_results <- ano_data %>% ano(sleeptime ~ light)
#' ano_results <- ano(sleeptime ~ light, ano_data)
#' ano_results <- ano(ano_data, sleeptime ~ light)
#' ano_results <- ano(ano_data$sleeptime ~ ano_data$light)
ano <- function(x, y = NULL, tr = .1, min_n = 5, max_cat = 10) {

  fd <- is.formula(x) * is.data.frame(y)
  df <- is.data.frame(x) * is.formula(y)
  fn <- is.formula(x) * missing(y)

  if (fd) {
    formula <- x
    data <- y
    mf <- model.frame(formula,data)
  }
  else if (df) {
    formula <- y
    data <- x
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


  if(!missing(tr) &&
     (length(tr) != 1 || !is.finite(tr) ||
      tr < 0 || tr > 1))
    stop("'tr' must be a single number between 0 and 1")

  #convert independent variable to a factor
  if(!(is.factor(mf[[2]])))
  {
  mf[[2]] <- as.factor(mf[[2]])
  print('Independent variable converted to a factor using as.factor()')
  }

  raw_fcts <- nlevels(mf[[2]])

  raw_rows <- nrow(data)

  #remove any infinite numbers any remaining nans
  mf <- mf[complete.cases(mf),]

  model_rows = nrow(mf)

  diff_rows = raw_rows - model_rows

  if(diff_rows > 0)
  {print(paste(as.character(diff_rows),'rows removed due to NA/Nan/Inf values in data.'))}

  #refactor if groups have less than minimum group size
  mf[[2]] <- forcats::fct_lump_min(mf[[2]],min=min_n)

  model_fcts = nlevels(mf[[2]])

  diff_fcts = raw_fcts - model_fcts

  if(diff_fcts > 0)
  {print(paste(as.character(diff_fcts),'categories collapsed to Other due to low n change min_n to alter'))}

  raw_fcts <- nlevels(mf[[2]])

  #refactor if groups number is higher than max_cat
  mf[[2]] <- forcats::fct_lump_n(mf[[2]],n=max_cat - 1)

  model_fcts = nlevels(mf[[2]])

  diff_fcts = raw_fcts - model_fcts

  if(diff_fcts > 0)
  {print(paste(as.character(diff_fcts),'categories collapsed to Other due to too many categories change max_cat to alter'))}


  levene <- (invisible(car::leveneTest(formula, data = mf)))
  levene_p <- levene$`Pr(>F)`[1]


  #if levene's test fails do not assume HOV
  if(!missing(levene_p) &&
     (length(levene_p) != 1 || !is.finite(levene_p) ||
      levene_p < 0 || levene_p > 1))
  {levene_p = .01}

  if (levene_p < .05) {
    aov <- WRS2::t1way(mf, tr = tr)
    ph <- WRS2::lincon(mf, tr = tr)
    Dfm <- aov$df1
    Dfr <- aov$df2
    Fv <- aov$test
    p <- aov$p.value
    w <- aov$effsize
    an <- paste('Homogeneity of Variance is not assumed, Robust One Way ANOVA for medians, ', as.character(tr), ' trimmed means, Post Hocs using linear equality constraints')
    pd <- data.frame(ph[[1]])
    lab <- pd[,1:2]
    colnames(lab) <- c('G1','G2')
    lab$G1 <- factor(lab$G1, levels = c(1:length(ph[[2]])), labels = ph[[2]])
    lab$G2 <- factor(lab$G2, levels = c(1:length(ph[[2]])), labels = ph[[2]])
    rownames(pd) <- paste(as.character(lab$G1),as.character(lab$G2),sep = '-')
    ph <- pd[,c('psihat','p.value')]
    colnames(ph) <- c('Difference','P.Value')
    ph$P.Value <- signif(ph$P.Value,3)
    tryCatch({
      bf <- '--'
      bf <- BayesFactor::anovaBF(formula,mf)
    },error=function(e) {
      print(e)
    }
    )

  } else {
    aov <- aov(mf)
    ph <- TukeyHSD(aov)
    df <- data.frame(summary(aov)[[1]])
    Dfm <- df[1,1]
    Dfr <- df[2,1]
    SSm <- df[1,2]
    SSr <- df[2,2]
    MSm <- df[1,3]
    MSr <- df[2,3]
    SSt <- SSm + SSr
    Fv <- df[1,4]
    p <- df[1,5]
    w2 <- (SSm - (Dfm * MSr))/(SSt + MSr)
    w <- sqrt(abs(w2))
    an <- 'Homogeneity of Variance is assumed, One Way ANOVA, Post Hocs using Tukeys Honest Significance Difference'
    ph <- data.frame(ph[[1]])
    ph <- ph[,c('diff','p.adj')]
    colnames(ph) <- c('Difference','P.Value')
    ph$P.Value <- signif(ph$P.Value,3)
    tryCatch({
      bf <- '--'
      bf <- BayesFactor::anovaBF(formula,mf)
    },error=function(e) {
      print(e)
    }
    )
  }

  if(typeof(bf)=='character'){
    #error in bayes factor calculation should return bf <- '-'
    byfct <- bf
    }
  else{
    byfct <- as.data.frame(bf)[1,'bf']
    byfct <- round(byfct,3)
  }

  res <- paste(c('F(', round(Dfm,2), ',', round(Dfr,2), ') = ', round(Fv,3),', p = ', round(p,3),', w = ',round(w,3),', bf10 = ',byfct), collapse = '')
  dsc <- desc_e(x = formula, y = mf, 'ano',deparse(substitute(formula)),deparse(substitute(data)))
  list('analysis_type' = an,'results' = res,'descriptive_statistics' = dsc,'post_hoc_analysis' = ph)
}
