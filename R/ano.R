ano <- function(formula, data = NULL, tr = .1) {

        if (missing(data)) {
          mf <- model.frame(formula)
        } else {
          mf <- model.frame(formula, data)
        }



        if(!missing(tr) &&
           (length(tr) != 1 || !is.finite(tr) ||
            tr < 0 || tr > 1))
          stop("'tr' must be a single number between 0 and 1")

        levene <- (invisible(car::leveneTest(formula, data = data)))
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
        }
        res <- paste(c('F(', round(Dfm,2), ',', round(Dfr,2), ') = ', round(Fv,3),', p = ', round(p,3),', w = ',round(w,3)), collapse = '')
        dsc <- desc_e(x = formula, y = data, 'ano',deparse(substitute(formula)),deparse(substitute(data)))
        list('analysis_type' = an,'results' = res,'descriptive_statistics' = dsc,'post_hoc_analysis' = ph)
      }
