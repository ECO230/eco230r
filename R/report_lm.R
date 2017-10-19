report_lm <- function(mod,vars) {
names <- strsplit(vars,'\\~')
y_name <- strsplit(names[[1]][[1]],'\\$')[[1]][[2]]
x_name <- strsplit(names[[1]][[2]],'\\$')[[1]][[2]]

an <- paste(c('Simple linear regression testing effect between ',y_name,'(y) and ',x_name, ' (x)'), collapse = '')

sum <- summary(mod)
Dfm <- sum$df[1]
Dfr <- sum$df[2]
Fv <- sum$fstatistic[1]
p <- sum$coefficients[2,4]
r2 <- sum$r.squared[1]
x1 <- x_name
x1t <- sum$coefficients[2,3]
x1p <- sum$coefficients[2,4]
slope <- sum$coefficients[2,1]
intercept <- sum$coefficients[1,1]
signed <- if (intercept < 0){' '} else {' + '}
res <- paste(c('F(', round(Dfm,2), ',', round(Dfr,2), ') = ', round(Fv,3),', p = ', round(p,3),', R2 = ',round(r2,3)), collapse = '')
lin <- paste(c('y = ',round(slope,2),'x',signed,round(intercept,2)), collapse = '')
cf1 <- paste(c('Predictor 1: ',x1,' (t = ',round(x1t,2),', p = ',round(x1p,2),')'), collapse = '')
coe <- data.frame(sum$coefficients)

list('analysis_type' = an,'results' = res,'linear_regression_model' = lin, 'predictors' = cf1, 'coefficients' = coe)
}
