slr <- function(formula, data = NULL) {
  #Build Model
  mod <- lm(formula = formula, data = data, na.action = na.exclude)

  #descriptives
  res_list <- report_lm(mod,deparse(formula))
  res_list
}
