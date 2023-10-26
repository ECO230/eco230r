source('R/analyze.R')
source('R/ano.R')
source('R/aov_e.R')
source('R/csf.R')
source('R/csi.R')
source('R/desc_e.R')
source('R/final_elapsed.R')
source('R/final_file.R')
source('R/final_load.R')
source('R/final_user.R')
source('R/idt.R')
source('R/idw.R')
source('R/is.formula.R')
source('R/ost.R')
source('R/pst.R')
source('R/psw.R')
source('R/report_csi.R')
source('R/report_lm.R')
source('R/report_t.R')
source('R/slr.R')
source('R/validate.R')
library(tidyverse)

#load R data sets for t test examples
load("data/t_tall.Rda")
load("data/t_wide.Rda")

#load R data sets for glm examples
load('data/napData.Rda')

#load R data sets for chi square examples
load('data/tea_tab.Rda')

#Independent t-test example
t_tall %>%
  idt(scones ~ tea)

#Independent Wilcoxon Rank-Sum test
t_tall %>%
  idw(scones ~ tea)

#Paired Samples t-test example
t_wide %>%
  pst(~black_tea,~green_tea)

#Paired Samples Wilcoxon Signed-Rank Test example
t_wide %>%
  psw(~black_tea,~green_tea)

#One Sample t-test example
t_tall %>%
  ost(~scones,mu = 4)

#Simple linear regression example
napData %>%
  slr(naptime~timestamp)

#One way ANOVA example
napData %>%
  ano(naptime ~ light)

tea_tab %>%
  csf(~cool_time)

print(csf_default)

tea_tab %>%
  csf(~cool_time,probs=c(20,50,15,15))

print(csf_model)

tea_tab %>%
  csi(finger_preference~tea_type)

tea_tab %>%
  csi(cool_time~tea_type)

