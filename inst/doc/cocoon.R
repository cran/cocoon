## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-real, echo = FALSE, message = FALSE--------------------------------
library(cocoon)
# devtools::load_all(".")

## ----setup-show, eval = FALSE-------------------------------------------------
#  library(cocoon)

## -----------------------------------------------------------------------------
mpg_disp_corr_pearson <- cor.test(mtcars$mpg, mtcars$disp, method = "pearson")
mpg_disp_corr_spearman <- cor.test(mtcars$mpg, mtcars$disp, method = "spearman", exact = FALSE)
mpg_disp_corr_kendall <- cor.test(mtcars$mpg, mtcars$disp, method = "kendall", exact = FALSE)

## -----------------------------------------------------------------------------
mpg_disp_ttest_gear_carb <- t.test(mtcars$gear, mtcars$carb)
mpg_disp_ttest_gear_carb_paired <- t.test(mtcars$gear, mtcars$carb, paired = TRUE)
mpg_disp_ttest_gear_carb_onesample <- t.test(mtcars$gear, mu = 4)
mpg_disp_wtest_gear_carb <- wilcox.test(mtcars$gear, mtcars$carb, exact = FALSE)
mpg_disp_wtest_gear_carb_paired <- wilcox.test(mtcars$gear, mtcars$carb, paired = TRUE, exact = FALSE)
mpg_disp_wtest_gear_carb_onesample <- wilcox.test(mtcars$gear, mu = 4, exact = FALSE)

## -----------------------------------------------------------------------------
bf_corr <- BayesFactor::correlationBF(mtcars$mpg, mtcars$disp)
bf_ttest <- BayesFactor::ttestBF(mtcars$vs, mtcars$am)
bf_lm <- BayesFactor::lmBF(mpg ~ am, data = mtcars)

