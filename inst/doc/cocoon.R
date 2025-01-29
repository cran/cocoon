## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rlang)

## ----setup-real, echo = FALSE, message = FALSE--------------------------------
library(cocoon)

## ----setup-show, eval = FALSE-------------------------------------------------
# library(cocoon)

## -----------------------------------------------------------------------------
mpg_disp_corr_pearson <- cor.test(mtcars$mpg, mtcars$disp, method = "pearson")
mpg_disp_corr_spearman <- cor.test(mtcars$mpg, mtcars$disp, method = "spearman", exact = FALSE)
mpg_disp_corr_kendall <- cor.test(mtcars$mpg, mtcars$disp, method = "kendall", exact = FALSE)

## -----------------------------------------------------------------------------
ttest_gear_carb <- t.test(mtcars$gear, mtcars$carb)
ttest_gear_carb_paired <- t.test(mtcars$gear, mtcars$carb, paired = TRUE)
ttest_gear_carb_onesample <- t.test(mtcars$gear, mu = 4)
wtest_gear_carb <- wilcox.test(mtcars$gear, mtcars$carb, exact = FALSE)
wtest_gear_carb_paired <- wilcox.test(mtcars$gear, mtcars$carb, paired = TRUE, exact = FALSE)
wtest_gear_carb_onesample <- wilcox.test(mtcars$gear, mu = 4, exact = FALSE)

## -----------------------------------------------------------------------------
aov_mpg_cyl_hp <- aov(mpg ~ cyl * hp, data = mtcars)
summary(aov_mpg_cyl_hp)

## -----------------------------------------------------------------------------
lm_mpg_cyl_hp <- lm(mpg ~ cyl * hp, data = mtcars)
summary(lm_mpg_cyl_hp)
glm_am_cyl_hp <- glm(am ~ cyl * hp, data = mtcars, family = binomial)
summary(glm_am_cyl_hp)
lmer_mpg_cyl_hp <- lme4::lmer(mpg ~ hp + (1 | cyl), data = mtcars)
summary(lmer_mpg_cyl_hp)
glmer_am_cyl_hp <- lme4::glmer(am ~ hp + (1 | cyl), data = mtcars, family = binomial)
summary(glmer_am_cyl_hp)
lmer_mpg_cyl_hp2 <- lmerTest::lmer(mpg ~ hp + (1 | cyl), data = mtcars)
summary(lmer_mpg_cyl_hp2)

## -----------------------------------------------------------------------------
bf_corr <- BayesFactor::correlationBF(mtcars$mpg, mtcars$disp)
bf_ttest <- BayesFactor::ttestBF(mtcars$vs, mtcars$am)
bf_lm <- BayesFactor::lmBF(mpg ~ am, data = mtcars)

