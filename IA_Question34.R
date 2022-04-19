setwd("C:/Unizeug/Übungen/CU_AdvRegressionModels/PS4")

library("colorspace")
library("xtable")

rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

### Basic exploration
### ++++++++++++++++++++++++++++++++++++++++++++
with(nels, table(sesmed))
with(nels, table(fa.educ))
with(nels, table(fa.wrk))
with(nels, table(region))

### Logistic Model
### ++++++++++++++++++++++++++++++++++++++++++++

(xtab3 <- with(nels, table(ses, fa.educ, fa.wrk, region)))
qq3 <- as.data.frame(xtab3, responseName = "N")

### Logit model (saturated)
fit3 <- glm(sesmed ~ (fa.educ + fa.wrk + region)^3, family = binomial, data = nels)
drop1(fit3, test = "LRT")
  # three-way significant, but model with max. 2-way interactions requested, so here we go

### Logit model, no three-way interaction
fit3alt <- glm(sesmed ~ (fa.educ + fa.wrk + region)^2, family = binomial, data = nels)
drop1(fit3alt, test = "LRT") ##drop fa.wrk:region first

### Start with fit3alt and try to simplify it
### --> fit30alt
fit30alt <- glm(sesmed ~ (fa.educ + region)^2 + fa.educ:fa.wrk + fa.wrk, family = binomial, data = nels)
anova(fit3alt, fit30alt, test = "LRT") # not significantly worse
drop1(fit30alt, test = "LRT")
  #fa.educ:region is highly significant, could try to drop fa.educ:region

fit31alt <- glm(sesmed ~ (fa.educ + region)^2 + fa.wrk, family = binomial, data = nels)
anova(fit3alt, fit31alt, test = "LRT") # not significantly worse
anova(fit30alt, fit31alt, test ="LRT") # same, but on edge, so pay attention

drop1(fit31alt, test = "LRT") # no further dropping of interaction terms suggested
fitFinal <- fit31alt
summary(fitFinal)

# for own interest: check exponentiated coefficients and CIs
(OR_fitFinal <- round(exp(coef(fitFinal)), 2))
(CI_fitFinal <- round(exp(confint(fitFinal)), 2))
##### *************************************************************************************** #####
CI_fitFinal <- as.data.frame(CI_fitFinal)
print(xtable(CI_fitFinal, digits = c(0, 1, 1)), floating = FALSE)

### (iv) sesmed vs. fa.educ, fa.wrk and region by loglinear model
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### data.frame for loglinear model
(xtab4 <- with(nels, table(sesmed, fa.educ, fa.wrk, region)))
qq4 <- as.data.frame(xtab4, responseName = "N")

### Specify and fit the loglinear model which is equivalent to the logistic model
### found in the previous step.
# logistic model: sesmed ~ (fa.educ + region)^2 + fa.wrk
# equiv. logliner: N ~ (sesmed + fa.educ + region)^3 + (sesmed + fa.wrk)^2 + (fa.educ + region + fa.wrk)^3

fitLoglin <- glm(N ~ sesmed*fa.educ*region + sesmed*fa.wrk + fa.educ*region*fa.wrk, family = poisson, data = qq4)
summary(fitLoglin)

OR_fitLoglin <- round(exp(coef(fitLoglin)), 2)
OR_fitLoglin_relevant <- OR_fitLoglin[grep("^sesmedOver med.:.+$",names(OR_fitLoglin))]
# ! need to compare e.g. fa.educCollege:regionWest with sesmedOver med.:fa.educCollege:regionWest
OR_fitLoglin_relevant

Odds_ratio_question34 = cbind(round(exp(fitFinal$coef[-1]),2),OR_fitFinal[-1])
Odds_ratio_question34 = as.data.frame(Odds_ratio_question34)
# we see that the estimated exponentiated coefficients are the same

colnames(Odds_ratio_question34) <- c("Question 3", "Question 4")
print(xtable(Odds_ratio_question34, digits = c(0, 1, 1)), floating = FALSE)
