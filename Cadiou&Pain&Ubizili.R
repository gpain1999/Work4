setwd("C:/Unizeug/Übungen/CU_AdvRegressionModels/PS4")

library("colorspace")
library("xtable")

rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

#(I)

### Contingency table
(tab1 <- with(nels, table(ses, fa.educ)))
(tab1 <- round(100*prop.table(tab1, margin = 2), 2)) # fractions indicate large differences
### Plot for simple relation
plot(ses ~ fa.educ, data = nels, col = rainbow_hcl(4), 
     main = "SES by father's education")
mosaicplot(tab1, main = "Mosaic Plot",
           xlab = "Socioeconomic status",
           ylab = "Father Education",
           las = 1,
           border = "gold",
           shade = TRUE)
### LaTeX-Code for table
print(xtable(tab1, digits = c(0, 1, 1, 1)), floating = FALSE)

### Data frame for loglinear modelling
qq1 <- as.data.frame(tab1, responseName = "N")
print(qq1)

### Saturated model
fit1 <- glm(N ~ (ses + fa.educ)^2, family = poisson, data = qq1)
summary(fit1)  

### Independence model
fit0 <- glm(N ~ ses + fa.educ, family = poisson, data = qq1)
summary(fit0)

### Deviance (likelihood-ratio) test of independence
anova(fit0, fit1, test = "LRT")

(coef1 <- coef(fit1))
coef_exp_1 <- round(exp(coef1),2)[-1]
coef_exp_1 # exponentiated coefficients

### ODDS on better ses (compared to ses = 1)
### given father's education

### fa.educ = Elementary
(oddsElem <- round(exp(coef1[paste("ses", 2:4, sep = "")]),2))

### fa.educ = High
(oddsHigh <- round(exp(coef1[paste("ses", 2:4, sep = "")] + 
                         coef1[paste("ses", 2:4, ":fa.educHigh", sep = "")]),2))

### fa.educ = College
(oddsColl <- round(exp(coef1[paste("ses", 2:4, sep = "")] + 
                         coef1[paste("ses", 2:4, ":fa.educCollege", sep = "")]),2))

### All in one table
ODDSbetterSES <- data.frame(Elementary = oddsElem, High = oddsHigh, 
                            College = oddsColl)
print(ODDSbetterSES)
print(xtable(ODDSbetterSES, digits = c(0, 1, 1, 1)), floating = FALSE)

### ODDS on better education (compared to fa.educ = Elementary)
### given family SES

### ses = 1
(odds1 <- round(exp(coef1[paste("fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 2
(odds2 <- round(exp(coef1[paste("fa.educ", c("High", "College"), sep = "")] + 
                      coef1[paste("ses2:fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 3
(odds3 <- round(exp(coef1[paste("fa.educ", c("High", "College"), sep = "")] + 
                      coef1[paste("ses3:fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 4
(odds4 <- round(exp(coef1[paste("fa.educ", c("High", "College"), sep = "")] + 
                      coef1[paste("ses4:fa.educ", c("High", "College"), sep = "")]),2))

### All in one table
ODDShigherEduc <- data.frame(ses1 = odds1, ses2 = odds2, ses3 = odds3, ses4 = odds4)
print(ODDShigherEduc)

print(xtable(ODDShigherEduc, digits = c(0, 1, 1, 1,1)), floating = FALSE)

### ODDS RATIOS (ratios of odds on higher ses compared to ses = 1
###   when comparing higher levels of education with Elementary one)
###   
### = ODDS RATIOS (ratios of odds on higher level of education 
###   compared to Elementary one)
###   when comparing higher ses with ses = 1)

ODDScollEduc = round(exp(coef1[grep(":fa.educCollege", names(coef1))] - coef1[grep(":fa.educHigh", names(coef1))]),2)
ODDScollEduc = as.data.frame(ODDScollEduc)
ODDScollEduc <- data.frame(ses1 = odds1, ses2 = odds2, ses3 = odds3, ses4 = odds4)
print(ODDShigherEduc)

print(xtable(ODDScollEduc, digits = c(0, 1)), floating = FALSE)

#(II) ses vs. fa.educ vs. region

with(nels, table(region)) # seems alright

(tab2 <- with(nels, table(ses, fa.educ, region)))
round(100*prop.table(tab2, margin = 3), 1)
qq2 <- as.data.frame(tab2, responseName = "N")

### Saturated model
fit2 <- glm(N ~ (ses + fa.educ + region)^3, family = poisson, data = qq2)
summary(fit2)

### Deviance test on the three-way interaction
drop1(fit2, test = "LRT")
coef2 = coef(fit2)

# Odds ratios (odds on higher ses compared to ses = 1) when comparing higher 
# educations with elementary ones
## --> conditional ones (given region)

## Region = Northeast (reference)
(lorNE <- c(coef2[grep("^ses[0-9]:fa.educHigh$",    names(coef2))],
            coef2[grep("^ses[0-9]:fa.educCollege$", names(coef2))]))
(orNE <- exp(lorNE))
(orNE <- matrix(round(orNE, 2), nrow = 3))
rownames(orNE) <- paste("ses", 2:4, sep = "")
colnames(orNE) <- paste("educ", c("High", "College"))

print(orNE) 

## Region = Midwest

(lorMW <- coef2[grep("^ses[0-9]:.+:regionMidwest$", names(coef2))])
(orMW <- exp(lorMW + lorNE))
(orMW <- matrix(round(orMW, 2), nrow = 3))
rownames(orMW) <- paste("ses", 2:4, sep = "")
colnames(orMW) <- paste("educ", c("High", "College"))
print(orMW) 

#p-values
pv2 <- summary(fit2)$coef[,4]
(pvChangeMW <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest",     
                        names(pv2))])

## Region = South
lorS <- coef2[grep("^ses[0-9]:.+:regionSouth$", names(coef2))]
(orS <- exp(lorS + lorNE))
(orS <- matrix(round(orS, 2), nrow = 3))
rownames(orS) <- paste("ses", 2:4, sep = "")
colnames(orS) <- paste("educ", c("High", "College"))
print(orS)

#p-values
(pvChangeS <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionSouth$",     
                       names(pv2))])

## Region = West
(lorW <- coef2[grep("^ses[0-9]:.+:regionWest$", names(coef2))])
(orW <- exp(lorW + lorNE))
(orW <- matrix(round(orW, 2), nrow = 3))
rownames(orW) <- paste("ses", 2:4, sep = "")
colnames(orW) <- paste("educ", c("High", "College"))
print(orW)

(pvChangeWE <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionWest",    
                        names(pv2))])

### Also Midwest-South, Midwest-West, South-West comparisons can be done...
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Would you know how?
# Yes, either just do some exp(lorMW-lorS) or alternatively build
# loglinear model with only e.g. Midwest and South data and use its
# coefficients and CIs

##(III)

with(nels, table(sesmed))
with(nels, table(fa.educ))
with(nels, table(fa.wrk))
with(nels, table(region))

### Logistic Model

(tab3 <- with(nels, table(ses, fa.educ, fa.wrk, region)))
qq3 <- as.data.frame(tab3, responseName = "N")

### Logit model (saturated)
fit3 <- glm(sesmed ~ (fa.educ + fa.wrk + region)^3, family = binomial, data = nels)
drop1(fit3, test = "LRT")
# three-way significant, but model with max. 
# 2-way interactions requested, so here we go

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

### (IV) sesmed vs. fa.educ, fa.wrk and region by loglinear model

### data.frame for loglinear model
(tab4 <- with(nels, table(sesmed, fa.educ, fa.wrk, region)))
qq4 <- as.data.frame(tab4, responseName = "N")

fitLoglin <- glm(N ~ sesmed*fa.educ*region + sesmed*fa.wrk + fa.educ*region*fa.wrk, family = poisson, data = qq4)
summary(fitLoglin)

OR_fitLoglin <- round(exp(coef(fitLoglin)), 2)
OR_fitLoglin_relevant <- OR_fitLoglin[grep("^sesmedOver med.:.+$",names(OR_fitLoglin))]
OR_fitLoglin_relevant

Odds_ratio_question34 = cbind(round(exp(fitFinal$coef[-1]),2),OR_fitFinal[-1])
Odds_ratio_question34 = as.data.frame(Odds_ratio_question34)
# we see that the estimated exponentiated coefficients are the same

colnames(Odds_ratio_question34) <- c("Question 3", "Question 4")
print(xtable(Odds_ratio_question34, digits = c(0, 1, 1)), floating = FALSE)

# (V) Final model 