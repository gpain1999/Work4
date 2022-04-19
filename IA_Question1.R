setwd("C:/Unizeug/Übungen/CU_AdvRegressionModels/PS4")

library("colorspace")
library("xtable")

rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

### Basic exploration
### ++++++++++++++++++++++++++++++++++++++++++++

### Marginal frequencies - seem alright
with(nels, table(ses,     useNA = "ifany"))
with(nels, table(fa.educ, useNA = "ifany"))

### (i) ses vs. fa.educ
### ++++++++++++++++++++++++++++++++++++++++++++

  ### Contingency table
(xtab1 <- with(nels, table(ses, fa.educ)))
(ptab1 <- round(100*prop.table(xtab1, margin = 2), 2)) # fractions indicate large differences
  ### Plot for simple relation
plot(ses ~ fa.educ, data = nels, col = rainbow_hcl(4), 
     main = "SES by father's education")
  ### LaTeX-Code for table
print(xtable(ptab1, digits = c(0, 1, 1, 1)), floating = FALSE)
print(xtable(xtab1, digits = c(0, 1, 1, 1)), floating = FALSE)

  ### Data frame for loglinear modelling
qq1 <- as.data.frame(xtab1, responseName = "N")
print(qq1)

  ### Saturated model
fit1 <- glm(N ~ (ses + fa.educ)^2, family = poisson, data = qq1)
summary(fit1)  

  ### Independence model
fit0 <- glm(N ~ ses + fa.educ, family = poisson, data = qq1)
summary(fit0)

### Deviance (likelihood-ratio) test of independence
anova(fit0, fit1, test = "LRT")

(be1 <- coef(fit1))
coef_exp_1 <- round(exp(be1),2)[-1]
coef_exp_1 # exponentiated coefficients

### ODDS on better ses (compared to ses = 1)
### given father's education
### ----------------------------------------
#
### fa.educ = Elementary
be1[paste("ses", 2:4, sep = "")]
(oddsElem <- round(exp(be1[paste("ses", 2:4, sep = "")]),2))
#
### fa.educ = High
be1[paste("ses", 2:4, sep = "")]
be1[paste("ses", 2:4, ":fa.educHigh", sep = "")]
(oddsHigh <- round(exp(be1[paste("ses", 2:4, sep = "")] + 
                   be1[paste("ses", 2:4, ":fa.educHigh", sep = "")]),2))
#
### fa.educ = College
be1[paste("ses", 2:4, sep = "")]
be1[paste("ses", 2:4, ":fa.educCollege", sep = "")]
(oddsColl <- round(exp(be1[paste("ses", 2:4, sep = "")] + 
                   be1[paste("ses", 2:4, ":fa.educCollege", sep = "")]),2))

### All in one table
ODDSbetterSES <- data.frame(Elementary = oddsElem, High = oddsHigh, 
                            College = oddsColl)
print(ODDSbetterSES)

print(xtable(ODDSbetterSES, digits = c(0, 1, 1, 1)), floating = FALSE)

### ODDS on better education (compared to fa.educ = Elementary)
### given family SES
### -----------------------------------------------------------------------
#
### ses = 1
(odds1 <- round(exp(be1[paste("fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 2
(odds2 <- round(exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses2:fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 3
(odds3 <- round(exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses3:fa.educ", c("High", "College"), sep = "")]),2))
#
### ses = 4
(odds4 <- round(exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses4:fa.educ", c("High", "College"), sep = "")]),2))

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
exp(be1[grep(":fa.educ", names(be1))])

### ODDS RATIOS (ratios of odds on higher ses compared to ses = 1
###   when comparing College education with High education)
###
### = ODDS RATIOS (ratios of odds on College education compared to High education 
###   when comparing higher ses with ses = 1

ODDScollEduc = round(exp(be1[grep(":fa.educCollege", names(be1))] - be1[grep(":fa.educHigh", names(be1))]),2)
ODDScollEduc = as.data.frame(ODDScollEduc)
ODDScollEduc <- data.frame(ses1 = odds1, ses2 = odds2, ses3 = odds3, ses4 = odds4)
print(ODDShigherEduc)

print(xtable(ODDScollEduc, digits = c(0, 1)), floating = FALSE)
