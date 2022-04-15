### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
rm(list=ls())
print(load("AdvRegr_4_nels.RData"))


### Basic exploration
### ++++++++++++++++++++++++++++++++++++++++++++

### Marginal frequencies
with(nels, table(ses,     useNA = "ifany"))
with(nels, table(sesmed,  useNA = "ifany"))
with(nels, table(parents, useNA = "ifany"))
with(nels, table(foreign, useNA = "ifany"))
with(nels, table(fa.educ, useNA = "ifany"))
with(nels, table(mo.educ, useNA = "ifany"))
with(nels, table(region,  useNA = "ifany"))
with(nels, table(fa.wrk,  useNA = "ifany"))
with(nels, table(mo.wrk,  useNA = "ifany"))


### (i) ses vs. fa.educ
### ++++++++++++++++++++++++++++++++++++++++++++

### Contingency table
(xtab1 <- with(nels, table(ses, fa.educ)))

### Exploration: column proportions
prop.table(xtab1, margin = 2)

### Formatted numbers
(ptab1 <- round(prop.table(xtab1, margin = 2) * 100, 1))

### Table in LaTeX 
print(xtable(ptab1, digits = c(0, 1, 1, 1)), floating = FALSE)

### Plot (two slightly different versions)
par(mfrow = c(1, 2), mar = c(3, 3, 3, 1) + 0.1)
plot(t(xtab1), col = rainbow_hcl(4), main = "SES by father's education")
plot(ses ~ fa.educ, data = nels, col = rainbow_hcl(4), 
     main = "SES by father's education")
par(mfrow = c(1, 1))

### Chi^2 test
chisq.test(xtab1)

### Data frame for loglinear modelling
(qq1 <- as.data.frame(xtab1, responseName = "N"))

### Saturated model
fit1 <- glm(N ~ (ses + fa.educ)^2, family = poisson, data = qq1)
fit1 <- glm(N ~ ses + fa.educ + ses:fa.educ, family = poisson, data = qq1)  
## the same as above
summary(fit1)         ### Surprised by (numerically) zero residual deviance?   

### Independence model
fit0 <- glm(N ~ ses + fa.educ, family = poisson, data = qq1)
summary(fit0)


### Deviance (likelihood-ratio) test of independence
anova(fit0, fit1, test = "LRT")

### Score test of independence
chisq.test(xtab1)

### Yes, the classical chi-sq test of independence
### is the score test in the corresponding loglinear model
anova(fit0, fit1, test = "Rao")

### Also the Wald test can be considered
(be1 <- coef(fit1))
V1 <- vcov(fit1)
(interIndex <- grep(":fa.educ", names(be1)))
W <- as.numeric(be1[interIndex] %*% solve(V1[interIndex, interIndex], be1[interIndex]))
pW <- pchisq(W, df = length(interIndex), lower.tail = FALSE)
Wald  <- data.frame(W = W, df = length(interIndex), Pvalue = pW)
print(Wald)


### Coef interpretation
exp(coef(fit1)[-1])
round(exp(coef(fit1)[-1]), 2)    ### Meaning of each coefficient?

### Check levels of the two factors
with(nels, table(ses,     useNA = "ifany"))
with(nels, table(fa.educ, useNA = "ifany"))
contr.treatment(3)

### ODDS on better ses (compared to ses = 1)
### given father's education
### ----------------------------------------
#
### fa.educ = Elementary
be1[paste("ses", 2:4, sep = "")]
(oddsElem <- exp(be1[paste("ses", 2:4, sep = "")]))
#
### fa.educ = High
be1[paste("ses", 2:4, sep = "")]
be1[paste("ses", 2:4, ":fa.educHigh", sep = "")]
(oddsHigh <- exp(be1[paste("ses", 2:4, sep = "")] + 
                   be1[paste("ses", 2:4, ":fa.educHigh", sep = "")]))
#
### fa.educ = College
be1[paste("ses", 2:4, sep = "")]
be1[paste("ses", 2:4, ":fa.educCollege", sep = "")]
(oddsColl <- exp(be1[paste("ses", 2:4, sep = "")] + 
                   be1[paste("ses", 2:4, ":fa.educCollege", sep = "")]))

### All in one table
ODDSbetterSES <- data.frame(Elementary = oddsElem, High = oddsHigh, 
                            College = oddsColl)
print(ODDSbetterSES)

### ODDS on better education (compared to fa.educ = Elementary)
### given family SES
### -----------------------------------------------------------------------
#
### ses = 1
(odds1 <- exp(be1[paste("fa.educ", c("High", "College"), sep = "")]))
#
### ses = 2
(odds2 <- exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses2:fa.educ", c("High", "College"), sep = "")]))
#
### ses = 3
(odds3 <- exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses3:fa.educ", c("High", "College"), sep = "")]))
#
### ses = 4
(odds4 <- exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
                be1[paste("ses4:fa.educ", c("High", "College"), sep = "")]))

### All in one table
ODDShigherEduc <- data.frame(ses1 = odds1, ses2 = odds2, ses3 = odds3, ses4 = odds4)
print(ODDShigherEduc)


### ODDS RATIOS (ratios of odds on higher ses compared to ses = 1
###   when comparing higher levels of education with Elementary one)
###   
### = ODDS RATIOS (ratios of odds on higher level of education 
###   compared to Elementary one)
###   when comparing higher ses with ses = 1)
###   
### -----------------------------------------------------------------------------
exp(be1[grep(":fa.educ", names(be1))])


### ODDS RATIOS (ratios of odds on higher ses compared to ses = 1
###   when comparing College education with High education)
###
### = ODDS RATIOS (ratios of odds on College education compared to High education 
###   when comparing higher ses with ses = 1
### ------------------------------------------------------------------------------
exp(be1[grep(":fa.educCollege", names(be1))] - be1[grep(":fa.educHigh", names(be1))])


### Are fit0 and fit1 the only reasonable models in this situation?
###
### How about the model below?
### Does it have reasonable interpretation?
### ----------------------------------------------------------------------
qq1 <- transform(qq1, nses = as.numeric(ses))
print(qq1)
summary(qq1)


### Linear trend?
fit1n <- glm(N ~ (nses + fa.educ)^2, family = poisson, data = qq1)
summary(fit1n)
be1n <- coef(fit1n)

### Odds on better ses (by 1)
### ----------------------------------------
#
### fa.educ = Elementary
exp(be1n["nses"])
#
(oddsnElem <- exp(be1n["nses"] * 1:3))  ## trend
exp(be1[paste("ses", 2:4, sep = "")])   ## saturated
#
### fa.educ = High
exp(be1n["nses"] + be1n["nses:fa.educHigh"])
#
(oddsnHigh <- exp((be1n["nses"] + be1n["nses:fa.educHigh"]) * 1:3))  ## trend
exp(be1[paste("ses", 2:4, sep = "")] +                           ## saturated
      be1[paste("ses", 2:4, ":fa.educHigh", sep = "")])                
#
### fa.educ = College
exp(be1n["nses"] + be1n["nses:fa.educCollege"])
#
(oddsnColl <- exp((be1n["nses"] + be1n["nses:fa.educCollege"]) * 1:3)) ## trend
exp(be1[paste("ses", 2:4, sep = "")] +                             ## saturated
      be1[paste("ses", 2:4, ":fa.educCollege", sep = "")])


### All in one table
ODDSnbetterSES <- data.frame(Elementary = oddsnElem, High = oddsnHigh, 
                             College = oddsnColl)
#
### Compare
print(ODDSnbetterSES)          ## SES ordinal (numeric)
print(ODDSbetterSES)           ## SES nominal


### Is (saturated) fit1 significantly better than fit1n?
### -------------------------------------------------------
anova(fit1n, fit1, test = "LRT")


### Odds on better education (compared to fa.educ = Elementary) 
### -----------------------------------------------------------------------
#
### ses = 1
(oddsn1 <- exp(be1n[paste("fa.educ", c("High", "College"), sep = "")] + 
                 be1n[paste("nses:fa.educ", c("High", "College"), sep = "")]))
#
exp(be1[paste("fa.educ", c("High", "College"), sep = "")])   ## Compare (saturated)
#
### ses = 2
(oddsn2 <- exp(be1n[paste("fa.educ", c("High", "College"), sep = "")] + 
                 2 * be1n[paste("nses:fa.educ", c("High", "College"), sep = "")]))
#
exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
      be1[paste("ses2:fa.educ", c("High", "College"), sep = "")])
#
### ses = 3
(oddsn3 <- exp(be1n[paste("fa.educ", c("High", "College"), sep = "")] + 
                 3 * be1n[paste("nses:fa.educ", c("High", "College"), sep = "")]))
#
exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
      be1[paste("ses3:fa.educ", c("High", "College"), sep = "")])
#
### ses = 4
(oddsn4 <- exp(be1n[paste("fa.educ", c("High", "College"), sep = "")] + 
                 4 * be1n[paste("nses:fa.educ", c("High", "College"), sep = "")]))
#
exp(be1[paste("fa.educ", c("High", "College"), sep = "")] + 
      be1[paste("ses4:fa.educ", c("High", "College"), sep = "")])


### All in one table
ODDSnhigherEduc <- data.frame(ses1 = oddsn1, ses2 = oddsn2, ses3 = oddsn3, 
                              ses4 = oddsn4)
#
### Compare
print(ODDSnhigherEduc)          ## SES ordinal (numeric)
print(ODDShigherEduc)           ## SES nominal


##### (Useful?) function to calculate
##### a goodness-of-fit test
##### - perhaps useful for multi-dimensional tables
##### ==============================================
gof <- function(m){
  DD <- deviance(m)
  df <- m$df.residual
  pval <- pchisq(DD, df, lower.tail = FALSE)
  nparm <- length(coef(m))
  
  LowCount <- sum(fitted(m) <= 5)
  
  cat("Goodness-of-fit test, model with ", nparm, " parameters\n", sep = "")
  cat("Deviance = ", DD, ", df = ", df, "\n", sep = "")
  cat("P-value: ", ifelse(pval < 0.001, "<0.001", format(round(pval, 3), nsmall = 3)), "\n\n", sep = "")
  
  if (LowCount){
    cat("Number of cells with low fitted counts: ", LowCount, "\n\n")
    print(summary(fitted(m)))
  }      
}


gof(fit1n)  ## Test of a linear trend for X again
#
gof(fit1)      ### Hmmmm...
### Do you have a better name for chi^2 distribution
### with 0 degrees of freedom?
### Correct p-value:
pchisq(0, df = 0, lower.tail = FALSE)

#
gof(fit0)    ## Test of independence again

