setwd("C:/Unizeug/Übungen/CU_AdvRegressionModels/PS4")

library("colorspace")
library("xtable")

rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

### Basic exploration
### ++++++++++++++++++++++++++++++++++++++++++++
with(nels, table(region)) # seems alright

### (ii) ses vs. fa.educ vs. region
### ++++++++++++++++++++++++++++++++++++++++++++

(xtab2 <- with(nels, table(ses, fa.educ, region)))
round(100*prop.table(xtab2, margin = 3), 1)
qq2 <- as.data.frame(xtab2, responseName = "N")

### Saturated model
fit2 <- glm(N ~ (ses + fa.educ + region)^3, family = poisson, data = qq2)
summary(fit2)

### Deviance test on the three-way interaction
drop1(fit2, test = "LRT")
    # Can't drop the three-way-interaction term as it is significant
    # continue to look into Odds Ratios

### Some odds ratios

    ### Odds ratios (odds on higher ses compared to ses = 1) when comparing higher 
    ### educations with elementary ones
    ### = odds ratios (odds on higher education compared to elementary one) when
    ###   comparing higher ses with ses = 1
    ### --> conditional ones (given region)

## Region = Northeast (reference)
## ------------------------------
(lorNE <- c(be2[grep("^ses[0-9]:fa.educHigh$",    names(be2))],
            be2[grep("^ses[0-9]:fa.educCollege$", names(be2))]))
(orNE <- exp(lorNE))
(orNE <- matrix(round(orNE, 2), nrow = 3))
rownames(orNE) <- paste("ses", 2:4, sep = "")
colnames(orNE) <- paste("educ", c("High", "College"))

print(orNE) 

## Region = Midwest
## ------------------------------
    # conditional ORs of higher ses compared to ses = 1 when comparing
    # higher education with elementary ones
    # = conditional ORs on higher educ. compared to elementary
    # comparing higher ses with ses = 1
  # CONDITIONING on MIDWEST
(lorMW <- be2[grep("^ses[0-9]:.+:regionMidwest$", names(be2))])
(orMW <- exp(lorMW + lorNE))
(orMW <- matrix(round(orMW, 2), nrow = 3))
rownames(orMW) <- paste("ses", 2:4, sep = "")
colnames(orMW) <- paste("educ", c("High", "College"))
print(orMW) # TO BE REPORTED

  #p-values
(pvChangeMW <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest",     
                        names(pv2))])

## Region = South
## ------------------------------
# CONDITIONING on SOUTH
lorS <- be2[grep("^ses[0-9]:.+:regionSouth$", names(be2))]
(orS <- exp(lorS + lorNE))
(orS <- matrix(round(orS, 2), nrow = 3))
rownames(orS) <- paste("ses", 2:4, sep = "")
colnames(orS) <- paste("educ", c("High", "College"))
print(orS)

  #p-values
(pvChangeS <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionSouth$",     
                        names(pv2))])

## Region = West
## ------------------------------
# CONDITIONING on WEST
(lorW <- be2[grep("^ses[0-9]:.+:regionWest$", names(be2))])
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