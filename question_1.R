### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
#setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

### Part 1 : ses vs. fa.educ
### ++++++++++++++++++++++++++++++++++++++++++++

(tab1 <- with(nels, table(ses, fa.educ)))
(ptab1 <- round(prop.table(tab1, margin = 2) * 100, 2))


#First observation : 
#Elementary -> more 1 & 2
#High -> More 2 & 3
#College -> More 4

#graph

mosaicplot(tab1, main = "Mosaic Plot",
           xlab = "Socioeconomic status",
           ylab = "Father Education",
           las = 1,
           border = "gold",
           shade = TRUE)
#blue : over-representation
#red : under-representation

#presence of much over- or under-representation. 
#The variables therefore appear to be dependent. 

#independance ?

chisq.test(tab1)
#p-values <2.2e-16 -> Dependance variables.

#The variables are dependent. The combination of certain modalities results
#in a larger number of people.

########modele########

### Data frame for loglinear modelling
(qq1 <- as.data.frame(tab1, responseName = "N"))
##for each combinaison we have the number of people.

##modele whit interaction.
#We add the interaction because we have found a link between the two variables.
fit1 <- glm(N ~ ses + fa.educ + ses:fa.educ, family = poisson, data = qq1)  
fit1.1 <- glm(N ~ses:fa.educ, family = poisson, data = qq1)  

summary(fit1)
summary(fit1.1)
#compare
anova(fit1, fit1.1, test = "Rao")
#fit1 is better.

#Perfect model because more variables than individuals. 

#table 
with(nels, table(ses,     useNA = "ifany"))
with(nels, table(fa.educ,     useNA = "ifany"))

### Coef interpretation
ce1 <- coef(fit1)

sort(round(exp(coef(fit1)[-1]), 2))   
#####Coefficients of the variable fa.educ#######

### fa.educ = Elementary
ce1[paste("ses", 2:4, sep = "")]
(oddsElem <- exp(ce1[paste("ses", 2:4, sep = "")]))
#The values of Elementary are found in the classic "ses" variables. 

#
### fa.educ = High
ce1[paste("ses", 2:4, sep = "")]
ce1[paste("ses", 2:4, ":fa.educHigh", sep = "")]
(oddsHigh <- exp(ce1[paste("ses", 2:4, sep = "")] + 
                   ce1[paste("ses", 2:4, ":fa.educHigh", sep = "")]))
#
### fa.educ = College
ce1[paste("ses", 2:4, sep = "")]
ce1[paste("ses", 2:4, ":fa.educCollege", sep = "")]
(oddsColl <- exp(ce1[paste("ses", 2:4, sep = "")] + 
                   ce1[paste("ses", 2:4, ":fa.educCollege", sep = "")]))

#####Coefficients of the variable ses#######

### ses = 1
(odds1 <- exp(ce1[paste("fa.educ", c("High", "College"), sep = "")]))
#
### ses = 2
(odds2 <- exp(ce1[paste("fa.educ", c("High", "College"), sep = "")] + 
                ce1[paste("ses2:fa.educ", c("High", "College"), sep = "")]))
#
### ses = 3
(odds3 <- exp(ce1[paste("fa.educ", c("High", "College"), sep = "")] + 
                ce1[paste("ses3:fa.educ", c("High", "College"), sep = "")]))
#
### ses = 4
(odds4 <- exp(ce1[paste("fa.educ", c("High", "College"), sep = "")] + 
                ce1[paste("ses4:fa.educ", c("High", "College"), sep = "")]))

#No values for Ellementary as it is present in the constant. 
#These are coefficients for the transition from one statue to another.

### ODDS RATIOS of coeff


### -----------------------------------------------------------------------------
exp(ce1[grep(":fa.educ", names(ce1))])

### ------------------------------------------------------------------------------
exp(ce1[grep(":fa.educCollege", names(ce1))] - ce1[grep(":fa.educHigh", names(ce1))])


