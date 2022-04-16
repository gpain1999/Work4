### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
rm(list=ls())
print(load("AdvRegr_4_nels.RData"))

### Part 1 : ses vs. fa.educ
### ++++++++++++++++++++++++++++++++++++++++++++

(tab1 <- with(nels, table(ses, fa.educ)))
(ptab1 <- round(prop.table(xtab1, margin = 2) * 100, 2))


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

##coeff##
sort(round(fit1$coefficients,2))

##ses4 :
#We recall that "ses4" is strongly under-represented when 
#crossed with fa.educ High and Elementary but very over-represented with College.

#We can see the same with coefficians.
#ses4 alone : -3.57 
#this value rises when cross-referenced with fa.educHigh (+3.15) and more with fa.educCollege.

#same for fa.educ : College 
#uder-représentation for ses1,2,3 and over representation for ses4. 
#This is shown in the coefficients. 
