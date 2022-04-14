###intro###

#
library("colorspace")
library("xtable")
library(MASS)
library(questionr)
library(nnet)
library(broom.helpers)
#
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")

print(load("AdvRegr_4_nels.RData"))

###part 1###
#socio-economic status of the family with the achieved level of education of the father.

#repartition %
round(prop.table(with(nels, table(fa.educ, useNA = "ifany")))*100,2)

### Contingency table
(tab1 <- with(nels, table(ses, fa.educ)))
(ptab1 <- round(prop.table(tab1, margin = 2) * 100, 1))

#First observation : 
#Elementary -> more 1 & 2
#High -> More 2 & 3
#College -> More 4

### Data frame for loglinear modelling
(qq1 <- as.data.frame(tab1, responseName = "N"))
fit1 <- glm(N ~ ses + fa.educ + ses:fa.educ, family = poisson, data = qq1)  
summary(fit1)

fit1_bis = glm(N)


###part 2###
#socio-economic status of the family with the achieved level of education of the father.
#+region

#repartition %
round(prop.table(with(nels, table(region, useNA = "ifany")))*100,2)
#South is bigger

### Contingency table
(tab2 <- with(nels, table(ses, fa.educ,region)))
(ptab2 <- round(prop.table(tab2, margin = 2) * 100, 1))

### Data frame for loglinear modelling
(qq2 <- as.data.frame(tab2, responseName = "N"))
fitp2.1 <- glm(N ~ ses + fa.educ + region, family = poisson, data = qq2)  
fitp2.2 <- glm(N ~ (ses:fa.educ)+(ses:region)+(fa.educ:region), family = poisson, data = qq2)
fitp2.3 <- glm(N ~ (ses:fa.educ:region), family = poisson, data = qq2)

summary(fitp2.1)
summary(fitp2.2)
summary(fitp2.3)

###part 3###
#socio-economic status of the family with the achieved level of education of the father.
#+region + fa.wrk

regm3 <- multinom(ses ~ fa.educ +region+fa.wrk ,family=poisson, data = nels)
summary(regm3)
odds.ratio(regm3)

ggcoef_multinom(
  regm3,
  exponentiate = TRUE
)

library(ggeffects)
plot(ggeffect(regm3, "region"))

