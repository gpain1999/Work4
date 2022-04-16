##question 2

setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
print(load("AdvRegr_4_nels.RData"))


### ses vs. fa.educ vs. region

#repartition %
round(prop.table(with(nels, table(region, useNA = "ifany")))*100,2)
#South is bigger

### Contingency table
(tab2 <- with(nels, table(ses, fa.educ,region)))
(ptab2 <- round(prop.table(tab2, margin = 2) * 100, 1))

#dataset

qq2 <- as.data.frame(tab2, responseName = "N")

### Saturated model
fit2 <- glm(N ~ (ses + fa.educ + region)^3, family = poisson, data = qq2)
summary(fit2)
