##question 3

setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
print(load("AdvRegr_4_nels.RData"))

#socio-economic status of the family with the achieved level of education of the father.
#+region + fa.wrk

### Contingency table
(tab3 <- with(nels, table(ses, fa.educ,region,fa.wrk)))
(ptab3 <- round(prop.table(tab3, margin = 2) * 100, 1))

(qq3 <- as.data.frame(tab3, responseName = "N"))
#Problem: cross-over with no observations.

#satured model
fitp3 <- glm(N ~ (ses+fa.educ+region+fa.wrk)^4, family = poisson, data = qq3)
fitp3.1 <- glm(N ~ (ses:fa.educ:region:fa.wrk), family = poisson, data = qq3)
anova(fitp3,fitp3.1,test = "LRT")
#modele fitp3.1 seems beter.

