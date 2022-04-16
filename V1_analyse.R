###intro###

rm(list=ls())
#
library("colorspace")
library("xtable")
library(MASS)
library(questionr)
library(nnet)
library(broom.helpers)
library(GGally)

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
regm1 <- multinom(ses ~ fa.educ,family=poisson, data = nels)

summary(regm1)
odds.ratio(regm1)
ggcoef_multinom(
  regm1,
  exponentiate = TRUE
)
chisq.test(tab1)
#p-values very low. -> Dependence

mosaicplot(tab1, main = "Mosaic Plot",
           xlab = "Socioeconomic status",
           ylab = "Father Education",
           las = 1,
           border = "yellow",
           shade = TRUE)
#blue : over-representation
#red : under-representation


##interpretation

fit1 <- glm(N ~ ses + fa.educ + ses:fa.educ, family = poisson, data = qq1)  

summary(fit1)
#Do not interpret p-values.
sort(round(fit1$coefficients,2))

#interprete extreme values

#fa.educCollege under-representation
#ses4 under-representation
#fa.educCollege & ses4 in same time -> over-representaion in comparaison of alone 
#same for fa.educCollege & ses3


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

#explain

#fitp2.1
sort(round(fitp2.1$coefficients,2))

#under-representaion : regionWest ses2 
#uper-representation : fa.educHigh



#fitp2.2
sort(round(fitp2.3$coefficients,2))

#under-representaion : ses4:fa.educElementary ses1:fa.educCollege # educElementary and educCollege 
#uper-representation : ses2:regionSouth ses1:regionSouth ##ALL regionSouth

#fitp2.3
sort(round(fitp2.2$coefficients,2))

#under-representaion : 

#ses4:fa.educElementary:(regionWest + regionNortheast + regionMidwest)
#ses1:fa.educCollege:(regionWest+regionNortheast)

#uper-representation : 
#ses4:fa.educCollege:(regionSouth+regionMidwest+regionNortheast)
#ses2:fa.educHigh:regionMidwest


mosaicplot(tab2, main = "Mosaic Plot",
           xlab = "Socioeconomic status",
           ylab = "Father Education",
           las = 1,
           border = "yellow",
           shade = TRUE)



###part 3###
#socio-economic status of the family with the achieved level of education of the father.
#+region + fa.wrk
### Contingency table
(tab3 <- with(nels, table(ses, fa.educ,region,fa.wrk)))
(ptab3 <- round(prop.table(tab3, margin = 2) * 100, 1))

(qq3 <- as.data.frame(tab3, responseName = "N"))
fitp3.1 <- glm(N ~ ses + fa.educ + region+fa.wrk, family = poisson, data = qq3)  
fitp3.2 <- glm(N ~ (ses:fa.educ:region:fa.wrk), family = poisson, data = qq3)

####eplain result
#fitp3.1
sort(round(fitp3.1$coefficients,2))
#under-representation : fa.wrkNot_working & regionWest
#uper representation : fa.educCollege & fa.educHigh

#fitp3.2
sort(round(fitp3.2$coefficients,2))
#under-representation : 

#ses4:fa.educElementary:regionMidwest:fa.wrkNot_working
#ses4:fa.educElementary:regionSouth:fa.wrkNot_working

#uper-representation : 

#ses4:fa.educCollege:regionSouth:fa.wrkWorking
#ses4:fa.educCollege:regionMidwest:fa.wrkWorking
#ses2:fa.educHigh:regionMidwest:fa.wrkWorking



#uper representation : fa.educCollege & fa.educHigh

regm3 <- multinom(ses ~ fa.educ +region+fa.wrk ,family=poisson, data = nels)
summary(regm3)
odds.ratio(regm3)
ggcoef_multinom(
  regm3,
  exponentiate = TRUE
)

library(ggeffects)
plot(ggeffect(regm3, "region"))

mosaicplot(tab3, main = "Mosaic Plot",
           xlab = "Socioeconomic status",
           ylab = "Father Education",
           las = 1,
           border = "yellow",
           shade = TRUE)


###part 4###
#socio-economic MEDIAN status of the family with the achieved level of education of the father.
#+region + fa.wrk
with(nels,table(ses,sesmed))
#Below med. -> 1 & 2
#Over med. -> 3& 4

regm4 <- multinom(sesmed ~ fa.educ +region+fa.wrk ,family=poisson, data = nels)
summary(regm4)
odds.ratio(regm4)

###part 5###all
regm5 <- multinom(sesmed ~parents+foreign+fa.educ+mo.educ+region+fa.wrk+mo.wrk,family=poisson, data = nels)
summary(regm5)
step(regm5)
