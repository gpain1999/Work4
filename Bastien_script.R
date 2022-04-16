###intro###

library("colorspace")
library("xtable")
library(MASS)
library(questionr)
library(nnet)
library(broom.helpers)

#
rm(list=ls())
print(load("AdvRegr_4_nels.RData"))


###FIRST PART : ses vs fa.educ

(ses_fa <- with(nels, table(ses, fa.educ)))
plot(ses ~ fa.educ, data = nels, col = rainbow_hcl(4), 
     main = "SES by father's education")
chisq.test(ses_fa)

data_1 = as.data.frame(ses_fa, responseName = "N")
summary(data_1)

fit_ses_fa1 = glm(N ~ (ses+fa.educ)^2, data = data_1, family = poisson)
summary(fit_ses_fa1)

fit_ses_fa2 <- glm(N~ses+fa.educ, family = poisson, data=data_1)
summary(fit_ses_fa2)

anova(fit_ses_fa1,fit_ses_fa2)

drop1(fit_ses_fa1,test="Chisq")
drop1(fit_ses_fa2,test="Chisq")

# Let's investigate the effect of fa.educ as well as the interaction between ses3 and fa.educ

(coefs1 = coef(fit_ses_fa1))

## fa.educ 

### Elementary 
(oddsElem <- exp(coefs1[paste("fa.educ", 2:4, sep = "")]))

### fa.educ = High
(oddsHigh <- exp(coefs1[paste("ses", 2:4, sep = "")] + 
                   coefs1[paste("ses", 2:4, ":fa.educHigh", sep = "")]))

### College
(oddsColl <- exp(coefs1[paste("ses", 2:4, sep = "")] + 
                   coefs1[paste("ses", 2:4, ":fa.educCollege", sep = "")]))

## ses3 on fa.educ