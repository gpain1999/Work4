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