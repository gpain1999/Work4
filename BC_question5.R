### (v) Analysis of a multi-way table
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Table
tab5 <- with(nels, table(sesmed, parents, foreign, fa.educ, mo.educ, region, 
                          fa.wrk, mo.wrk))
dim(tab5)


### Marginal counts
### Some exploration (do we have at least marginally reasonable counts?)
margin.table(tab5, margin = 1)
margin.table(tab5, margin = 2)
margin.table(tab5, margin = 3)
margin.table(tab5, margin = 4)
margin.table(tab5, margin = 5)
margin.table(tab5, margin = 6)
margin.table(tab5, margin = 7)
margin.table(tab5, margin = 8)


### Some pairwise dependencies
with(nels, table(fa.wrk, mo.wrk))
prop.table(with(nels, table(fa.wrk, mo.wrk)), margin = 2)
chisq.test(with(nels, table(fa.wrk, mo.wrk)))
#
with(nels, table(fa.educ, mo.educ))
prop.table(with(nels, table(fa.educ, mo.educ)), margin = 2)
chisq.test(with(nels, table(fa.educ, mo.educ)))
#
with(nels, table(mo.wrk, mo.educ))
prop.table(with(nels, table(mo.wrk, mo.educ)), margin = 2)
chisq.test(with(nels, table(mo.wrk, mo.educ)))


### Three-way interaction model
qq5 <- as.data.frame(xtab5, responseName = "N")
m1 <- glm(N ~ (sesmed + parents + foreign + fa.educ + mo.educ + region + 
                 fa.wrk + mo.wrk)^3, family = poisson, data = qq5)
summary(m1)
gof(m1)
?gof

### Two-way interaction model
mTwoWay <- glm(N ~ (sesmed + parents + foreign + fa.educ + mo.educ + region + 
                      fa.wrk + mo.wrk)^2, family = poisson, data = qq5)
#
gof(mTwoWay)
anova(mTwoWay, m1, test = "LR")        ### Some three-way interactions needed...

### Usual model building while starting from the three-way interaction model
D1 <- drop1(m1, test = "LR")            ## takes some time, be patient...
print(D1)
## -> not really handy to look at this just by eyes (manually)

### Candidates to remove in the first step
class(D1)
attr(D1, "row.names")
D1[["Pr(>Chi)"]]

(Drop1 <- attr(D1, "row.names")[-1][D1[["Pr(>Chi)"]][-1] > 0.3])

paste(Drop1, collapse = "-")
paste(". ~ . - ", paste(Drop1, collapse = "-"))
m2 <- update(m1, paste(". ~ . - ", paste(Drop1, collapse = "-")))
summary(m2)
anova(m2, m1, test = "LRT")

(coef <- round(coef(m2),2))

### region = East
coef[paste("region", c("Midwest","South","West"), sep = "")]
(oddsEast <- round(exp(coef[paste("region",  c("Midwest","South","West"), sep = "")]),2))
#
### region = Midwest
coef[paste("mo.educCollege", ":region", c("Midwest","South","West"), sep = "")]
coef[paste("region", c("Midwest","South","West"), sep = "")]
(oddsHigh <- round(exp(coef[paste("ses", 2:4, sep = "")] +coef[paste("region", c("Midwest","South","West"), ":fa.wrkNot_working", sep = "")]),2))
oddsmMid
#
### fa.educ = College
coef[paste("ses", 2:4, sep = "")]
coef[paste("ses", 2:4, ":fa.educCollege", sep = "")]
(oddsColl <- round(exp(coef[paste("ses", 2:4, sep = "")] + 
                         coef[paste("ses", 2:4, ":fa.educCollege", sep = "")]),2))

### All in one table
ODDSbetterSES <- data.frame(Elementary = oddsElem, High = oddsHigh, 
                            College = oddsColl)
print(ODDSbetterSES)

print(xtable(ODDSbetterSES, digits = c(0, 1, 1, 1)), floating = FALSE)
