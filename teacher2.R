### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("/home/komarek/teach/mff_2020/nmst432_AdvRegr/Problem_4/")
#
library("colorspace")
library("xtable")
#
print(load("AdvRegr_4_nels.RData"))

##### *************************************************************************************** #####

### (ii) ses vs. fa.educ vs. region
### ++++++++++++++++++++++++++++++++++++++++++++

### Table and related data.frame
(xtab2 <- with(nels, table(ses, fa.educ, region)))
qq2 <- as.data.frame(xtab2, responseName = "N")

### Saturated model
fit2 <- glm(N ~ (ses + fa.educ + region)^3, family = poisson, data = qq2)
summary(fit2)

### Deviance test on the three-way interaction
drop1(fit2, test = "LRT")

### Coefficients
(be2 <- coef(fit2))
pv2 <- summary(fit2)$coefficients[, "Pr(>|z|)"]
(pv2 <- round(pv2, 3))


### Some odds ratios

### Odds ratios (odds on higher ses compared to ses = 1) when comparing higher 
### educations with elementary ones
### = odds ratios (odds on higher education compared to elementary one) when
###   comparing higher ses with ses = 1
### --> conditional ones (given region)
#
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
### Try by yourself and report in your document.
### Some three-way interactions must be involved in your calculations ;-)
#
## Region = South
## ------------------------------
### Try by yourself, no need to report in your document.
#
## Region = West
## ------------------------------
### Try by yourself, no need to report in your document.


### Three-way interactions: changes of above conditional odds ratios
### when comparing given region with Northeast (reference)
### ------------------------------------------------------------------
#
### Midwest - Northeast (reference)
### ++++++++++++++++++++++++++++++++
(orChangeMW <- exp(be2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest", 
                            names(be2))]))
(pvChangeMW <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest",     
                        names(pv2))])

### West - Northeast (reference)
### +++++++++++++++++++++++++++++++
(orChangeWE <- exp(be2[grep("^ses[0-9]:fa.educ(College|High):regionWest",    
                            names(be2))]))
(pvChangeWE <- pv2[grep("^ses[0-9]:fa.educ(College|High):regionWest",    
                        names(pv2))])

### South and Northeast (reference)
### ++++++++++++++++++++++++++++++++
### Try by yourself, just for yourself.


### Also Midwest-South, Midwest-West, South-West comparisons can be done...
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Would you know how? 
### Try by yourself, just for yourself.


##### *************************************************************************************** #####

### (iii) sesmed vs. fa.educ, fa.wrk and region by logistic regression
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Logit model (saturated)
fit3 <- glm(sesmed ~ (fa.educ + fa.wrk + region)^3, family = binomial, data = nels)
drop1(fit3, test = "LRT")

### Logit model, no three-way interaction
fit3alt <- glm(sesmed ~ (fa.educ + fa.wrk + region)^2, family = binomial, data = nels)
drop1(fit3alt, test = "LRT")

### Start with fit3alt and try to simplify it
### --> fit30alt


##### *************************************************************************************** #####

### (iv) sesmed vs. fa.educ, fa.wrk and region by loglinear model
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### data.frame for loglinear model
(xtab4 <- with(nels, table(sesmed, fa.educ, fa.wrk, region)))
qq4 <- as.data.frame(xtab4, responseName = "N")

### Specify and fit the loglinear model which is equivalent to the logistic model
### found in the previous step.


##### *************************************************************************************** #####

##### (Useful?) function to calculate
##### a goodness-of-fit test
##### - perhaps useful for multi-dimensional tables
##### ==============================================
gof <- function(m){
  DD <- deviance(m)
  df <- m$df.residual
  pval <- pchisq(DD, df, lower.tail = FALSE)
  nparm <- length(coef(m))
  
  LowCount <- sum(fitted(m) <= 5)
  
  cat("Goodness-of-fit test, model with ", nparm, " parameters\n", sep = "")
  cat("Deviance = ", DD, ", df = ", df, "\n", sep = "")
  cat("P-value: ", ifelse(pval < 0.001, "<0.001", format(round(pval, 3), nsmall = 3)), "\n\n", sep = "")
  
  if (LowCount){
    cat("Number of cells with low fitted counts: ", LowCount, "\n\n")
    print(summary(fitted(m)))
  }      
}


##### *************************************************************************************** #####

### (v) Analysis of a multi-way table
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Table
xtab5 <- with(nels, table(sesmed, parents, foreign, fa.educ, mo.educ, region, 
                          fa.wrk, mo.wrk))
dim(xtab5)
prod(dim(xtab5))       ## 1152


### Marginal counts
### Some exploration (do we have at least marginally reasonable counts?)
margin.table(xtab5, margin = 1)
margin.table(xtab5, margin = 2)
margin.table(xtab5, margin = 3)
margin.table(xtab5, margin = 4)
margin.table(xtab5, margin = 5)
margin.table(xtab5, margin = 6)
margin.table(xtab5, margin = 7)
margin.table(xtab5, margin = 8)


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
gof(m1)


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

(Drop1 <- attr(D1, "row.names")[-1][D1[["Pr(>Chi)"]][-1] > 0.5])

paste(Drop1, collapse = "-")
paste(". ~ . - ", paste(Drop1, collapse = "-"))
m2 <- update(m1, paste(". ~ . - ", paste(Drop1, collapse = "-")))
gof(m2)
anova(m2, m1, test = "LRT")

### If we really want to be sure that nothing "important" has been removed,
### standard back-check can be performed (rather automatically) by adding
### each of removed terms back to the model
(Return1  <- add1(m2, scope = m1, test = "LRT"))
min(Return1[, "Pr(>Chi)"], na.rm = TRUE)


### Alternative: specify those factors which should be kept in the model
(Keep1 <- attr(D1, "row.names")[-1][D1[["Pr(>Chi)"]][-1] < 0.2])
m3 <- update(mTwoWay, paste(". ~ . + ", paste(Keep1, collapse = "+")))
gof(m3)
anova(m3, m1, test = "LRT")