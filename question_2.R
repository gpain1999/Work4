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


### Coefficients
(ce2 <- coef(fit2))
pv2 <- summary(fit2)$coefficients[, "Pr(>|z|)"]
(pv2 <- round(pv2, 3))

#ODD ratio for region.

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

### Midwest 
### ++++++++++++++++++++++++++++++++
(orChangeMW <- exp(ce2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest", 
                            names(ce2))]))

## Region = Midwest

### Midwest 
### ++++++++++++++++++++++++++++++++
(orChangeMW <- exp(ce2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest", 
                            names(ce2))]))

## Region = Midwest

### Midwest 
### ++++++++++++++++++++++++++++++++
(orChangeMW <- exp(ce2[grep("^ses[0-9]:fa.educ(College|High):regionMidwest", 
                            names(ce2))]))

## Region = South

### South 
### ++++++++++++++++++++++++++++++++
(orChangeS <- exp(ce2[grep("^ses[0-9]:fa.educ(College|High):regionSouth", 
                            names(ce2))]))

## Region = West

### West 
### ++++++++++++++++++++++++++++++++
(orChangeW <- exp(ce2[grep("^ses[0-9]:fa.educ(College|High):regionWest", 
                           names(ce2))]))
## Region = Northeast
### Northeast 
### is the reference.

#The West region has coefficients that appear to be much larger
#than the other regions for crosses with ses4 and educCollege