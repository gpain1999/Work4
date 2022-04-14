### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work4")
#
library("colorspace")
library("xtable")
#
print(load("AdvRegr_4_nels.RData"))

##region

tableregion<-with(nels,table(ses,region))
(ses_table<-round(with(nels,table(ses))/dim(nels)[1] * 100,1))
(ptableregion <- round(prop.table(tableregion, margin = 2) * 100, 1))

#In the North West, 40% of people are in SES category 4. 
#While only 29% of people in the Midwest region are.

#We can run a chi² test but the repair seems rather non-independent in view of the 
#distance to the mean value and the amount of data. 

chisq.test(tableregion)

#The p-value is very low, so there is a dependency between the region of residence
#and the social and professional category.  

#graph : 

mosaicplot(tableregion, las = 3, shade = TRUE)

#Northeast over-represented : ses4 | under-represented : ses1 (and a little bit ses2)
#Midwest over-represented : ses2 | under-represented : (ses4 a little bit)
#South over-represented : ses1 | under-represented : (ses4 a little bit)
#West over-represented : (ses3 a little bit)| under-represented : (ses2 a little bit)


#The Northeast seems to be a rather rich region
#The South seems to be a rather poor region
#The West seems to be a good representation of the medium-effet.

#automatic calcule p_values

vari<-colnames(nels)[3:9]
res<-rep(NA,length(vari))

for (i in 1:length(vari)){
  table<-table(nels$ses,nels[,i+2])
  res[i]<-chisq.test(table)$p.value
}

data.frame(vari,round(res,4))

#All variables are related to the socio-professional category.
