# clear all memory
rm(list=ls(all=TRUE))

coefstrip <- function(x) { return(gsub("^.*?[.]","",x)) } 

# load flexmix
library('flexmix')

# load data into memory
load("mydataframe.RData")

# add transformation of one column
mydata$jsd2 = mydata$averagejsd^2

# convert year to age
mydata$year = 2013 - mydata$year

# restrict to three journals but leave year unrestricted
load('goodrows.RData')
mydataIS = mydata[goodrows,]

# fit mixture model
nc = 3
mf = stepFlexmix(ToC ~ ., k=3, data=mydataIS, model=FLXglm(family="poisson"), nrep=100)

# show summary of best model
print(summary(refit(mf)))


