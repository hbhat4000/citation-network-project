# clear all memory
rm(list=ls(all=TRUE))

# load flexmix
library('flexmix')

# load data into memory
load("mydataframe.RData")

# convert year to age
mydata$year = 2013 - mydata$year

# restrict to three journals but leave year unrestricted
load('goodrows.RData')
mydataIS = mydata[goodrows,]

# fit mixture model
nc = 3
Model1 = FLXMRglmfix(family="poisson",fixed=~TocR + year + averagepr + averagenp + pages + titlescore,nested=list(k=c(1,1,1),formula=c(~averagejsd+I(averagejsd^2),~averagejsd+I(averagejsd^2),~0)))
mf1 = stepFlexmix(ToC ~ 1, k=nc, data=mydataIS, model=Model1, nrep=10)
Model2 = FLXMRglmfix(family="poisson",fixed=~TocR + year + averagepr + averagenp + pages + titlescore,nested=list(k=c(1,1,1),formula=c(~0 + averagejsd+I(averagejsd^2),~1 + averagejsd+I(averagejsd^2),~1)))
mf = flexmix(ToC ~ 0, k=nc, cluster=clusters(mf1), data=mydataIS, model=Model2)

