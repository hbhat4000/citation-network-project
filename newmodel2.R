# clear all memory
rm(list=ls(all=TRUE))

coefstrip <- function(x) { return(gsub("^.*?[.]","",x)) } 

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
mf1 = stepFlexmix(ToC ~ 1, k=3, data=mydataIS, model=Model1, nrep=10)
Model2 = FLXMRglmfix(family="poisson",fixed=~TocR + year + averagepr + averagenp + pages + titlescore,nested=list(k=c(1,1,1),formula=c(~0 + averagejsd+I(averagejsd^2),~1 + averagejsd+I(averagejsd^2),~1)))
mf = flexmix(ToC ~ 0, k=3, cluster=clusters(mf1), data=mydataIS, model=Model2)

accfig = 0
if (accfig == 1)
{

# figure out accuracy
lmyd = dim(mydataIS)[1]
fv = vector(mode='numeric',length=lmyd)
fvc = matrix(-1,nrow=lmyd,ncol=nc)
fmf = fitted(mf)
cmf = clusters(mf)
for (j in c(1:lmyd))
{
    fv[j] = as.numeric(fmf[j, cmf[j]])
    fvc[j, cmf[j]] = fv[j]
}

fvcnn = list(NULL)
for (j in c(1:nc))
    fvcnn[[j]] = which(fvc[,j] != -1)

par(mfrow=c(2,nc))
for (j in c(1:nc))
    plot(fvc[fvcnn[[j]],j], mydataIS$ToC[fvcnn[[j]]])

# out-of-sample
mydataOOS = mydata[-goodrows,]
testPost = posterior(mf, newdata=mydataOOS)
testClust = max.col(testPost)
testPred = predict(mf, newdata=mydataOOS)
lmydOOS = dim(mydataOOS)[1]
fvOOS = vector(mode='numeric',length=lmydOOS)
fvcOOS = matrix(-1,nrow=lmydOOS,ncol=nc)
for (j in c(1:lmydOOS))
{
    fvOOS[j] = testPred[[testClust[j]]][j]
    fvcOOS[j,testClust[j]] = fvOOS[j]
}

fvcOOSnn = list(NULL)
for (j in c(1:nc))
    fvcOOSnn[[j]] = which(fvcOOS[,j] != -1)

for (j in c(1:nc))
    plot(fvcOOS[fvcOOSnn[[j]],j], mydataOOS$ToC[fvcOOSnn[[j]]])

print(cor(fv,mydataIS$ToC)^2)
print(cor(fvOOS,mydataOOS$ToC)^2)

}
