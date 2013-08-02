# load flexmix
library('flexmix')

# fit mixture model
# nc = dim(parameters(mf))[2]
# nc = 3
nc = length(parameters(mf))

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


