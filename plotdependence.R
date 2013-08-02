# clear all memory
# rm(list=ls(all=TRUE))

# load flexmix
library('flexmix')

nc = length(parameters(mf))

# array of plots
par(mfcol=c(2,nc))

# make new matrix where non-jsd columns contain the mean
meandataIS = mydataIS
meandataOOS = mydataOOS
numcols = length(colnames(meandataIS))
jsdcols = grep(colnames(meandataIS),pattern="jsd")
for (k in setdiff(c(2:numcols),jsdcols))
{
    meandataIS[,k] = mean(mydataIS[,k])
    meandataOOS[,k] = mean(mydataOOS[,k])
}

# make new posteriors and predictions
meanpostIS = posterior(mf, newdata=meandataIS)
meanclustIS = max.col(meanpostIS)
meanpredIS = predict(mf, newdata=meandataIS)

meanpostOOS = posterior(mf, newdata=meandataOOS)
meanclustOOS = max.col(meanpostOOS)
meanpredOOS = predict(mf, newdata=meandataOOS)

for (j in c(1:nc))
{
    isj = which(meanclustIS==j)
    plot(mydataIS$averagejsd[isj], mydataIS$ToC[isj],col="black")

    fvjold = meanpredIS[[j]][isj]
    jsdord = order(mydataIS$averagejsd[isj])
    lines(mydataIS$averagejsd[isj][jsdord], fvjold[jsdord],col="blue")

    oosj = which(meanclustOOS==j)
    plot(mydataOOS$averagejsd[oosj], mydataOOS$ToC[oosj],col="black")

    fvjold = meanpredOOS[[j]][isj]
    jsdord = order(mydataOOS$averagejsd[isj])
    lines(mydataOOS$averagejsd[isj][jsdord], fvjold[jsdord],col="blue")
}


