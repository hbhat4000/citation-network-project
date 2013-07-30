# clear all memory
rm(list=ls(all=TRUE))

# for sparse matrix support
library('Matrix')

# load data into memory
load("extract2.RData")

# load network results
load("afterbigrun2.RData")

# number of papers each author has written
numauth = dim(author)[1]
numpapers = rep(0,numauth)
for (j in c(1:numauth))
    numpapers[j] = sum(comb$AID==j)

# journal distribution for all authors
journdist = matrix(0,nrow=dim(journal)[1],ncol=numauth)
for (j in c(1:numauth))
{
    # figure out all papers written by this author
    titlelist = comb$TID[which(comb$AID==j)]

    # journals associated with this title
    journallist = title$JID[titlelist]

    # count how many times each journal appears
    journalfreq = as.data.frame(table(journallist))

    # now assign probabilities
    journdist[journalfreq$journallist,j] = journalfreq$Freq/length(journallist)
}
journdist = t(journdist)

myentropy <- function(x) {y <- x[x > 0]; -sum(y * log2(y))}; 

jsd <- function(p,q) {myentropy(q %*% p) - q %*% apply(p, 1, myentropy)}

# NEW COLUMNS with one row per row of the title table
numtitle = dim(title)[1]
numauth = rep(0,numtitle)
averagepr = rep(0,numtitle)
averagenp = rep(0,numtitle)
averagejsd = rep(0,numtitle)
averageent = rep(0,numtitle)
for (j in c(1:numtitle))
{
    # figure out all the authors attached to this title
    authlist = comb$AID[which(comb$TID==j)]

    # number of authors for this paper
    numauth[j] = length(authlist)
    
    # average pagerank of those authors
    averagepr[j] = exp(mean(log(author$pagerank[authlist])))

    # average number of papers written by those authors
    averagenp[j] = mean(numpapers[authlist])

    # jensen-shannon divergence of authors' journal distributions
    if (length(authlist) >= 2)
        averagejsd[j] = jsd( journdist[authlist,], rep(1,length(authlist))/length(authlist) )

    # average of authors' journal distribution entropies
    if (length(authlist) >= 2)
        averageent[j] = mean(apply(journdist[authlist,],1,myentropy))
    else
        averageent[j] = myentropy(journdist[authlist,])
}
# normalize average pagerank it is between 0 and 1
averagepr = averagepr/max(averagepr)

# put stuff into a data frame
mydata = data.frame(cbind(title$ToC,title$TocR,title$year,numauth,averagepr,averagenp,averagejsd,averageent))
colnames(mydata) = c("ToC","TocR","year","numauth","averagepr","averagenp","averagejsd","averageent")

# save to file
save(mydata, file="mydataframe.RData")

