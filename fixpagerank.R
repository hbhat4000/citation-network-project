# clear all memory
rm(list=ls(all=TRUE))

# for sparse matrix support
library('Matrix')

# load data into memory
load("extract2.RData")

# load preliminary network results
load("afterbigrun.RData")

# problem: some of the authors have no connections

# figure out which ones those are
tmp = colSums(authnet)
zeroauth = which(tmp==0)

# delete those authors from network
authnet = authnet[-c(zeroauth),-c(zeroauth)]
numauth = dim(authnet)[1]

# ranking of authors in author network
# using PageRank for now
d = 0.85
normalizer = sparseMatrix(i=c(1:numauth),j=c(1:numauth),x=1/colSums(authnet))
transmat = authnet %*% normalizer
lhs = diag(numauth) - d*transmat
rhs = ((1-d)/numauth)*rep(1,numauth)
pr = solve(lhs, rhs)

# put results back into author data frame
author$pagerank = rep(0,dim(author)[1])
author$pagerank[-zeroauth] = as.numeric(pr)


# now do the same for the title network
tmp = colSums(titlenet)
zerotitle = which(tmp==0)

# delete those authors from network
titlenet = titlenet[-c(zerotitle),-c(zerotitle)]
numtitle = dim(titlenet)[1]

# ranking of authors in author network
# using PageRank for now
d = 0.85
normalizer = sparseMatrix(i=c(1:numtitle),j=c(1:numtitle),x=1/colSums(titlenet))
transmat = titlenet %*% normalizer
lhs = diag(numtitle) - d*transmat
rhs = ((1-d)/numtitle)*rep(1,numtitle)
pr = solve(lhs, rhs)

# put results back into title data frame
title$pagerank = rep(0,dim(title)[1])
title$pagerank[-zerotitle] = as.numeric(pr)

# save results
save.image(file="afterbigrun2.RData")

