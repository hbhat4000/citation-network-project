# clear all memory
rm(list=ls(all=TRUE))

# for sparse matrix support
library('Matrix')

# load data into memory
load("extract2.RData")

# make network where each vertex is an author
# two authors are connected if they have coauthored a paper

# also make network where each vertex is a paper
# two papers are connected if they share authors
numauth = dim(author)[1]
numtitle = dim(title)[1]

ilist = list(NULL)
jlist = list(NULL)
curind = 1
for (j in c(1:numtitle))
{
    # find all authors who wrote paper j
    authlist = comb$AID[which(comb$TID==j)]
    
    # connect 'em!
    maxk = length(authlist)
    if (maxk >= 2)
    {
      for (k in c(1:(maxk-1)))
      {
        for (l in c((k+1):maxk))
        {
            ilist[[curind]] = authlist[k]
            jlist[[curind]] = authlist[l]
            curind = curind + 1
            
            ilist[[curind]] = authlist[l]
            jlist[[curind]] = authlist[k]
            curind = curind + 1
        }
      }
    }
}
authnet = sparseMatrix(i=unlist(ilist),j=unlist(jlist),x=1)

ilist = list(NULL)
jlist = list(NULL)
curind = 1
for (j in c(1:numauth))
{
    # find all titles with author j
    titlelist = comb$TID[which(comb$AID==j)]
    
    # connect 'em!
    maxk = length(titlelist)
    if (maxk >= 2)
    {
      for (k in c(1:(maxk-1)))
      {
        for (l in c((k+1):maxk))
        {
            ilist[[curind]] = titlelist[k]
            jlist[[curind]] = titlelist[l]
            curind = curind + 1
            
            ilist[[curind]] = titlelist[l]
            jlist[[curind]] = titlelist[k]
            curind = curind + 1
        }
      }
    }
}
titlenet = sparseMatrix(i=unlist(ilist),j=unlist(jlist),x=1)

# ranking of authors in author network
# using PageRank for now
d = 0.85
normalizer = sparseMatrix(i=c(1:numauth),j=c(1:numauth),x=1/colSums(authnet))
transmat = authnet %*% normalizer
lhs = diag(numauth) - d*transmat
rhs = ((1-d)/numauth)*rep(1,numauth)
pr = solve(lhs, rhs)

# save file because the above takes way too long even with sparse matrices
save.image(file="afterbigrun.RData")



