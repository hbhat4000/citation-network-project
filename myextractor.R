mytmextractor <- function(vs)
{
    # load text mining library if it has not yet been loaded
    require('tm')

    # create corpus
    ts <- Corpus(VectorSource(vs))

    # convert to lowercase
    ts <- tm_map(ts, tolower)

    # remove stopwords
    ts <- tm_map(ts, removeWords, stopwords("english"))

    # eliminate extra whitespace
    ts <- tm_map(ts, stripWhitespace)

    # eliminate punctuation
    removepunct <- function(x) { return(gsub("[[:punct:]]","",x)) }
    ts <- tm_map(ts, removepunct)

    # eliminate numbers
    removenum <- function(x) { return(gsub("[0-9]","",x)) }
    ts <- tm_map(ts, removenum)

    # make term-document matrix
    tsm <- DocumentTermMatrix(ts)

    # convert to sparse matrix
    require('Matrix')
    tmat = sparseMatrix(tsm$i,tsm$j,x=tsm$v)

    # split off the training set, and order by ascending ToC
    load('goodrows.RData')
    tmattrain = tmat[goodrows,]
    tmattrain = tmattrain[myord,]

    tmatlist = list(NULL)
    tmatlist[[1]] = tmattrain
    tmatlist[[2]] = tmat
    return(tmatlist)
}

mycossim <- function(xmat, ymat)
{
    xmatnorms = sqrt(rowSums(xmat^2))
    xmatzero = which(xmatnorms==0)
    xmatnormalizer = diag(1/xmatnorms)
    xmatnormalizer[xmatzero,xmatzero] = 1

    ymatnorms = sqrt(rowSums(ymat^2))
    ymatzero = which(ymatnorms==0)
    ymatnormalizer = diag(1/ymatnorms)
    ymatnormalizer[ymatzero,ymatzero] = 1

    cossimmat = xmatnormalizer %*% xmat %*% t(ymat) %*% ymatnormalizer
    return(cossimmat)
}

runthrough1 <- function(mat)
{
    load('extract2.RData')
    load('goodrows.RData')
    nt = length(goodrows) - 1
    outvec = rep(0,nt)
    for (j in c(1:nt))
    {
        test = colMeans(mat[j:length(goodrows),])
        outvec[j] = cor(test[goodrows],title$ToC[goodrows])
    }
    return(outvec)
}

# title, 428:440
# abstract, 429:440
# Keyword, 429:440



