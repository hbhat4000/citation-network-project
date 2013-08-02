# clear all memory
rm(list=ls(all=TRUE))

# load text mining library
library('tm')

# load title data
load('extract2.RData')

# create corpus
ts <- Corpus(VectorSource(title$name))

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
library('Matrix')
tsmmat = sparseMatrix(tsm$i,tsm$j,x=tsm$v)

# restrict to three journals but leave year unrestricted
# JID 24 = COGNITIVE SCIENCE
# JID 41 = COGNITION
# JID 52 = AM J. PSYCH
goodrows = which(title$JID==24 | title$JID==41 | title$JID==52)
myord = order(title$ToC[goodrows])
mytoc = title$ToC[myord]

# reorder the training portion of tsmmat
tsmmattrain = tsmmat[goodrows,]
tsmmattrain = tsmmattrain[myord,]


