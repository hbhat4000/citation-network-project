# clear all memory
rm(list=ls(all=TRUE))

# load data into memory
load("extract.RData")

# fix AID numbering
numauth = dim(author)[1]
for (j in c(1:numauth))
{
    thisaid = author$AID[j]
    combrows = which(comb$AID==thisaid)
    comb$AID[combrows] = j
    author$AID[j] = j
}

# fix JID numbering
numjour = dim(journal)[1]
for (j in c(1:numjour))
{
    thisjid = journal$JID[j]
    titlerows = which(title$JID==thisjid)
    title$JID[titlerows] = j
    journal$JID[j] = j
}

# fix TID numbering
numtitles = dim(title)[1]
for (j in c(1:numtitles))
{
    thistid = title$TID[j]
    combrows = which(comb$TID==thistid)
    comb$TID[combrows] = j
    title$TID[j] = j
}

# one year is bad
title$year[1122] = 2005

# save data back
save(author,comb,journal,title,file="extract2.RData")

