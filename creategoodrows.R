# clear all memory
rm(list=ls(all=TRUE))

# load title data
load('extract2.RData')

# restrict to three journals but leave year unrestricted
# JID 24 = COGNITIVE SCIENCE
# JID 41 = COGNITION
# JID 52 = AM J. PSYCH
goodrows = which(title$JID==24 | title$JID==41 | title$JID==52)
myord = order(title$ToC[goodrows])
mytoc = title$ToC[goodrows][myord]

save(file="goodrows.RData",goodrows,myord,mytoc)

