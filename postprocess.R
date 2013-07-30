# clear all memory
rm(list=ls(all=TRUE))

# load data into memory
load("negin.RData")

# apply postprocessing
for (j in c(1,3,5,7))
{
    firstcolname = colnames(alltabs[[j]])[1]
    assign(x=tablist[j],value=data.frame(alltabs[[j]]))
}

# work on the title table
# define a function that does what we want to an individual string
myintproc <- function(x)
{
    y = regexpr(x,pattern="[[:digit:]]+")
    y2 = substring(x,y[1],y[1]+attr(y,"match.length")-1)
    return(strtoi(y2))
}

myyearproc <- function(x)
{
    y = regexpr(x,pattern="[[:digit:]]{4}")
    y2 = substring(x,y[1],y[1]+attr(y,"match.length")-1)
    return(strtoi(y2))
}

newtoc = unlist(lapply(X=title$ToC,FUN=myintproc))
title$ToC = newtoc

newtocr = unlist(lapply(X=title$TocR,FUN=myintproc))
title$TocR = newtocr

newyear = unlist(lapply(X=title$year,FUN=myyearproc))
title$year = newyear

# save extracted tables to file
save(file="extract.RData",list=tablist[c(1,3,5,7)])

