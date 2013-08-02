# clear all memory
rm(list=ls(all=TRUE))

# preliminary db stuff
library('RMySQL')
drv = dbDriver("MySQL")

# load tables into memory
tablist = dbListTables(con)
myread <- function(tn) { return(dbReadTable(con,tn)) }
alltabs = lapply(tablist, myread)

# save into R file for later processing
filelist = c("tablist","alltabs")
save(list=filelist,file="negin.RData")

# disconnect resources
dbDisconnect(con)
dbUnloadDriver(drv)

