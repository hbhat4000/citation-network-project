source('dbload.R') # transfers data from SQL to R.
                   # for security, one line is missing

# data post-processing, must be run in sequence
source('postprocess.R')
source('fixnumbering.R')

# construct pageRank network model, again this must be run in sequence
source('makenetworks.R')
source('fixpagerank.R')

# construct a bunch of new covariates such as JSD (Jensen-Shannon Divergence)
source('model.R')

# this is where we actually build the Poisson mixture regression model
source('newmodel2.R') # must be run repeatedly until good in-sample result
                      # is achieved; reason is that model is fit using 
                      # expectation maximization w/ random initial conditions
                      # and each time it converges to a different local max
                      # of the log likelihood function...by running many times
                      # you can approach the true global max of the 
                      # log likelihood

source('detaccuracy.R') # this carries out in-sample and out-of-sample tests

source('plotdependence.R') # this plots the dependence on JSD with all other
                           # covariates frozen at mean values



