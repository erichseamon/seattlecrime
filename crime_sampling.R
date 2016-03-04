#------------------------------------------------------------------------#
# TITLE:        crime_sampling.R
#
# COURSE:       crime sampling  code to select a subset for autocorrelation analysis
#               Seattle child cortosol project.  Gartstein and Seamon
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         July 23, 2015
#
# STAGE:        crime sampling
#
#
#--Setting the working directory an d clearing the workspace-----------#


#-----Rcode starts-----



setwd("/nethome/erichs/seattle/")
x <- read.csv("/nethome/erichs/seattle/seattlecrime2008_rankedcrime.csv")

mysample <- x[sample(1:nrow(x), 10000, replace=FALSE),]
write.table(mysample, file = "seattle2008_sample.csv")

