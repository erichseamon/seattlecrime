
#------------------------------------------------------------------------#
# TITLE:        crime_index_calc.R
#
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         August 2015
#
# STAGE:        crime_index_calc.R
#
# COMMENTS:     this script calculates a crime index for each sample from
# the provided dataset.  The input dataset in an output from an ArcGIS model that 
# organizes crime data 1000 feet from a set of participant observations.
#                
#
#--Setting the working directory an d clearing the workspace-----------

library(foreign)
library(plyr)

#--read in the file

x <- read.dbf("/nethome/erichs/go1_addresses_Seattle_out4.dbf")

#--convert to a data frame
x <- data.frame(x)

#--create the sum of distrank for all values within OBJECTID_1 that are the same.
#--this total goes into the crime index calculation

x2 <- ddply(x, .(IN_FID), summarise, X2=sum(distrank))

x_oneeach <- x[order(-x$IN_FID),]
#x_oneeach2 <- x[order(-x$IN_FID),]
x_oneeach_agg <- aggregate(x_oneeach,by=list(key=x_oneeach$IN_FID),FUN=function(IN_FID){IN_FID[1]})
xfreq <- data.frame(x_oneeach_agg$FREQUENCY)


xfam <- x[order(-x$Family_ID),]
x_fam_agg <- aggregate(xfam,by=list(key=xfam$Family_ID),FUN=function(Family_ID){Family_ID[1]})


xfinal <- cbind(x2$IN_FID,x2$X2,xfreq$x_oneeach_agg.FREQUENCY,x_fam_agg$Family_ID)
colnames(xfinal) <- c("OBJECTID_1", "NEAR_DIST", "FREQUENCY", "FAMILYID")
xfinal <- data.frame(xfinal)

xfinal2 <- data.frame(xfinal$NEAR_DIST/xfinal$FREQUENCY)
xfinalfinal <- cbind(xfinal2, xfinal)
colnames(xfinalfinal) <- c("CRIMEINDEX", "OBJECTID_1", "NEAR_DIST", "FREQUENCY", "FAMILYID")

write.table(xfinalfinal, file="/nethome/erichs/go1_addresses_Seattle_out4a.csv")




-----




x_freq <- data.frame(x$FREQUENCY)
x_freq_unique <- unique(x$OBJECTID_1)




obs <- c(1:122)
indexmatrix <- matrix(NA, nrow = 122, ncol = 1)

for (i in obs) {
  
  apply(x, i, sum)
  crimes <- sum(x$OBJECTID_1==i)
  for (j in crimes) {
  x[i,4] * x[i,36]
}
  indexmatrix[i,] <- (x[i,4] * x[i,36])/x[i,12]
}


