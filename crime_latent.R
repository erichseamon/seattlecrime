
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


x <- data.frame(read.csv("/nethome/erichs/class3a.csv"))

xx <- x[,-15]
xx[is.na(xx)] <- 0

class1 <- subset(x, class == 1)
class1 <- class1[,-15]
class1[is.na(class1)] <- 0
class2 <- subset(x, class ==2)
class2 <- class2[,-15]
class2[is.na(class2)] <- 0
class3 <- subset(x, class ==3)
class3 <- class3[,-15]
class3[is.na(class3)] <- 0

#class4 <- subset(x, class ==4)
#class4 <- class4[,-15]
#class4[is.na(class4)] <- 0



var <- c(1:14)
classmatrix1 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix2 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix3 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix4 <- matrix(NA, nrow = 14, ncol = 1)


for (i in var) {
  classmatrix1[i,] <- mean(class1[,i]) 
}

for (i in var) {
  classmatrix2[i,] <- mean(class2[,i]) 
}

for (i in var) {
  classmatrix3[i,] <- mean(class3[,i]) 
}

#for (i in var) {
#  classmatrix4[i,] <- mean(class4[,i]) 
#}

classmatrixall <- cbind(classmatrix1, classmatrix2, classmatrix3)
classt <- t(classmatrixall)


plot(classt[1,], xaxt = "n", ylim=c(0,8),
     main="latent class analysis plot",
     ylab="ylabel",
     xlab="xlabel") 
lines(classt[1,], col="blue")
points(classt[2,])
lines(classt[2,], col="green")
points(classt[3,])
lines(classt[3,], col="red")

     legend("bottomright", c("class1", "class2", "class3"), lty=c(1,1), lwd=c(2,5, 2,5, 2,5), col=c("blue", "green", "red"))
     
     



