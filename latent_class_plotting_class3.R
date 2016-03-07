
#------------------------------------------------------------------------#
# TITLE:        latent_class_plotting.R
#
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         August 2015
#
# STAGE:        latent_class_plotting.R
#
# COMMENTS:     this script plots some latent class data
#                
#
#--Setting the working directory an d clearing the workspace-----------
rm(list=ls())

#--loads the file

library(TeachingDemos)

x <- data.frame(read.csv("/nethome/erichs/class3fin.csv"))

#--removes the class field for use later, and change NA to zero

xx <- x[,-15]
xx[is.na(xx)] <- 0


#--create separate vectors for each class

class1 <- subset(x, class == 1)
class1 <- class1[,-15]
class1[is.na(class1)] <- 0
class2 <- subset(x, class==2)
class2 <- class2[,-15]
class2[is.na(class2)] <- 0
class3 <- subset(x, class==3)
class3 <- class3[,-15]
class3[is.na(class3)] <- 0

class4 <- subset(x, class==4)
class4 <- class4[,-15]
class4[is.na(class4)] <- 0

class5 <- subset(x, class==5)
class5 <- class5[,-15]
class5[is.na(class5)] <- 0


#--loop variable and prepopulated matrices

var <- c(1:14)
classmatrix1 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix2 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix3 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix4 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix5 <- matrix(NA, nrow = 14, ncol = 1)
#classmatrix4 <- matrix(NA, nrow = 14, ncol = 1)


#--loops for each class - calc mean for each variable

for (i in var) {
  classmatrix1[i,] <- mean(class1[,i]) 
}

for (i in var) {
  classmatrix2[i,] <- mean(class2[,i]) 
}

for (i in var) {
  classmatrix3[i,] <- mean(class3[,i]) 
}

for (i in var) {
  classmatrix4[i,] <- mean(class4[,i]) 
}

for (i in var) {
  classmatrix5[i,] <- mean(class5[,i]) 
}

#--now merge the vectors back to one data frame

classmatrixall <- cbind(classmatrix1, classmatrix2, classmatrix3, classmatrix4, classmatrix5)

#--and transpose it

classt <- t(classmatrixall)

#--now plot it!>

par(mar=c(9,7.5,2,2))
par(mgp=c(7.7,1,0))
par(oma=c(0,0,0,0) )

labels <- c("act", "sl", "hp", "ps", "app", "vr", "dl", "fear", "sad", "fall", "do", "lp", "sooth", "cudd")
mar.default <- c(2,1,0)
plot(classt[1,], family = "Garamond", xaxt = "n", xlim=c(1,14), ylim=c(0,8),  
     main="Figure 2. Latent Profile Analysis (LPA) Plot: 3-class solution, with infant age as a covariate",
     xlab="IBQ-R Scales \n \nNOTE: act = activity, sl = smiling/laughter, hp = high intensity pleasure, ps = perceptual sensitivity, \napp = approach, vr = vocal reactivity, dl = distress to limitations, fear = fearfulness, \nsad = sadness, fall = falling reactivity, do = duration of orienting, \nlp = low intensity pleasure, sooth = soothability, cudd = cuddliness",
     ylab="")
title(ylab="Frequency Rating", line=2.2, cex.lab=1.0, family = "Garamond")
axis(1, at = 1:14, labels = FALSE)
text(x = seq(1, 14, by=1), -1.4, labels = labels, srt = 90, pos = 4, xpd = TRUE)


lines(classt[1,], col="blue")
points(classt[2,])
lines(classt[2,], col="green")
points(classt[3,])
lines(classt[3,], col="red")
points(classt[4,])
lines(classt[4,], col="orange")
points(classt[5,])
lines(classt[5,], col="magenta")

#--add a legend

legend("bottomright", c("class1", "class2", "class3"), lty=c(1,1), lwd=c(2,5, 2,5, 2,5, 2,5, 2,5), col=c("blue", "green", "red", "orange", "magenta"))


