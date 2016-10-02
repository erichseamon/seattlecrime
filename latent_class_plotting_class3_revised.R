
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

x1 <- data.frame(read.csv("/nethome/erichs/IBQ-R_yng_orig_Erich.csv"))

#--removes th id field, occurant for some csv files masha provides lol

x1 <- x1[,-1]
colnames(x1)[14] <- "class"

#--removes the class field for use later, and change NA to zero

xx <- x1[,-14]
xx[is.na(xx)] <- 0

#--create separate vectors for each class

class1 <- subset(x1, class == 1)
class1 <- class1[,-14]
class1[is.na(class1)] <- 0
class2 <- subset(x1, class==2)
class2 <- class2[,-14]
class2[is.na(class2)] <- 0
class3 <- subset(x1, class==3)
class3 <- class3[,-14]
class3[is.na(class3)] <- 0


#--loop variable and prepopulated matrices

var <- c(1:13)
classmatrix1 <- matrix(NA, nrow = 13, ncol = 1)
classmatrix2 <- matrix(NA, nrow = 13, ncol = 1)
classmatrix3 <- matrix(NA, nrow = 13, ncol = 1)


#classmatrix4 <- matrix(NA, nrow = 13, ncol = 1)


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

#--now merge the vectors back to one data frame

classmatrixall <- cbind(classmatrix1, classmatrix2, classmatrix3)

#--and transpose it

classt <- t(classmatrixall)

#--now plot it!

plot(classt[1,], xaxt = "n", ylim=c(0,7),
     main="Figure 2. IBQ-R Young: 3-profile solution",
     xlab="",
     ylab="")
     title(ylab="Frequency Rating", line=2.2, cex.lab=1.0, family = "Garamond")
     axis(1, at = 1:14, labels = FALSE)
     text(x = seq(1, 14, by=1), -.9, labels = names(x1), srt = 90, pos = 4, xpd = TRUE)
     mtext("IBQ-R Scales: act = activity, sl = smiling/laughter, hp = high intensity pleasure, ps = perceptual sensitivity, app = approach", side = 1, line = 3)
     mtext("nvr = vocal reactivity, dl = distress to limitations, fear = fearfulness, sad = sadness, fall = falling reactivity, do = duration of orienting", side=1, line=4) 
     mtext("lp = low intensity pleasure, sooth = soothability, cudd = cuddliness", side=1, line=5)
     

lines(classt[1,], col="blue")
points(classt[2,])
lines(classt[2,], col="green")
points(classt[3,])
lines(classt[3,], col="red")


#--add a legend

legend("topright", c("profile 1", "profile 2", "profile 3"), lty=c(1,1), lwd=c(2,5, 2,5, 2,5), col=c("blue", "green", "red"))
