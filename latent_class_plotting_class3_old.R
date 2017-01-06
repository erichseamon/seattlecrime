
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

x1 <- data.frame(read.csv("/nethome/erichs/IBQ-R_old_orig_Erich.csv"))

#--removes th id field, occurant for some csv files masha provides lol

#x1 <- x1[,-1]
colnames(x1)[15] <- "class"
colnames(x1)[13] <- "sth"

#--removes the class field for use later, and change NA to zero

xx <- x1[,-15]
#xx[is.na(xx)] <- 0

#--create separate vectors for each class

class1 <- subset(x1, class == 1)
class1 <- class1[,-15]
class1[is.na(class1)] <- 0
class2 <- subset(x1, class==2)
class2 <- class2[,-15]
class2[is.na(class2)] <- 0
class3 <- subset(x1, class==3)
class3 <- class3[,-15]
class3[is.na(class3)] <- 0


#--loop variable and prepopulated matrices

var <- c(1:14)
classmatrix1 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix2 <- matrix(NA, nrow = 14, ncol = 1)
classmatrix3 <- matrix(NA, nrow = 14, ncol = 1)


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

#--now plot it

par(mar=c(8.1,4.1,4.1,2.1))

plot(classt[1,], xaxt = "n", ylim=c(0,7),
     main="Figure 2. 3-profile solution for the older subsample", adj=0,
     xlab="",
     ylab="")
     title(ylab="Frequency Rating", line=2.2, cex.lab=1.0, family = "Garamond")
     axis(1, at = 1:14, labels = FALSE)
     text(x = seq(1, 14, by=1), -.85, labels = names(xx), srt = 90, pos = 4, xpd = TRUE)
     mtext("IBQ-R Scales: act = activity, sl = smiling/laughter, hp = high intensity pleasure, ps = perceptual sensitivity, app = approach", side = 1, line = 3)
     mtext("nvr = vocal reactivity, dl = distress to limitations, fear = fearfulness, sad = sadness, fall = falling reactivity, do = duration of orienting", side=1, line=4) 
     mtext("lp = low intensity pleasure, sth = soothability, cudd = cuddliness", side=1, line=5)
     

lines(classt[1,], lty=1, lwd=5, col="gray")
points(classt[2,])
lines(classt[2,], lty=2, lwd=5, col="gray")
points(classt[3,])
lines(classt[3,], lty=3, lwd=5, col="gray")


#--add a legend

legend("bottomright", c("Profile 1: Low Approach/Difficult to Calm", "Profile 2: Fearless/Inattentive", "Profile 3: High Positive/Regulated"), lty=c(1,2,3), lwd=c(3,5, 3,5, 3,5), col=c("gray", "gray", "gray"))
