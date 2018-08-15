setwd("~/wentworth/machine-learning/bayes")
# library(mvtnorm)
library(mnormt)
library(caret)

insect.test <- read.csv("insect_test.csv") # insect.test
insect.train <- read.csv("insect_train.csv") # insect.train

class1 <- insect.train[which(insect.train[, 1] == 1),] # data1
class1.noclass <- class1[, -1] #data1nc
class1.length <- nrow(class1)

class2 <- insect.train[which(insect.train[, 1] == 2),] # data2
class2.noclass <- class2[, -1] #data2nc
class2.length <- nrow(class2)

# Mean vector class 1
class1.abd.mean <- mean(class1.noclass$abd) # me11
class1.ant.mean <- mean(class1.noclass$ant) # me12
class1.meanVector <- c(class1.abd.mean, class1.ant.mean) #mu1
# Mean vector class 2
class2.abd.mean <- mean(class2.noclass$abd) # me21
class2.ant.mean <- mean(class2.noclass$ant) # me22
class2.meanVector <- c(class2.abd.mean, class2.ant.mean) #mu2

# Covariance matrix class 1
class1.co <- matrix(0.000, nrow = 2, ncol = 2) # co1
class1.covariance <- matrix(0.000, nrow = 2, ncol = 2) # cov1
for (i in 1:class1.length) {
  class1.abd <- class1.noclass$abd[i] # x
  class1.ant <- class1.noclass$ant[i] # y
  class1.vector <- c(class1.abd, class1.ant) # v
  d <- class1.vector - class1.meanVector # d
  class1.co <- d %o% d # co1
  class1.covariance <- class1.covariance + class1.co # cov1
}
class1.covariance <- class1.covariance * (1 / nrow(class1.noclass)) # cov1

# Covariance matrix class 2
class2.co <- matrix(0.000, nrow = 2, ncol = 2) # co2
class2.covariance <- matrix(0.000, nrow = 2, ncol = 2) # cov2
for (i in 1:class2.length) {
  class2.abd <- class2.noclass$abd[i] # x
  class2.ant <- class2.noclass$ant[i] # y
  class2.vector <- c(class2.abd, class2.ant) # v
  d <- class2.vector - class2.meanVector # d
  class2.co <- d %o% d # co2
  class2.covariance <- class2.covariance + class2.co # cov2
}
class2.covariance <- class2.covariance * (1 / nrow(class2.noclass)) # cov2

# Naive bayes classification
class1.prob <- numeric() # p1
class2.prob <- numeric() # p2
classification <- integer() # cl
for(i in 1:nrow(insect.test)) {
  test.abd <- insect.test$abd[i] # x
  test.ant <- insect.test$ant[i] # y
  f1 <- dmnorm(c(test.abd, test.ant), class1.meanVector, class1.covariance) # f1
  f2 <- dmnorm(c(test.abd, test.ant), class2.meanVector, class2.covariance) # f2
  class1.prob[i] <- f1 / (f1 + f2) # p1[i]
  class2.prob[i] <- f2 / (f1 + f2) # p2[i]
  probboth <- cbind(class1.prob[i], class2.prob[i]) # probboth
  nm <- which(probboth == max(probboth)) # nm
  classification[i] = nm # cl[i]
}

# table
# confusion matrix

msize <- 100
x.points <- seq(0, 8, length.out = msize) # x.points
y.points <- seq(0, 8, length.out = msize) # y.points

class1.z <- matrix(0, nrow = msize, ncol = msize)
class2.z <- matrix(0, nrow = msize, ncol = msize)

for (i in 1:msize) {
  for (j in 1:msize) {
    class1.z[i, j] <- dmnorm(c(x.points[i], y.points[j]), class1.meanVector, class1.covariance)
    class2.z[i, j] <- dmnorm(c(x.points[i], y.points[j]), class2.meanVector, class2.covariance)
  }
}

pdf("2Dnorm.pdf")
contour(x.points, y.points, class1.z)
contour(x.points, y.points, class2.z, add = TRUE)
dev.off()













