# Naive Bayes with Frequency Distribution

setwd("~/wentworth/machine-learning/bayes")
insect_test <- read.csv(file = "insect_test.csv")
insect_train <- read.csv(file = "insect_train.csv")

dataSP1 <-
  insect_train[which(insect_train[, 1] == 1), ]  #All rows where species == 1
dataSP2 <-
  insect_train[which(insect_train[, 1] == 2), ]  #All rows where species == 2

n1 <- nrow(dataSP1)
n2 <- nrow(dataSP2)
n12 <- n1 + n2

# Calculate probability of class 1 (c1) and class 2 c2)
pc1 <- n1 / n12
pc2 <- n2 / n12

# Using the dataset lets make a frequency distribution
# and relative freq. distr. to calculate 
# P(x1|c1), P(x2|c1), ... & P(x1|c2), ... 
# and don't assume some distribution

x <- dataSP1$ant
y <- dataSP2$ant
# Need range for freq. distr.
antena <- insect_train$ant
antena.range <- range(antena)
antena.range.low <- antena.range[1]
antena.range.high <- antena.range[2]

# Bin the different classes by antena size
stepValue <- 0.5 # The size of our bins
breaks <- seq(antena.range.low, antena.range.high, stepValue) # Each brake point
insect.cut1 <- cut(x, breaks, right = FALSE)
insect.cut2 <- cut(y, breaks, right = FALSE)
insect.freq1 <- table(insect.cut1)
insect.freq2 <- table(insect.cut2)

# Frame the data so we can manipulate it
freq.ant1 <- data.frame(insect.freq1)
freq.ant2 <- data.frame(insect.freq2)
colnames(freq.ant1) <- c("Interval", "Frequency")
colnames(freq.ant2) <- c("Interval", "Frequency")

# Calculate midpoints or class marks
midpoints <- seq(antena.range.low + (stepValue/2), antena.range.high - (stepValue/2), stepValue)

# Calculate relative frequencies
freq.relative1 <- freq.ant1$Frequency / sum(freq.ant1$Frequency)
freq.relative2 <- freq.ant2$Frequency / sum(freq.ant2$Frequency)

# Make data.frames with midpoint and rel. freq. (Maybe make array for more features)
prob_1_insect_ant <- data.frame(midpoints, freq.relative1)
prob_2_insect_ant <- data.frame(midpoints, freq.relative2)
colnames(prob_1_insect_ant) <- c("x", "prob1")
colnames(prob_2_insect_ant) <- c("x", "prob2")

# Number of rows in the test set
insect_test.length <- nrow(insect_test)
nc <- nrow(prob_1_insect_ant)

# Calculate the distances of the test data points
# from each modpoint in freq. distr. to determine
# which bin it goes in
d <- numeric()
p1 <- numeric()
p2 <- numeric()
cl <- integer()
for (i in 1:insect_test.length) {
  test.ant <- insect_test$ant[i]
  for (j in 1:nc) {
    d[j] = abs(test.ant - prob_1_insect_ant$x[j])
  }
  distances <- cbind(d) # Bind distances as columns. Probably not required
  bin <- which(distances == min(distances)) #nm
  
  # Calculate likelyhood of class 1 and 2
  p1[i] <- prob_1_insect_ant$prob1[bin]
  p2[i] <- prob_2_insect_ant$prob2[bin]
  prob_both <- cbind(p1[i], p2[i]) #nm1
  probable_class <- which(prob_both == max(prob_both)) # Finds max likely hood
  cl[i] = probable_class # Predicted class
}

all_results <- data.frame(cl, insect_test$c, p1, p2)
colnames(all_results) <- c("Predicted", "Actual", "Prob. of 1", "Prob. of 2")

write.table(all_results, row.names = FALSE, sep = ", ")

# Do confusion matrix







