setwd("~/wentworth/machine-learning/bayes")
#Get data:
insect_test <- read.csv(file = "insect_test.csv")
insect_train <- read.csv(file = "insect_train.csv")
dataSP1 <-
  insect_train[which(insect_train[, 1] == 1), ]  #All rows where species == 1
dataSP2 <-
  insect_train[which(insect_train[, 1] == 2), ]  #All rows where species == 2

#Scatter Plot:
#pdf("scatter_plot_insect.pdf")
plot(
  abd ~ ant,
  data = dataSP1,
  col = "red",
  xlab = "Abdomen Length",
  ylab = "Antenna Length",
  main = "Insect Data"
)
points(abd ~ ant, data = dataSP2, col = "blue")
#dev.off() #Uncomment if saving as pdf above

#Frequency distributions (Histograms):
insect_train_abd <-
  insect_train[,-3] #Remove column for antenae length
insect_train_ant <-
  insect_train[,-2] #Remove column for abdomen length

#Abdomen:
data1 <-
  insect_train_abd[which(insect_train_abd[, 1] == 1), ]  #All rows where species == 1 (without ant column)
data2 <-
  insect_train_abd[which(insect_train_abd[, 1] == 2), ]  #All rows where species == 2 (without ant column)
#pdf("insect_hist_abd.pdf")
hist(
  data1$abd,
  breaks = 20,
  prob = T,
  col = rgb(1, 0, 0, 0.5),
  xlim = c(0, 7),
  ylim = c(0, 0.7),
  main = "Insects",
  xlab = "Abdomen Length"
)
lines(density(data1$abd), col = "black", lwd = 4)
hist(
  data2$abd,
  breaks = 20,
  prob = T,
  col = rgb(0, 0, 1, 0.5),
  add = T
)
lines(density(data2$abd), col = "black", lwd = 4)
#dev.off() #Uncomment if saving as pdf above

p1 <- numeric()
p2 <- numeric()
cl <- integer()
#means and sd's for x in both classes
me1 = mean(data1$abd)
me2 = mean(data2$abd)
sd1 = sd(data1$abd)
sd2 = sd(data2$abd)
n <- length(insect_test$abd)
# loop over test set for abdomen length
# and calculate z score
for (i in 1:n) {
  z1 = (insect_test$abd[i] - me1) / sd1
  z2 = (insect_test$abd[i] - me2) / sd2
  y1 = dnorm(z1, 0, 1)
  y2 = dnorm(z2, 0, 1)
  p1[i] = y1 / (y1 + y2)
  p2[i] = y2 / (y1 + y2)
  probboth <- cbind(p1[i], p2[i])
  nc <-
    which(probboth == max(probboth)) # Can use an if statement here
  cl[i] = nc
}

allresults <- data.frame(cl, insect_test$c, p1, p2)
colnames(allresults) <-
  c("Predicted", "Actual", "Prob_class1", "Prob_class2")
write.table(
  allresults,
  file = "TestResults_abd.csv",
  sep = ",",
  quote = FALSE,
  eol = "\n",
  append = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

TN = 0
TP = 0
FN = 0
FP = 0

c1 <- numeric()
c2 <- numeric()
for (i in 1:n) {
  c1[i] <- allresults$Actual[i]
  c2[i] <- allresults$Predicted[i]
  if (c1[i] == 1) {
    if (c2[i] == 1) {
      TN = TN + 1
    } else {
      FN = FN + 1
    }
  } else {
    if (c2[i] == 2) {
      TP = TP + 1
    } else {
      FP = FP + 1
    }
  }
}

# Calculate overall access
OA = (TN + TP) / n
AC1 = (TN) / (TN + FN)
AC2 = (TP) / (TP + FP)

print(OA)
print(AC1)
print(AC2)




