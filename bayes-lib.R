setwd("~/wentworth/machine-learning/bayes")
library(caret)
library(lattice)
library(ggplot2)
library(klaR)

data(iris)

x <- iris[, -5]
y <- iris$Species
NBmodel <- train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))
pred <- predict(NBmodel$finalModel, x)

t <- table(pred$class, y)
print(t)

naive_iris <- NaiveBayes(iris$Species~., data = iris)
#pdf("irisdensityplots.pdf")
plot(naive_iris)
#dev.off()








