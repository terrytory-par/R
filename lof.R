install.packages("e1071")
library(e1071)
data(iris)
N <- nrow(iris)
set.seed(1)
train.index <- sample(N, size = N*0.7)
features <- 4
train.x <- iris[train.index, features, drop=F]
train.y <- iris[train.index, 5]
test.x <- iris[-train.index, features, drop=F]
test.y  <- iris[-train.index, 5]
model <- naiveBayes(train.x, train.y)
pred <- predict(model, test.x)
table(pred, test.y)
sum(pred == test.y) / length(pred)

#
#install.packages("DMwR")
#library(DMwR)
#iris2 <- iris[,1:4]
#outlier.scores <- lofactor(iris2, k=5)
#outliers <- order(outlier.scores, decreasing=T)[1:5]
#print(outliers)
#print(iris2[outliers,])

install.packages("Rlof")
library(Rlof)
iris2 <- iris[,1:4]
outlier.scores <- lof(iris2, k=5)
#outlier.scores <- lof(iris2, k=c(5:10))
#outlier.scores
outliers <- order(outlier.scores, decreasing=T)[1:5]
outliers 
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o", cex=0.5)
#points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)

irisLOF <- iris[-outliers,]
N <- nrow(irisLOF)
irisLOFindex <- c(1:150)
irisLOFindex <- irisLOFindex[-outliers]
set.seed(1)
train.index <- sample(irisLOFindex, size = N*0.7)
train.x <- irisLOF[train.index, 4, drop=F]
train.y <- irisLOF[train.index, 5]
test.x <- irisLOF[-train.index, 4, drop=F]
test.y  <- irisLOF[-train.index, 5]
model <- naiveBayes(train.x, train.y)
pred <- predict(model, test.x)
table(pred, test.y)
sum(pred == test.y) / length(pred)

