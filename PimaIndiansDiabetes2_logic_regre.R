library(mlbench)
data("PimaIndiansDiabetes2")
summary(PimaIndiansDiabetes2)
data1 <- na.omit(PimaIndiansDiabetes2)
summary(data1)
library(DMwR2)
knn_data <- knnImputation(PimaIndiansDiabetes2)
summary(knn_data)
set.seed(123)
library(caret)
knn_train <- createDataPartition(y=knn_data$diabetes,p=0.7, list=FALSE)
knn_train.data <- knn_data[knn_train, ]
knn_test.data <- knn_data[-knn_train, ]
model1 <- glm( diabetes ~., data = knn_train.data, family = binomial)
summary(model1)
caret::varImp(model1)
car::vif(model1)
model2 <- step(model1, direction="both")
summary(model2)
anova(model1, model2, test = "LRT")
model2.pred <- predict(model2,newdata=knn_test.data,type='response')
model2.pred <- ifelse(model2.pred > 0.5,'pos', 'neg')
confusionMatrix(knn_test.data$diabetes, as.factor(model2.pred))
