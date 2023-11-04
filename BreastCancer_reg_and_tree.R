library(mlbench)
data(BreastCancer)
summary(BreastCancer)
data2 <- na.omit(BreastCancer)
data2 <- data2[,-1]
for(i in 1:9) {data2[, i] <- as.numeric(as.character(data2[, i]))}
summary(data2)
set.seed(123)
library(caret)
train2 <- createDataPartition(data2$Class,p=0.7, list=FALSE)
train2.data <- data2[train2, ]
test2.data <- data2[-train2, ]
train <- train2.data[complete.cases(train2.data),]
test <- test2.data[complete.cases(test2.data),]
model2 <- glm( Class ~ ., data = train2.data, family = binomial)
summary(model2)
caret::varImp(model2)
car::vif(model2)
model3 <- step(model2, direction="both")
summary(model3)
anova(model2, model3, test = "LRT")
model3.pred <- predict(model3,newdata=test,type='response')
model3.pred <- ifelse(model3.pred > 0.5,'benign', 'malignant')
confusionMatrix(test$Class, as.factor(model3.pred))
library(ROCR)
prob <- predict(model3, newdata=test, type="response")
pred <- prediction(prob, test$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
################################################
library(rpart)
tree2 <- rpart(Class ~., data=train)
tree2
varImp(tree2)
plotcp(tree2)
tree2$variable.importance
t2.pred <- predict(tree2, test, type="class")
confusionMatrix(test$Class, t2.pred)
prob <- predict(tree2, newdata=test, type="vector")
pred <- prediction(prob, test$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


