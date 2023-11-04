library(mlbench)
data(HouseVotes84)
summary(HouseVotes84)
Test3 <- na.omit(HouseVotes84)
summary(Test3)
for(i in 2:17) {Test3[, i] <- as.numeric(Test3[, i])}
set.seed(123)
library(caret)
train <- createDataPartition(y=Test3$Class,p=0.7, list=FALSE)
train.data <- Test3[train, ]
train.test <- Test3[-train, ]
train <- train.data[complete.cases(train.data),]
test <- train.test[complete.cases(train.test),]
model <- glm( Class ~., data = train, family = binomial)
summary(model)
caret::varImp(model)
car::vif(model)
model2 <- step(model, direction="both")
summary(model2)
anova(model, model2, test = "LRT")
model2.pred <- predict(model2,newdata=test,type='response')
model2.pred <- ifelse(model2.pred > 0.5,'democrat', 'republican')
confusionMatrix(test$Class, as.factor(model2.pred))
prob <- predict(model2, newdata=test, type="response")
library(ROCR)
pred <- prediction(prob, test$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
###################################################
library(rpart)
tree2 <- rpart(Class ~., data=train)
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

