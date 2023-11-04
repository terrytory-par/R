data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(data)
summary(data)
sapply(data, sd)
xtabs(~admit + rank, data = data)

data$rank <- factor(data$rank)
logit <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")
summary(logit)

library(aod)
wald.test(Sigma = vcov(logit), b = coef(logit), Terms = 4:6)

confint(logit)
confint.default(logit)

exp(coef(logit))
exp(cbind(OR = coef(logit), confint(logit)))

newdata <- with(data, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata$rankP <- predict(logit, newdata = newdata, type = "response")

rsquared <- function(created_model) {
  dev <- created_model$deviance
  null_dev <- created_model$null.deviance
  model_n <- length(created_model$fitted.values)
  R_l <- 1 - dev / null_dev
  R_cs <- 1 - exp(-(null_dev - dev) / model_n)
  R_n <- R_cs / (1 - exp(-(null_dev / model_n)))
  cat("Pseudo R-squared for logistic regression model\n\n")
  cat("Hosmer and Lemeshow R-squared\t", round(R_l, 3), "\n")
  cat("Cox and Snell R-squared\t\t\t", round(R_cs, 3), "\n")
  cat("Nagelkerke R-squared\t\t\t\t", round(R_n, 3), "\n")
}
rsquared(logit)

#####################################################################
library(bestglm)
data("SAheart")

model1 = glm(chd ~ ldl, data = SAheart, family = binomial)
model2 = glm(chd ~ ., data = SAheart, family = binomial)

summary(model2)
anova(model1, model2, test = "LRT")

model3 <- step(model2, direction="both")



#############################################################
install.packages(c('ISLR','pscl','car'))
data <- ISLR::Default
str(data)

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]

model <- glm(default~student+balance+income, family="binomial", data=train)
summary(model)

pscl::pR2(model)["McFadden"]
caret::varImp(model)
car::vif(model)

prob <- predict(model, test, type="response")
pred <- ifelse(prob>.5, 'Yes', 'No')
table(test$default, as.factor(pred))
library(caret)
confusionMatrix(test$default, as.factor(pred))



new <- data.frame(balance = 1400, income = 2000, student = c("Yes", "No"))
predict(model, new, type="response")

#############
library(party)
tree <- ctree(default~student+balance+income, data=train)
tree

t.pred <- predict(tree, test, type="response")
table(test$default, t.pred)
confusionMatrix(test$default, t.pred)

plot(tree)
##############
library(rpart)
tree2 <- rpart(default~student+balance+income, data=train)

library(rpart.plot)
prp(tree2, type=4, extra=2)
plotcp(tree2)

tree2$variable.importance

