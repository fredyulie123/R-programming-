library(MASS)
library(car)
library(tidyverse)
library(caret)
library(e1071)
library(glmnet)
#1
data("Pima.tr")
str(Pima.tr)
summary(Pima.tr)
data.split <- createDataPartition(Pima.tr$type,p = .75,list = F)
training <- Pima.tr[data.split,]
testing <- Pima.tr[-data.split,]
#2
#logical regression model
Pima.tr$type <- relevel(Pima.tr$type,ref = "No") 
Pima.model <- glm(type~.,data = training,family = "binomial")
#model output
summary(Pima.model)
exp(coef(Pima.model))
predict(Pima.model,type = "response")


glm1.prob.train <- predict(Pima.model,type = "response")
glm1.prob.test <- predict(Pima.model,newdata = testing,type = "response")
glm1.type.train <- ifelse(glm1.prob.train >= .5, "Yes","No")
glm1.type.test <- ifelse(glm1.prob.test >= .5, "Yes","No")
#3
#model accurancy
accuracy(actual = training$type, predicted = glm1.type.train)
accuracy(actual = testing$type, predicted = glm1.type.test)
#roc
#testing
actual_pima_type_testing <- factor(testing$type)
predicted.pima.possbility_testing <- ifelse(glm1.prob.test >= .5,"1","0")
predicted.pima.possbility_testing <- as.numeric(predicted.pima.possbility_testing)
roc(response = actual_pima_type_testing,
    predictor = predicted.pima.possbility_testing, plot = T)
#training
actual_pima_type_training <- factor(training$type)
predicted.pima.possbility_training <- ifelse(glm1.prob.train >= .5,"1","0")
predicted.pima.possbility_training <- as.numeric(predicted.pima.possbility_training)
roc(response = actual_pima_type_training,
    predictor = predicted.pima.possbility_training,plot = T)
#4
set.seed(1234)
Pima.glm.cv <- train(type~.,data = training, method = "glm",family = "binomial",
                     trControl = trainControl(method = "repeatedcv",
                                              repeats = 5,number = 5,
                                              savePredictions = T))
#5
Pima.glm.cv$results
Pima.glm.cv
#accuracy distribution
ggplot(data = Pima.glm.cv$resample,aes(x=Accuracy))+
  geom_density(alpha = .2,fill="red")
quantile(Pima.glm.cv$resample$Accuracy)



#7
data("UScereal")
UScereal.split <- createDataPartition(UScereal$calories,p = .75,list = F)
training1 <- UScereal[UScereal.split,]
testing1 <- UScereal[-UScereal.split,]
str(UScereal)
UScereal.lm1 <- lm(calories~mfr+protein+fat+sodium+fibre+carbo+sugars+shelf+vitamins,data = training1)
summary(UScereal.lm1)
par(mfrow = c(2,2))
plot(UScereal.lm1)
vif(UScereal.lm1)
#8
UScereal.matrix <- model.matrix(calories~mfr+protein+fat+sodium+
                                fibre+carbo+sugars+shelf+vitamins,data = training1)[,-1]

UScereal.matrix.testing <- model.matrix(calories~mfr+protein+fat+sodium+
                                fibre+carbo+sugars+shelf+vitamins,data = testing1)[,-1]
#ridge regression
UScereal.ridge <- cv.glmnet(x=UScereal.matrix,y = training1$calories,
                            alpha = 0, nfolds = 5)
plot(UScereal.ridge)
coef(UScereal.ridge, s="lambda.min")
UScereal_predictions_ridge <-  predict(UScereal.ridge, s = UScereal.ridge$lambda.min,
                                      newx = UScereal.matrix)
#Lasso regression
UScereal.lasso <- cv.glmnet(x=UScereal.matrix,y=training1$calories,
                            alpha = 1,nfolds = 5)
plot(UScereal.lasso)
coef(UScereal.lasso,s = "lambda.min")
UScereal_predictions_lasso <- predict(UScereal.lasso,s = UScereal.lasso$lambda.min,
                                      newx = UScereal.matrix)
#model evaluation
#training
mse(actual = training1$calories, predicted = predict(UScereal.lm1))
mse(actual = training1$calories,predicted = UScereal_predictions_ridge)
mse(actual = training1$calories,predicted = UScereal_predictions_lasso)
#testing
mse(actual = testing1$calories,predicted = predict(UScereal.lm1,newdata = testing1))
mse(actual = testing1$calories,predicted = predict(UScereal.ridge,
                                                   newx = UScereal.matrix.testing,s=UScereal.ridge$lambda.min))
mse(actual = testing1$calories,predicted = predict(UScereal.lasso,
                                                   newx = UScereal.matrix.testing,s=UScereal.lasso$lambda.min))
