library(e1071)
library(caret)
library(ggplot2)
library(lattice)
library(mlbench)
library(Metrics)
library(tidyverse)
library(MASS)
library(pROC)
data("Pima.tr")
?Pima.tr
data.split <- createDataPartition(Pima.tr$type, p = .75, list = F)
training <- Pima.tr[data.split, ]
testing <- Pima.tr[-data.split, ]
view(training)

pima.glml<-glm(type ~ ., data = training, family = "binomial")
summary(pima.glml)

glm1.prob.train <- predict(pima.glml, type = "response")
glm1.prob.test <- predict(pima.glml, newdata = testing, type = "response")
glm1.type.train <- ifelse(glm1.prob.train >= .5, "Yes", "No")
glm1.type.test <- ifelse(glm1.prob.test >= .5, "Yes", "No")
accuracy(actual = training$type, predicted = glm1.type.train)
accuracy(actual = testing$type, predicted = glm1.type.test)
actual_type <- factor(testing$type)
roc(response = actual_type,
    predictor = glm1.prob.test, plot = T)

set.seed(1234)
pima.glm.cv <- train(type ~ ., data = training,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",
                                               repeats = 5, number = 3,
                                               savePredictions = T))
pima.glm.cv$results
pima.glm.cv

set.seed(1234)
pima.cv <- train(type ~ ., data = Pima.tr,
                  method = "glm", family = "binomial",
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 5, number = 3, p=.75,
                                           savePredictions = T))
pima.cv$results

pima.glm.cv$resample

ggplot(data = pima.glm.cv$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill="red")
quantile(pima.glm.cv$resample$Accuracy)

pima.glm2 <- pima.glm.cv$finalModel
summary(pima.glm2)

#q7
library(glmnet)
library(Matrix)
library(MASS)
library(foreach)
library(carData)
library(car)
data("UScereal")
?UScereal
view(UScereal)

UScereal$vitamins<-factor(UScereal$vitamins, labels = c("none" , "enriched", "full"))
data.split <- createDataPartition(UScereal$calories, p = .75, list = F)
training <- UScereal[data.split, ]
testing <- UScereal[-data.split, ]
cereal.glm <- lm( calories ~ mfr + protein + fat + sodium + fibre + carbo + sugars +shelf + vitamins, 
                   data = training)
summary(cereal.glm)
par(mfrow = c(2,2))
plot(cereal.glm)
par(mfrow = c(1,1))
vif(cereal.glm)

cereal.matrix <- model.matrix( calories ~ mfr + protein + fat + sodium + fibre + carbo + sugars +shelf + 
                                 vitamins, data = training)[, -1] 
cereal.ridge <- cv.glmnet(x = cereal.matrix, y = training$calories,
                         alpha = 0, nfolds = 5)
plot(cereal.ridge)
coef(cereal.ridge, s="lambda.min")
coef(cereal.ridge, s="lambda.1se")
cereal_predictions_ridge <-  predict(cereal.ridge, s = cereal.ridge$lambda.1se,
                                      newx = cereal.matrix)
ridge_output <- data.frame(fit = c(cereal_predictions_ridge),
                           res = c(training$calories -
                                     cereal_predictions_ridge))
ggplot(data = ridge_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
qqnorm(ridge_output$res)
qqline(ridge_output$res)

cereal.lasso <- cv.glmnet(x = cereal.matrix, y = training$calories, alpha = 1,
                         nfolds = 5)
plot(cereal.lasso)
coef(cereal.lasso, s = "lambda.min")
coef(cereal.lasso, s = "lambda.1se")
cereal_predictions_lasso <-  predict(cereal.lasso, s = cereal.lasso$lambda.1se,
                                      newx = cereal.matrix)
lasso_output<- data.frame(fit = c(cereal_predictions_lasso),
                          res = c(training$calories -
                                    cereal_predictions_lasso))
ggplot(data = lasso_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
qqnorm(lasso_output$res)
qqline(lasso_output$res)

testing.matrix <- model.matrix( calories ~ mfr + protein + fat + sodium + fibre + carbo + sugars +shelf + vitamins,
                                 data = testing)[, -1]
rmse(actual = training$calories, predicted = predict(cereal.glm))
rmse(actual = training$calories, predicted = cereal_predictions_ridge)
rmse(actual = training$calories, predicted = cereal_predictions_lasso)

rmse(actual = testing$calories,
     predicted = predict(cereal.glm, newdata = testing))
rmse(actual = testing$calories,
     predicted = predict(cereal.ridge, newx = testing.matrix,
                         s = cereal.ridge$lambda.1se))
rmse(actual = testing$calories,
     predicted = predict(cereal.lasso, newx = testing.matrix,
                         s = cereal.lasso$lambda.1se))



