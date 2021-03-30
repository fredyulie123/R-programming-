library(MASS)
library(caret)
library(pROC)
library(Metrics)
library(MASS)
library(tidyverse)
data("Pima.tr")
#Question1
Pima.split <- createDataPartition(Pima.tr$type, p = .75, list = F)
Pima.training <- Pima.tr[Pima.split, ]
Pima.testing <- Pima.tr[-Pima.split, ]
#Because we want the majority of our data to be training set, and 75 is between 70 and 80.
#Question2
Pima.glm <- glm(type ~ ., data = Pima.tr, family = "binomial")
summary(Pima.glm)
#Question3
actual.pimo.type <- factor(Pima.tr$type)
predicted.pimo.type <- factor(ifelse(
  predict(Pima.glm, type = "response") >= .5, "Yes", "No"))
predicted.pimo.probability <- predict(Pima.glm, type = "response")
accuracy(actual = actual.pimo.type, predicted = predicted.pimo.type)
roc(response = actual.pimo.type,
    predictor = predicted.pimo.probability, plot = T)

pima.prob.train <- predict(Pima.glm, newdata = Pima.training, type = "response")
pima.prob.test <- predict(Pima.glm, newdata = Pima.testing, type = "response")
pima.class.train <- ifelse(pima.prob.train >= .5, "Yes", "No")
pima.class.test <- ifelse(pima.prob.test >= .5, "Yes", "No")
accuracy(actual = Pima.training$type, predicted = pima.class.train)
accuracy(actual = Pima.testing$type, predicted = pima.class.test)
#Question4
set.seed(1234)
Pima.glm.Kfold <- train(type ~ ., data = Pima.tr,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5, repeats = 10,
                                               savePredictions = T))
Pima.glm.Kfold$results
Pima.glm.Kfold
temp = Pima.glm.Kfold$resample
#Question5
ggplot(data = Pima.glm.Kfold$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill="red")
quantile(temp$Accuracy)
#Question6


#Question7
data("UScereal")
UScereal <- UScereal[-c(10)]
cereal.lm <- lm(calories ~ protein+fat+sodium+fibre+carbo+sugars+shelf+vitamins, data = UScereal)
summary(cereal.lm)
#Question8
UScereal.matrix <- model.matrix(calories ~ protein+fat+sodium+fibre+carbo+sugars+shelf+vitamins,
                               data = UScereal)
cereal.ridge <- cv.glmnet(x = UScereal.matrix, y = UScereal$calories, alpha = 0, nfolds = 5)
plot(cereal.ridge)
cereal.lasso <- cv.glmnet(x = UScereal.matrix, y = UScereal$calories, alpha = 1, nfolds = 5)
plot(cereal.lasso)
#Question9