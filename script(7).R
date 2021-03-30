#########################################
####### R Statistical Programming #######
####   Lesson 10: Model Validation  #####
#########################################

# Install and load the caret package
install.packages("caret")
install.packages("e1071")
library(e1071)
library(caret)
library(ggplot2)
library(lattice)
# Bring the sonar dataset into our environment
library(mlbench)
data("Sonar")
?Sonar
# We'll take a subset of these sonar columns
Sonar <- Sonar[, c(1:20, 61)]

library(Metrics)
library(tidyverse)

#### Building a Basic GLM ##
# We can use caret's create data partition to split our data into a
# training and test set
data.split <- createDataPartition(Sonar$Class, p = .75, list = F)
# Index Sonar by data.split to get a training and testing set
training <- Sonar[data.split, ]
testing <- Sonar[-data.split, ]
view(training)

# Use glm to try and predict class
sonar.glm1 <- glm(Class ~ ., data = training, family = "binomial")
summary(sonar.glm1)

# Get the predicted probabilities and classes
glm1.prob.train <- predict(sonar.glm1, type = "response")
glm1.prob.test <- predict(sonar.glm1, newdata = testing, type = "response")
glm1.class.train <- ifelse(glm1.prob.train >= .5, "R", "M")
glm1.class.test <- ifelse(glm1.prob.test >= .5, "R", "M")
#find accuracy of model
accuracy(actual = training$Class, predicted = glm1.class.train)
accuracy(actual = testing$Class, predicted = glm1.class.test)


#### Using Caret for Model Building ####

# Use train to develop a model and specify what method you want to use
set.seed(1234)
sonar.glm.cv <- train(Class ~ ., data = training,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",
                                               repeats = 5, number = 3,
                                               savePredictions = T))
sonar.glm.cv$results
sonar.glm.cv

## train original data
set.seed(1234)
sonar.cv <- train(Class ~ ., data = Sonar,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",
                                               repeats = 5, number = 3, p=.75,
                                               savePredictions = T))
sonar.cv$results

# By calling resample, we can see how caret performed on each held out fold
sonar.glm.cv$resample
# We can even plot the accuracy distribution
ggplot(data = sonar.glm.cv$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill="red")

# If we needed to get a non-caret object, we can extract the final model
sonar.glm2 <- sonar.glm.cv$finalModel
summary(sonar.glm2)


# We can try different validation methods by specifying them in
# the control parameter
set.seed(123)
sonar.glm.cv2 <- train(Class ~ ., data = training,
                       method = "glm", family = "binomial",
                       trControl=trainControl(method = "boot", number = 5,
                                              savePredictions = T))
sonar.glm.cv2
sonar.glm.cv2$resample
  
# Calling predict on the caret output brings back class predictions
class.predictions <- predict(sonar.glm.cv, newdata = testing)
# We can create confusion matrices with the caret package
confusionMatrix(data = class.predictions, reference = testing$Class,
                positive = "R")
