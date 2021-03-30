library(MASS)
library(glmnet)
library(caret)
library(pROC)
library(tidyverse)

mtcars <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data")
#editing column V4 to make it numeric
mtcars$V4 <- as.numeric(as.character(ifelse(mtcars$V4 == "?", "NA", mtcars$V4)))

data(biopsy)


#### Partial F-test ####

mtcars.lm1 <- lm(V3 ~ V1, data = mtcars)
mtcars.lm2 <- lm(V3 ~ V1 + V6, data = mtcars)

anova(mtcars.lm1, mtcars.lm2)

biopsy.glm1<- glm(class ~ V1 + V2, data=biopsy, family = "binomial")
biopsy.glm2<- glm(class ~ V1 + V2 + V3, data=biopsy, family = "binomial")

anova(biopsy.glm1, biopsy.glm2, test = "Chisq")


#### Logistic Regression Output ####

summary(biopsy.glm2)
exp(biopsy.glm2$coefficients)

biopsy_roc <- roc(response = biopsy$class,
                  predictor = predict(biopsy.glm2,
                                      type = "response"),
                  plot = T)


#### Regularized Logistic Regression ####
biopsy.matrix <- model.matrix(class ~ . -ID, data = biopsy)[, -1]

biopsy.lasso <- cv.glmnet(x = biopsy.matrix, y = na.omit(biopsy)$class, family = "binomial",
                          type.measure = "class", alpha = 1, nfolds = 5)

plot(biopsy.lasso)
coef(biopsy.lasso, s = "lambda.min")
coef(biopsy.lasso, s = "lambda.1se")


#### Cross-Validation ####

biopsy.glm.cv <- train(class ~ V1 + V2 + V3, data = biopsy, method = "glm",
                       family = "binomial", trControl = trainControl(
                         method = "repeatedcv", number = 5, repeats = 3,
                         savePredictions = T))
biopsy.glm.cv$results
biopsy.glm.cv

biopsy.glm.cv$resample
ggplot(data = biopsy.glm.cv$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill= "red" )
