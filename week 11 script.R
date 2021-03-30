#########################################
####### R Statistical Programming #######
####  Lesson 11: Feature Selection  #####
#########################################

library(MASS)
library(car)
install.packages("carData")
install.packages("glmnet")
install.packages("Matrix")
install.packages("foreach")
library(glmnet)
library(Metrics)
library(tidyverse)
library(Matrix)
library(foreach)
library(carData)

#### Data and Plots ####

# Let's load up the us crime dataset from the mass package

data("UScrime")
head(UScrime)
# We'll change So to a factor
UScrime$So <- factor(UScrime$So, labels = c("No", "Yes"))


#### Linear Regression ####

# Let's first take a look at a full model with all variables of interest
crime.lm1<- lm(y ~ M + So + Ed + Po1 + LF + M.F + Pop + NW + U1 + Ineq + Time,
               data = UScrime[1:40, ])
summary(crime.lm1)
#plot to check assumptions
par(mfrow = c(2,2))
plot(crime.lm1)
par(mfrow = c(1,1))
vif(crime.lm1)


#### Ridge Regression ####

# We need to create a model matrix before inputting data into the model
UScrime.matrix <- model.matrix(y ~ M + So + Ed + Po1 + LF + M.F + Pop + NW +
                                 U1 + Ineq + Time,
                               data = UScrime[1:40, ])[, -1]

# We can now imput the data into glmnet
crime.ridge <- cv.glmnet(x = UScrime.matrix, y = UScrime[1:40, ]$y,
                         alpha = 0, nfolds = 5)

# We can plot the values of lambda to identify which values lead to smallest mse
plot(crime.ridge)

# We can identify coefficients of the lambda that produces the smallest mse
coef(crime.ridge, s="lambda.min")
# Or the one that is the most parsimonious while being 1 standard error away
# from the min mse
coef(crime.ridge, s="lambda.1se")

# Predictions can be retrieved like so
UScrime_predictions_ridge <-  predict(crime.ridge, s = crime.ridge$lambda.1se,
                                      newx = UScrime.matrix)

# Assumptions still apply!
ridge_output <- data.frame(fit = c(UScrime_predictions_ridge),
                           res = c(UScrime[1:40, ]$y -
                                     UScrime_predictions_ridge))
# Residuals vs. Fitted Plot
ggplot(data = ridge_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
# QQ Plot
qqnorm(ridge_output$res)
qqline(ridge_output$res)


#### LASSO Regression ####
  
# Set alpha = 1 for LASSO regularization
crime.lasso <- cv.glmnet(x = UScrime.matrix, y = UScrime[1:40, ]$y, alpha = 1,
                         nfolds = 5)

# We can plot the values of lambda to identify which values lead to smallest mse
plot(crime.lasso)

# We can identify coefficients of the lambda that produces the smallest mse
coef(crime.lasso, s = "lambda.min")
coef(crime.lasso, s = "lambda.1se")

# Predictions can be retrieved like so
UScrime_predictions_lasso <-  predict(crime.lasso, s = crime.lasso$lambda.1se,
                                      newx = UScrime.matrix)

# Assumptions still apply!
lasso_output<- data.frame(fit = c(UScrime_predictions_lasso),
                          res = c(UScrime[1:40, ]$y -
                                    UScrime_predictions_lasso))
# Residuals vs. Fitted
ggplot(data = lasso_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
# QQ plot
qqnorm(lasso_output$res)
qqline(lasso_output$res)


#### Model Evaluation ####

# We'll create a testing matrix
testing.matrix <- model.matrix(y ~ M + So + Ed + Po1 + LF + M.F + Pop + NW +
                                 U1 + Ineq + Time,
                               data = UScrime[41:47, ])[, -1]

# Look at how well models perform on training data
rmse(actual = UScrime[1:40, ]$y, predicted = predict(crime.lm1))
rmse(actual = UScrime[1:40, ]$y, predicted = UScrime_predictions_ridge)
rmse(actual = UScrime[1:40, ]$y, predicted = UScrime_predictions_lasso)
#but what about testing data?
rmse(actual = UScrime[41:47, ]$y,
     predicted = predict(crime.lm1, newdata = UScrime[41:47, ]))
rmse(actual = UScrime[41:47, ]$y,
     predicted = predict(crime.ridge, newx = testing.matrix,
                         s = crime.ridge$lambda.1se))
rmse(actual = UScrime[41:47, ]$y,
     predicted = predict(crime.lasso, newx = testing.matrix,
                         s = crime.lasso$lambda.1se))

