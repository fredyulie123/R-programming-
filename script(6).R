#########################################
####    R Statistical Programming    ####
#### Week 9: Metrics and Enginnering ####
#########################################


#### Loading Up ####

install.packages("caret")
install.packages("Metrics")
install.packages("pROC")
library(ggplot2)
library(lattice)
library(caret)
library(pROC)
library(Metrics)
library(MASS)
library(tidyverse)

data("mpg")
data("biopsy")


#### Model Metrics ####

# Let's first build some models to predict hwy and class
mpg_model <- lm(hwy ~ displ + cyl + drv, data = mpg)
summary(mpg_model)
biopsy_model <- glm(class ~ V1 + V5 + V7, data = biopsy, family = "binomial")
summary(biopsy_model)

# For linear regression, we can look at resdiuals to determine
# model performance
actual_mpg <- mpg$hwy
predicted_mpg <- predict(mpg_model)

# MAE: mean absolute error
mean(abs(mpg_model$residuals))
mae(actual = actual_mpg, predicted = predicted_mpg)
# MSE: mean square error
mean(mpg_model$residuals^2)
mse(actual = actual_mpg, predicted = predicted_mpg)
# RMSE: root-mean square error
sqrt(mean(mpg_model$residuals^2))
rmse(actual = actual_mpg, predicted = predicted_mpg)

# For classification, we can use both our class and probability predictions
actual_biopsy_class <- factor(biopsy$class)
predicted_biopsy_class <- factor(ifelse(
  predict(biopsy_model, type = "response") >= .5, "malignant", "benign"))
predicted_biopsy_probability <- predict(biopsy_model, type = "response")

# Accuracy can be used to find the total percent accuracy
mean(actual_biopsy_class == predicted_biopsy_class)
accuracy(actual = actual_biopsy_class, predicted = predicted_biopsy_class)
# Accuracy might not tell the whole story
cm_biopsy <- xtabs(~ predicted_biopsy_class + actual_biopsy_class)
cm_biopsy

# Sensitivity is the proportion of true positives over all actual positives
cm_biopsy[2, 2] / (cm_biopsy[1, 2] + cm_biopsy[2, 2])
sensitivity(reference = actual_biopsy_class, data = predicted_biopsy_class,
            positive = "malignant")
# Specificity does the same with the negative class
cm_biopsy[1, 1] / (cm_biopsy[2, 1] + cm_biopsy[1, 1])
specificity(reference = actual_biopsy_class, data = predicted_biopsy_class,
            negative = "benign")

# The ROC curve and its AUC value give us an idea of how classification
# performs at different threshholds
roc(response = actual_biopsy_class,
    predictor = predicted_biopsy_probability, plot = T)


#### Feature Engineering ####

# Let's look at how we can manipulate some columns in mpg
# We'll add in some random columns to represent car price and year made
set.seed(1234)
mpg <- mpg %>%
  mutate(price = paste("$", round(rnorm(n = 234, mean = 20000, sd = 5000),
                                  digits = 0), sep = "")) %>%
  mutate(production_date = sample(seq(as.Date('1998/01/01'),
                                      as.Date('2008/12/31'), by="day"), 234))

# Maybe we want to simplify trans to either be auto or manual
# str_split can be used to split up a character vector
transmission <- str_split_fixed(string = mpg$trans, pattern = "[(]", n = 2)
mpg <- mpg %>%
  mutate(trans_simplified = transmission[, 1])

# Price could be good to model with, but the $ makes it a character
# Use gsub to substitute out values
mpg <- mpg %>%
  mutate(price = gsub(pattern = "[$]", replacement = "", price)) %>%
  mutate(price = as.numeric(price))

# We could pull out a specific aspect of a date type using format
mpg <- mpg %>%
  mutate(month = format(x = production_date, "%m"),
         year = format(x = production_date, "%Y"),
         day = format(x = production_date, "%d"))

