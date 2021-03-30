###########################################
####### R Statistical Programming #########
####   Lesson 8: Logistic Regression  #####
###########################################

# Load in the biopsy dataset from MASS package
library(MASS)
library(car)
library(tidyverse)
data(biopsy)
?biopsy


#### Pre-analysis ####

# Removing missing values
biopsy <- na.omit(biopsy)
summary(biopsy)

#We'll make a numeric version of class for plotting purposes
biopsy<-
  biopsy %>%
  mutate(class2 = ifelse(class == "benign", 0, 1))

# Visualizing the relationship between class and V7, bland chromatin
ggplot(data = biopsy, aes(x = V7, y = class2)) +
  geom_jitter(height = .01, alpha = .3) + #clustering 
  geom_smooth(method = "lm", se = F)


#### Modeling ####

# Let's begin by modeling the class by linear regression
tumor.lm <- lm(class2 ~ V1 + V3 + V4 + V5 + V6 + V7 + V8 + V9, data = biopsy)
summary(tumor.lm)
# We can see some clear non-linearity issues in our assumptions
par(mfrow=c(2,2))
plot(tumor.lm)
par(mfrow=c(1,1))
vif(tumor.lm)
# We also have some predictions that don't make sense
round(predict(tumor.lm), 4)

# It would be better to model using logistic regression
# Let's try plotting a logistic relationship between V7 and class
ggplot(data = biopsy, aes(x = V7, y = class2)) +
  geom_jitter(height = .01, alpha = .3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              formula = y~x, se = F, color = "red") +
  geom_smooth(method = "lm", se=F)
# Developing the model
# We are predicting the probability of the non-reference level
  # We can change the reference level using relevel
biopsy$class <- relevel(biopsy$class, ref = "benign")
tumor.glm <- glm(class ~ V1 + V3 + V4 + V5 + V6 + V7 + V8 + V9, data = biopsy,
                 family = "binomial")
summary(tumor.glm)
# Simply calling predict gives the predicted log odds
predict(tumor.glm)
#We have to specify type = "response" to get predicted probabilities
predict(tumor.glm, type = "response")
summary(predict(tumor.glm, type = "response"))

exp(coef(tumor.glm))
vif(tumor.lm,tumor.glm)

# We can identify variables that could use a polynomial by plotting the x value
# against the predicted log odds
  # This relationship should be linear
biopsy2 <- biopsy %>%
  mutate(log.odds = predict(tumor.glm))
ggplot(data = biopsy2, aes(x = V7, y = log.odds)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, lty = 2,
              color = "red")

# We can measure model accuracy by rounding the predicted probabilities
predict.class <- round(predict(tumor.glm, type="response"), digits = 0)
mean(predict.class == biopsy$class2)
# We can also create a confusion matrix to help identify by class accuracy
xtabs(~ biopsy$class + predict.class)
predict.class
