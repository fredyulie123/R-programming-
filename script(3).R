###############################################
########## R Statistical Programming ##########
####   Lesson 6: Linear Regression Pt. 1   ####
###############################################

# We're going to load up an outside dataset called prestige
  # Prestige comes from a social survey measuring how much prestige certain jobs
  # bring
  # It contains various pieces of info on different jobs
  # Can we develop a model to predict a job's prestige using information from 
  # the survey?

install.packages("broom")
library(broom)
library(tidyverse)

# The prestige dataset is stored in a csv file
prestige <- read_csv(file = "~/Class/Week 6/Materials/prestige.csv")
  
  
#### Checking out the data ####
  
# We'll use the str() command to look at the different data types
str(prestige)

# Summary can be used to look at distribution and counts of our data
summary(prestige)

# Let's look at the graphical distribution of prestige using a histogram
prestige %>%
  ggplot(aes(x = prestige)) +
  geom_histogram() +
  labs(title = "Distribution of Prestige")
# Sometimes histograms can be manipulated based on the number of bins chosen
  # It might be better to measure the density of the dataset
prestige %>%
  ggplot(aes(x = prestige)) +
  geom_density(fill = "lightcoral") +
  labs(title = "Distribution of Prestige")


#### Simple Linear Regression ####

# Let's say we wanted to take education, x, and predict prestige, y
  
# Let's first look at a scatterplot of education and prestige
prestige %>%
    ggplot(aes(x = education, y = prestige)) +
    geom_point(size = 2) +
    labs(title = "Relationship Between Education and Prestige")

# We can create a linear model in r using lm()
  # Let's create a model using lm() called pres.lm1
pres.lm1 <- lm(prestige ~ education, data = prestige)
# To get info from the model, we need to call summary
summary(pres.lm1)
# We can put a lot of this into a dataframe using the tidy function from the 
# broom package
tidy(pres.lm1)

# We can plot our regression line on our previous scatterplot using the 
# geom_smooth function
  # Specify the method argument to be "lm"
prestige %>%
  ggplot(aes(x = education, y = prestige)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "red", se = F) +
  labs(title = "Relationship Between Education and Prestige")

# Linear regression has assumptions
  # 1. Normality of residuals(quantile-quantile plot)
  qqnorm(pres.lm1$residuals)
  qqline(pres.lm1$residuals)
  # 2. Constant variance
  # 3. Linearity
  scatter.smooth(pres.lm1$fitted.values, pres.lm1$residuals)
  # 4. Independence of observations
# You can also plot your model directly to get these graphs immediately
par(mfrow=c(2,2))
plot(pres.lm1)
#each time see 1 plot
par(mfrow=c(1,1))
plot(pres.lm1)
  
# We may want to use our model to make future predictions
  # Let's say we had a vector of 10 new education levels
  set.seed(1234)
  new.edu <- data.frame(education = rnorm(10, mean = 10, sd = 3))
?rnorm
# We can use predict() to predict the prestige of these numbers
predict(pres.lm1, newdata = new.edu)

#### Simple Linear Regression with Categorical Independent ####

# Categorical data are treated a little bit differently in R
# Let's try and predict prestige using the type variable

# First we'll make a boxplot to show prestige distributions by type
prestige %>%
  ggplot(aes(x = type, y = prestige)) +
  geom_boxplot() +
  labs(title = "Relationship Between Type and Prestige")

# Let's make the model
pres.lm2 <- lm(prestige ~ type, data = prestige)
summary(pres.lm2)
# To see if the variable as a whole is significant, we can use anova()
anova(pres.lm2)

# Again, the same assumptions that apply with a continuous predictor apply here
par(mfrow=c(2,2))
plot(pres.lm2)
par(mfrow=c(1,1))
plot(pres.lm2)
