###############################################
########## R Statistical Programming ##########
####   Lesson 7: Linear Regression Pt. 2   ####
###############################################

# Load up some packages

install.packages("mlbench")

library(car)
library(GGally)
library(mlbench)
library(MASS)
library(tidyverse)

# We're going to be loading up the crime dataset
crime <- read_csv(file = "~/Class/Week 7/Materials/uscrime.csv")
# Also going to load up the Ozone dataset
data("Ozone")
# One more dataset: a version of that famous mtcars dataset!
mtcars <- 
  read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data")
  # Editing column V4 to make it numeric
  mtcars$V4 <- as.numeric(as.character(ifelse(mtcars$V4=="?", "NA", mtcars$V4)))


#### Getting a look at our data ####

# Let's look at a summary of our data
summary(crime)
# And also what data types we are dealing with
str(crime)

# We can create a scatterplot matrix using pairs()
pairs(crime[, c("GDP", "Ed", "y")], lower.panel = panel.smooth)
# We can do with this with the ggpairs function in ggally
ggpairs(crime[, c("GDP", "Ed", "y")], lower = list(continuous = "smooth"))


#### Developing a linear model ####

# Let's start by making a simple model with just two variables; GDP and Ed
# We'll be predicting the crime rate, y
crime.lm1 <- lm(y ~ GDP + Ed, data=crime)
summary(crime.lm1)
# Check model assumptions
par(mfrow=c(2,2))
plot(crime.lm1)
par(mfrow=c(1,1))
# We have an additional assumption that variables are not colinear
# We can have some subjective correlation value that we say is too correlated
cor(crime$GDP, crime$Ed)
# Could also check the variance inflation factor (VIF)
vif(mod = crime.lm1)

# Let's make a model using all of the variables given to us
  #In the formula interface, using a "." indicates all other variables
crime.lm2<- lm(y ~ GDP + Ed + Po1 + Po2, data = crime)
summary(crime.lm2)

# We can use anova() to compare nested models 
anova(crime.lm1, crime.lm2)

# Now let's check the model assumptions
par(mfrow = c(2,2))
plot(crime.lm2)
par(mfrow = c(1,1))
vif(crime.lm2)
# po1 and po2 appear to have very strongly correlated
crime %>%
  ggplot(aes(Po1, Po2)) +
  geom_point()
# We can remove one of these variables
crime.lm3<- lm(y ~ GDP + Ed + Po1, data = crime)
vif(crime.lm3)

# Let's look at using our first model to predict a few new observations
set.seed(1234)
new.data <- data.frame(Ed=rnorm(20, mean=105, sd=20), GDP=rnorm(20, mean=530, sd=250),
                      y=rnorm(20, mean=900, sd=500))
predict(crime.lm1, new.data)


#### Transforming the Response ####

# Using the ozone dataset to try and predict maximum ozone reading (V4)
# Create the linear model
ozone.lm1 <- lm(V4 ~ V1 + V7, data = Ozone)
summary(ozone.lm1)
# Verify assumptions
par(mfrow=c(2,2))
plot(ozone.lm1)
par(mfrow=c(1,1))

# Check out the distribution of the response
Ozone %>%
  ggplot(aes(x = V4)) +
  geom_histogram()

# Use box-cox transformation from the MASS package to help identify best transformation
bc <- boxcox(V4 ~ V1 + V7, data = Ozone)
# Identify which value of x leads to the most normalized data
bc$x[which.max(bc$y)]

# Create a model of where the response is taken to the .5 power (or square root)
ozone.lm2 <- lm(sqrt(V4) ~ V1 + V7, data = Ozone)
summary(ozone.lm2)
par(mfrow = c(2,2))
plot(ozone.lm2)
par(mfrow = c(1,1))


#### Polynomial Regression ####

# Try predicting horsepower (V3) based on mpg (V1)
mtcars %>%
  ggplot(aes(x = V1, y = V3)) +
  geom_point() +
  #smooth line
  geom_smooth(se = F, color = "red") +
  #linear line
  geom_smooth(method = "lm", se = F) +
  #curved line
  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2), color = "green")

# Try modeling the two variables 
mtcars.lm1<- lm(V3 ~ V1, data = mtcars)

# Check assumptions
par(mfrow = c(2,2))
# We see a definite non-linear trend in the resid vs. fit
  # This relationship is not linear!
plot(mtcars.lm1)
par(mfrow = c(1,1))

# We can model using polynomials of predictors
mtcars.lm2 <- lm(V3 ~ poly(V1, degree = 2), data = mtcars)
summary(mtcars.lm2)

# Use a partial f-test to determine if adding a polynomial to the function 
# improves the model
anova(mtcars.lm1, mtcars.lm2)
