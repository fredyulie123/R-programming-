library(tidyverse)
library(tidyr)
library(car)
library(caret)
library(pROC)
library(Metrics)
library(MASS)
library(ggplot2)
library("e1071")
library(GGally)
library(glmnet)
install.packages("RSNNS")
library(Rcpp)
library(RSNNS)
##Data pre-analysis
#check if have mssing value
sum(is.na(bryant.data.set.v2$Ozone))
#There are no missing value
kobe <- bryant.data.set.v2
view(kobe)
#describe the data type
str(kobe)
summary(kobe$action_type)
#23 variables:11 Factor and 12 integer
#change data type from integer to factor
a <- as.factor(kobe$period)
kobe$period <- a
b <- as.factor(kobe$playoffs)
kobe$playoffs <- b
c <- as.factor(kobe$shot_made_flag)
kobe$shot_made_flag <- c
d <- as.character(kobe$game_date)
kobe$game_date <- d
e <- as.Date(kobe$game_date,format = '%m/%d/%Y')
kobe$game_date <- e
str(kobe)
#delet team id and team name
kobe <- kobe[,-18]
kobe <- kobe[,-18]
#add new col named year
kobe <- kobe %>%
  mutate(year = format(x=kobe$game_date,'%Y')) 
kobe <- kobe %>%
  mutate(age = as.numeric(kobe$year)-1978)
#add col named home 1 means home
kobe <- kobe %>%
  mutate(home = str_extract(string = kobe$matchup,pattern = "[@]")) 
kobe <- kobe %>%
  mutate(home = str_replace(string = kobe$home,pattern = "@",replacement = "0"))
kobe[is.na(kobe)] <- 1
kobe <- kobe[,-19]
kobe_1 <- kobe
str(kobe_1)

### Modeling
##1.logistic model
#data split
glm.data.split <- createDataPartition(kobe_1$shot_made_flag, p = .75, list = F)
training <- kobe_1[glm.data.split,]
testing <- kobe_1[-glm.data.split,]
#build model
kobe.glm1 <- glm(data = training,shot_made_flag~combined_shot_type+period+playoffs
                +minutes_remaining+seconds_remaining+shot_distance+age+home,family = 'binomial')
kobe.glm2 <- step(object = kobe.glm1,trace = 0)

                
summary(kobe.glm1)
summary(kobe.glm2)
vif(kobe.glm2)
anova(kobe.glm1,kobe.glm2)
#predict model result
glm1.prob.train <- predict(kobe.glm2, type = "response")
glm1.prob.test <- predict(kobe.glm2, newdata = testing, type = "response")
glm1.class.train <- ifelse(glm1.prob.train >= .5, "1", "0")
glm1.class.test <- ifelse(glm1.prob.test >= .5, "1", "0")
#check accuracy of model
accuracy(actual = training$shot_made_flag, predicted = glm1.class.train)
accuracy(actual = testing$shot_made_flag, predicted = glm1.class.test)
#cv
set.seed(1234)
kobe.glm.cv <- train(shot_made_flag~combined_shot_type+period+playoffs
                     +minutes_remaining+seconds_remaining+shot_distance+age+home, data = training,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",
                                               repeats = 5, number = 5,
                                               savePredictions = T))
kobe.glm.cv$results
class.prediction <- predict(kobe.glm.cv,newdata = testing)
confusionMatrix(data = class.prediction, reference = testing$shot_made_flag,
                positive = "1")
kobe.glm.cv2 <- kobe.glm.cv$finalModel
summary(kobe.glm.cv2)

#roc
roc(response = training$shot_made_flag,
    predictor = glm1.prob.train, plot = T)


##linear model
#Engineer new goal data
#import kobe_goal
kobe_goal_1 <- kobe_goal
kobe_goal_1 <- unite(kobe_goal,"game_info",game_id,playoffs,opponent,year,age,home)
kobe_goal_2 <- str_split_fixed(string = kobe_goal_1$shot_type, pattern = "P", n = 2)
kobe_goal_1 <- kobe_goal_1 %>%
  mutate(shot_type = kobe_goal_2[,1])
#output our data
write.csv(kobe_goal_1,file="C:/Users/dell/Desktop/MSIS/R/Final project/kobe_goal_1.csv",quote=F,row.names = F)
#Find corrlationships
kobe_model <- kobe_goal_model2
#remove NA value
kobe_model[is.na(kobe_model)] <- 0
str(kobe_model)
ggplot(data = kobe_model, aes(x=kobe_model$X2PT.times,y=kobe_model$goal))+
  geom_point()+
  geom_smooth(ethod = "lm",color = "red",se = F)
cor(kobe_model$X2PT.times,kobe_model$goal)

ggplot(data = kobe_model, aes(x=kobe_model$X3PT.times,y=kobe_model$goal))+
  geom_point()
cor(kobe_model$X3PT.times,kobe_model$goal)

ggplot(data = kobe_model, aes(x=kobe_model$X2PT.times,y=kobe_model$X3PT.times))+
  geom_point()

ggplot(data = kobe_model, aes(x=kobe_model$X2.PTgoalrate,y=kobe_model$goal))+
  geom_point()+
  geom_smooth(method = "lm", color = "red", se = F)+
  geom_smooth(method = "auto", color = "blue", se = F)+
  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 3), color = "green")

ggplot(data = kobe_model,aes(x=kobe_model$X3.PTgoalrate,y=kobe_model$goal))+
  geom_point()+
  geom_smooth(method = "auto", color = "red", se = F)

ggplot(data = kobe_model, aes(x=kobe_model$total.goalrate,y=kobe_model$goal))+
  geom_point()+
  geom_smooth(method = "auto", color = "blue", se = F)+
  geom_smooth(method = "lm", color = "red", se = F)+
  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 3), color = "green")

##build linear model
#data split
lm.data.split <- createDataPartition(kobe_model$goal, p = .75, list = F)
lm.training <- kobe_model[lm.data.split,]
lm.testing <- kobe_model[-lm.data.split,]
kobe.lm <- lm(kobe_model$goal ~ kobe_model$X2PT.times+kobe_model$X3PT.times+kobe_model$X2.PTgoalrate,
              data = lm.training)
summary(kobe.lm)
#check assumption
par(mfrow=c(2,2))
plot(kobe.lm)
vif(kobe.lm)
#our model is not linear,so use polynominal
kobe.poly <- lm(kobe_model$goal~kobe_model$X2PT.times+kobe_model$X3PT.times+
                  poly(kobe_model$X2.PTgoalrate,degree = 3),data = lm.training)
summary(kobe.poly)
vif(kobe.poly)
anova(kobe.lm,kobe.poly)
#model should be polynomial 
#model evaluation
mse(actual = lm.training$goal,predicted = predict(kobe.poly))
mse(actual = lm.testing$goal,predicted = predict(kobe.poly,newdata = lm.testing))
#Improve our data bu split game_info
kobe_goal_model2 <- kobe_model 
game_info_split <- str_split_fixed(string = kobe_goal_model2$`game-id`,pattern = "_",n = 6)
#Get new dataset including more features
kobe_goal_model2 <- kobe_goal_model2 %>%
  mutate(playoffs = game_info_split[,2]) %>%
  mutate(opponent = game_info_split[,3]) %>%
  mutate(age = game_info_split[,5]) %>%
  mutate(home = game_info_split[,6]) %>%
  mutate(game_id = game_info_split[,1])
str(kobe_goal_model2)
kobe_goal_model2$age <- as.numeric(kobe_goal_model2$age)
##Build linear model 2
#Data split
set.seed(1234)
lm2.data.split <- createDataPartition(kobe_goal_model2$goal, p = .75, list = F)
lm2.training <- kobe_goal_model2[lm2.data.split,]
lm2.testing <- kobe_goal_model2[-lm2.data.split,]

kobe.lm2 <- lm(goal~playoffs+opponent+home+age+X2PT.times+X3PT.times+
                 poly(X2.PTgoalrate,degree = 3)
               ,data = lm2.training)
summary(kobe.lm2)
kobe.lm2 <- step(object = kobe.lm2,trace = 0)
summary(kobe.lm2)
par(mfrow=c(2,2))
plot(kobe.lm2)
vif(kobe.lm2)
anova(kobe.lm,kobe.lm2)
# model evaluation 
rmse(actual = lm2.training$goal, predicted = predict(kobe.lm2))
rmse(actual = lm2.testing$goal, predicted = predict(kobe.lm2,newdata = lm2.testing))
## Lasso regression
kobe.lm2.matrix <- model.matrix(goal~playoffs+opponent+age+home+X2PT.times+X3PT.times+
                                  poly(X2.PTgoalrate,degree = 3),
                               data = lm2.training)[, -1]

lm2.kobe.lasso <- cv.glmnet(x = kobe.lm2.matrix, y = lm2.training$goal, alpha = 1,
                         nfolds = 5)
plot(lm2.kobe.lasso)
coef(lm2.kobe.lasso, s = "lambda.min")
coef(lm2.kobe.lasso, s = "lambda.1se")

lm2_predictions_lasso <-  predict(lm2.kobe.lasso, s = lm2.kobe.lasso$lambda.min,
                                      newx = kobe.lm2.matrix)
#Check assumption
lasso_output <- data.frame(fit = c(lm2_predictions_lasso),
                          res = c(lm2.training$goal -
                                    lm2_predictions_lasso))
#linearity
ggplot(data = lasso_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
#qq plot
qqnorm(lasso_output$res)
qqline(lasso_output$res)
#model evaluation
kobe.lm2.matrix.testing <- model.matrix(goal~playoffs+opponent+age+home+X2PT.times+X3PT.times+
                                          poly(X2.PTgoalrate,degree = 3),
                               data = lm2.testing)[, -1]

#How model well in training
rmse(actual = lm2.training$goal, predicted = predict(kobe.lm2))
rmse(actual = lm2.training$goal, predicted = lm2_predictions_lasso)
#How model well in testing
rmse(actual = lm2.testing$goal, predicted = predict(kobe.lm2,newdata = lm2.testing))
rmse(actual = lm2.testing$goal, predicted = predict(lm2.kobe.lasso, newx = kobe.lm2.matrix.testing,
                                                    s = lm2.kobe.lasso$lambda.min))


##Ridge regression
lm2.kobe.ridge <- cv.glmnet(x = kobe.lm2.matrix, y = lm2.training$goal, alpha = 0,
                            nfolds = 5)
plot(lm2.kobe.ridge)
coef(lm2.kobe.ridge, s = "lambda.min")
coef(lm2.kobe.ridge, s = "lambda.1se")
lm2_predictions_ridge <-  predict(lm2.kobe.ridge, s = lm2.kobe.ridge$lambda.min,
                                  newx = kobe.lm2.matrix)
ridge_output <- data.frame(fit = c(lm2_predictions_ridge),
                           res = c(lm2.training$goal -
                                     lm2_predictions_ridge))
#Check assumption
ggplot(data = ridge_output, aes(x = fit, y = res)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept = 0)
qqnorm(ridge_output$res)
qqline(ridge_output$res)

#How model well in training
rmse(actual = lm2.training$goal, predicted = predict(kobe.lm2))
rmse(actual = lm2.training$goal, predicted = lm2_predictions_lasso)
rmse(actual = lm2.training$goal, predicted = lm2_predictions_ridge)
#How model well in testing
rmse(actual = lm2.testing$goal, predicted = predict(kobe.lm2,newdata = lm2.testing))
rmse(actual = lm2.testing$goal, predicted = predict(lm2.kobe.lasso, newx = kobe.lm2.matrix.testing,
                                                    s = lm2.kobe.lasso$lambda.1se))
rmse(actual = lm2.testing$goal, predicted = predict(lm2.kobe.lasso, newx = kobe.lm2.matrix.testing,
                                                    s = lm2.kobe.ridge$lambda.1se))

