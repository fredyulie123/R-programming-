
library(tidyverse)
library(ggplot2)
library(GGally)
library(broom)
library(car)
library(GGally)
library(mlbench)
library(MASS)
library(tidyverse)
#Q1
data("mpg")
x<-gather(hwy,cty,data=mpg,key=location,value=mpg)
x
ggplot(data=x,aes(x=location,y=mpg))+
  geom_boxplot()

# Q2
ggplot(data=x,aes(x=location,y=mpg))+
  geom_boxplot(fill="skyblue")+
  labs(x ="location",y ="mpg (m/g)",title ="the mpg situation for different location")
    
#Q3
average<-x%>%
  group_by(manufacturer,location)%>%
  summarise(average=median(mpg))
ggplot(data=average, aes(x=manufacturer,y=average,fill=location))+
  geom_col(position = "dodge")+
  scale_fill_manual(values=c("lightcoral","purple"))

ggplot(data=x,aes(x=mpg,col=location))+
  geom_density(position="stack" )
ggplot(data=x,aes(x=mpg,col=location))+
  geom_density(position="fill" )
ggplot(data=x,aes(x=mpg,col=location))+
  geom_density( )
?geom_density

#Q4
mpg%>%
  count(mpg$drv)
quantile(mpg$hwy,probs=c(0,.25,.50,.75,1))
quantile(mpg$hwy, probs=c(0,.33,.66,.90,1))
quantile(mpg$cty,probs=c(0,.25,.50,.75,1))
mean(mpg$cty)
median(mpg$cty)

# Q5
str(mpg)
cor(mpg$hwy,mpg$cty)

cor(mpg$hwy,mpg$displ)

cov(mpg$hwy,mpg$cyl)
cor(mpg$hwy,mpg$cyl)
ggplot(data=mpg,aes(x=fl,y=hwy))+
  geom_boxplot(fill="skyblue")

# Q6
mpg.lm1 <- lm(hwy ~ displ+drv+class, data = mpg)
summary(mpg.lm1)
plot(mpg.lm1)

#Q7
qqnorm(mpg.lm1$residuals)
qqline(mpg.lm1$residuals)
mpg.lm1 <- lm(hwy ~ displ+drv+class, data = mpg)
par(mfrow=c(2,2))
plot(mpg.lm1)
scatter.smooth(mpg.lm1$fitted.values, mpg.lm1$residuals)
anova(mpg.lm1)
vif(mod=mpg.lm1)
?vif
#Q8
library(MASS)
data("geyser")
str(geyser)
cor(geyser$duration,geyser$waiting)

#Q9
lm.gey<-lm(duration ~ waiting,data=geyser)
summary(lm.gey)
anova(lm.gey)


#Q10 
lm.gepl<-lm(duration ~ poly(waiting,degree=3),data=geyser)
summary(lm.gepl)
plot(lm.gepl)
anova(lm.gey,lm.gepl)
geyser%>%
  ggplot(aes(x=waiting,y=duration))+
  geom_point()+
  geom_smooth(se=F,color="red")+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="lm",se=F,formula=y~poly(x,3),color="green")
