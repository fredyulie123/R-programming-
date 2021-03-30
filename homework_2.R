install.packages("GGally")
library(tidyverse)
library(GGally)
#1,2
data("mpg")
view(mpg)
#transfer data to wide fomat
long.tb <- gather(cty,hwy,data=mpg,key=location,value=mpg)
view(long.tb)
long.tb %>%
  ggplot(aes(x=location,y=mpg))+
  geom_boxplot(fill="skyblue")+
  labs(title = "MPG Relationships between city and highways",x = "Location",y = "Miles per gallon")+
  theme(axis.title.y = element_text(face="bold",size = 12),
        axis.title.x = element_text(face="bold",size = 12),
        axis.text.x = element_text(face = "bold",size = 12,angle = 45,hjust = 1),
        plot.title = element_text(face="bold",size =12,hjust = .5))
#3
#create a new table including median of mpg grouped by location
average<-long.tb%>%
  group_by(manufacturer,location)%>%
  summarize(ave=median(mpg))

#To see what is the relationships between manufacturers and their mpg
ggplot(data = average,aes(x=manufacturer,y=ave,fill=location))+
  geom_col(position = "dodge",width = 0.5)+
  geom_text(aes(label= ave), vjust=-0.2)+
  scale_fill_manual(values=c("green","blue"))+
  labs(title = "The average of every manufactuers' mpg",y="Average values")+
  theme(plot.title = element_text(face="bold",hjust = .5))

ggplot(data = long.tb,aes(x=mpg,col=location))+
  geom_density(position = "identity")
#4.1
mpg %>%
  count(drv)
#conclusion:In our data, we notice that most cars produced by 4 wheel and front wheel drive.
  ggplot(data=mpg,aes(x=manufacturer,fill=drv))+
  geom_bar(position = "stack")+
  scale_fill_manual(values = c("blue", "green", "red"))+
  labs(title = "drive wheel condition")
#4.2
  quantile(mpg$hwy)
  quantile(mpg$hwy,probs = c(.33,.66,.90,1))
#we notice that in cty may have extrem value that influent our mean
#4.3
  quantile(mpg$cty)
  quantile(mpg$cty,probs = c(.33,.66,.90,1))
  mean(mpg$cty)
  median(mpg$cty)
  
#Week6&7
#5
library(broom)
library(car)
str(mpg)
summary(mpg$hwy)
hist(mpg$hwy)
ggplot(data = mpg,aes(x=hwy))+
  geom_density()
cor(mpg$year,mpg$hwy)#drop
cor(mpg$displ,mpg$hwy)
cor(mpg$cty,mpg$hwy)
cor(mpg$cyl,mpg$hwy)

#summary(mpg)
ggplot(data = mpg,aes(x=cyl,y=hwy))+
  geom_point()#drop

ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point(size = 2)+
  geom_smooth(method = "lm",color="red",se=F)+
  labs(title = "relationships between displ and hwy")

ggplot(data = mpg,aes(x=cty,y=hwy))+
  geom_point(size=2)+
  geom_smooth(method = "lm",color="red",se=F)+
  labs(title = "relationships between cty and hwy")

#displ~hwy
mpg.lm1 <- lm(hwy~displ,data = mpg)
summary(mpg.lm1)
tidy(mpg.lm1)

#cty~hwy
mpg.lm2 <- lm(hwy~cty,data = mpg)
summary(mpg.lm2)
tidy(mpg.lm2)

par(mfrow=c(2,2))
plot(mpg.lm1)

#6
data(mpg)
ggpairs(mpg[,c("displ","drv","class","hwy")],lower = list(continuous = "smooth"))
mpg.lm3<-lm(hwy~displ+drv+class,data = mpg)
mpg.lm3
summary(mpg.lm3)
tidy(mpg.lm3)
#7
par(mfrow=c(2,2))
plot(mpg.lm3)
vif(mpg.lm3)

qqPlot(mpg.lm3)
crPlots(mpg.lm3)
ncvTest(mpg.lm3)
spreadLevelPlot(mpg.lm3)

#8
library(MASS)
data("geyser")
str(geyser)
summary(geyser)
hist(geyser$waiting)
hist(geyser$duration)
cor(geyser$waiting,geyser$duration)
ggplot(data = geyser,aes(x=waiting,y=duration))+
  geom_point()+
  labs(title = "Relationships between waiting and duration")
#9
geyser.lm1 <- lm(duration~waiting,data = geyser)
summary(geyser.lm1)
tidy(geyser.lm1)
par(mfrow = c(2,2))
plot(geyser.lm1)


#we find that residuals vs fitted plot is not symmetrical, so linear model may not the best.
#10
ggplot(data = geyser,aes(x=waiting,y=duration))+
  geom_point(size=2)+
  geom_smooth(se=F,color="red")+
  geom_smooth(method = "lm",se = F,color = "blue")+
  geom_smooth(method = "lm",se = F,formula = y~poly(x,3),color = "green")

geyser.lm2 <- lm(duration~poly(waiting,degree = 3),data = geyser)
summary(geyser.lm2)
par(mfrow = c(2,2))
plot(geyser.lm2)
anova(geyser.lm1,geyser.lm2)

crPlots(geyser.lm2)
ncvTest(geyser.lm2)
spreadLevelPlot(geyser.lm2)
