install.packages("broom")
library(broom)
library(tidyverse)
prestige<-read_csv(file="~/prestige.csv")
str(prestige)
summary(prestige)
prestige%>%
  ggplot(aes(x=prestige))+
  geom_density(fill="lightcoral")+
  labs(title="distribution of prestige")
prestige %>%
  ggplot(aes(x=education,y=prestige))+
  geom_point(size=2)+
  labs(title="Relationship Between Education and Prestige")
pres.lm1<-lm(prestige~education,data=prestige)
summary(pres.lm1)
tidy(pres.lm1)
qqnorm(pres.lm1$residuals)
qqline(pres.lm1$residuals)
