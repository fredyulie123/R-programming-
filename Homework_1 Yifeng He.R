# week 2
# Q4
hw_factor<-factor(c(2,1,3,5),level=c(1,2,3,5))
hw_factor
as.numeric(hw_factor)
?as.numeric
# Q5
hw_logical<-"TRUE"
is.logical(hw_logical)
as.logical(hw_logical)
hw_logical<-TRUE
is.logical(hw_logical)
# Q6
test_function<-function(x,y){
  test_list<-list(a=x+6,b=y*2)
  return(test_list)
}
# week 3
# Q8
library(tidyverse)
install.iris<-iris %>%
  select(Petal.Length,Petal.Width,Species)%>%
  group_by(Species) %>%
  summarize(avg.length=mean(Petal.Length, na.rm=T),avg.width=mean(Petal.Width, na.rm=T))
install.iris
avg_petal<-install.iris
avg_petal
# Q9
long.tb <- gather(avg.length, avg.width, data = avg_petal, key = description, value = number)
long.tb
long.tb%>%
  filter(number>=2)

