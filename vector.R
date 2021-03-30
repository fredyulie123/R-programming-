# Vector

x<-vector("character",length=10)

x1<-1:4

x2<-c(1,2,3,4)

x3<-c(TRUE,10,"a")

x4<-c("a","b","c")
as.numeric(x4)
as.logical()
as.character()

class(x1)
names(x1)<-c("a","b","c","d")
x1

#Matrix&Array
x<-matrix(1:6,nrow=3,ncol=2)
dim(x)
attributes(x)

y<-1:6
dim(y)<-c(2,3)

y2<-matrix(1:6,nrow=2,ncol=3)
y
rbind(y,y2)
cbind(y,y2)

x<-array(1:24,dim=c(4,6))
x
x1<-array(1:24,dim=c(2,3,4))
x1


#list
l<-list("a",2,10L,3+4i,TRUE)
l
l2<-list(a=1,b=2,c=3)
l2
l3<-list(c(1,2,3),c(4,5,6,7))
l3

x<-matrix(1:6,nrow=2,ncol=3)
dimnames(x)<-list(c("a","b"),c("c","d","e"))

#factor
x<-factor(c("female","female","male","male","female"))
y<-factor(c("female","female","male","male","female"),level=c("male","female"))
table(x)
unclass(x)
class(unclass(x))
class(x)

#missing value
x<-c(1,NA,2,NA,3)
is.na(x)
is.nan(x)

y<-c(1,NaN,2,NaN,3)
is.na(y)
is.nan(y)

#data frame

df<-data.frame(id=c(1,2,3,4),name=c("a","b","c","d"),gender=c(TRUE,TRUE,FALSE,FALSE))
df
5=x
