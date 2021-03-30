####################################
##                                ##
##   SDGB-7844 - Lecture 3 Code   ##
##   Loops, Summary Stats         ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################

#########################
# Workspace & R Files   #
#########################

# listing every object in your workspace
ls()
# removing items in your workspace
rm()
# clearing your workspace - deletes all
rm(list=ls())
# saving every object in your workspace
save.image()
# file_name.RData
# loading a saved workspace
load()

# save code as a ".R" file
# R is case sensitive, remember this!
# Use to upload the code in the entire file in to the R console
source("FILE PATH.R")

# save/load history of commands entered into console
savehistory()
loadhistory()

# determine/change directory R is pointing to using
# can also do this manually in RStudio
getwd()
setwd()


#######################
# Creating Vectors    #
#######################

# c() function combines values
A <- c(13, 5, 3, 11)

# scalar
B <- 3

# extract third element
# [ ] brackets are used to create subsets
# we will cover subsets in detail in future lectures
A[3]

# extract 1st & 2nd element
A[c(1, 2)]

# extract 3rd element
i <- 3
A[i]


# other ways to construct vectors:

# : is used by r as a "from : to"
# this is useful in selecting rows or columns
1:25
x <- 1:25

# seq( ) 
# the sequence function has three key arguments
# from, to, by
# length is also an argument
seq(from = 1, to=20, by=2)

# length of a vector
length(x) 

# sort a vector in ascending order
sort(x, decreasing=FALSE)

# order will give the index numbers
# for the ordered values of the vector x
unordered_vector <- c(10, 14, 2, 21, 4, 7, 6, 11)
order(unordered_vector, decreasing=FALSE)


#######################
# Data Frames & Lists
#######################

# data frame - can combine variable types in columns
y <- data.frame("a"=c(1,2,3),
                "b"=c("words","are","here"),
                "c"=c(TRUE,TRUE,FALSE))
colnames(y) #will return column names
rownames(y) #will return row names
str(y) #will give a summary of object data types

# Extracting elements from Data Frames & Lists

# data frames
# 4 ways to extract column (vector) "a"
y$a
y[,1]
y[,"a"]
y[["a"]]


##################################
# how does a loop work ?         #
##################################

#for loop example

# vector represents a shopping cart
# imagine pulling each item out of 
# the cart in order to pay for them
# we will use the print() function
shopping_cart <- c("apple", "banana", "mango", "chocolate",
                   "cereal", "cake", "yogurt", "ice cream")


# the for() funtion is of the form:
# for(variable in range_of_values)
# generally this looks like 1:number_you_want_to_stop_at

# followed by a set of braces for(variable in range_of_values){   }
# the code in the braces is evaluated once for each value in our range_of_values
# our "variable" will take on the values 1, 2, 3, 4, 5, 6, 7, 8

# for(item in 1:8) ~~ for(item in 1:length(shopping_cart))

# item is our variable
# will take on the values 1:8
for(item in 1:8){
  
  # print value of item
  print( item )
  
  # loop will execute shopping_cart[1], shopping_cart[2]...
  print( shopping_cart[item] )
  
}

# item is our variable
# will take on the values 1:8
for(item in 1:length(shopping_cart)){
  
  # print value of item
  print( item )
  
 # loop will execute shopping_cart[1], shopping_cart[2]...
 print( shopping_cart[item] )
  
}

# seq_along() an alternative function
# less typing, might be more intuitive
# use what you feel comfortable with
seq_along(shopping_cart) #1:8

for(item in seq_along(shopping_cart)){
  
  print( item )
  
  print ( shopping_cart[item] )
  
  
}

##################################
# computing the mean (version 1) #
##################################

x <- c(5, 2, 6, 8, 4, 2)

# average the numbers in vector x

n <- length(x)
x_sum <- 0

for(i in 1:n){ # loop repeated 6 times
  
  # compute cumulative sum
  x_sum <- x_sum + x[i]
  
  print(i) # print index value in each iteration (for debugging)
  
} # end for loop

# compute average
x_sum/n

# verify with R function
mean(x)


############################
#  logical operators table #
############################

x <- 5
y <- c(7,4,2)
z <- c("a","b","c")

!(x > 4)
x == 4
z == "c"
x != 4
(x>4) & (x<=10)
(x>4) | (x==6)
any(y==2)
all(x==3)
is.element(x,y)
x%in%y


############################
#  while loops             #
############################


# the while() funtion is of the form:
# while(condition is TRUE)
# it will stop when your condition returns false
# you can think of this as each time the code within the brackets is evaluated
# R will check to see if your condition is still returning TRUE

# followed by a set of braces while(condition is TRUE){}
# the code in the braces is evaluated over and over until 
# the condition following while() returns a FALSE value
# our "variable" will take on the values 1:10 in the example below

# let's start our n at 0
n <- 0

while(n < 10){ # our condition starting value is 1
  
  # each code iteration
  # increase n by 1
  n <- n+1
  
  # is the condition still TRUE?
  print(n<10)
  
}


##################################
# computing the mean (version 2) #
##################################

x <- c(5, 2, 6, 8, 4, 2)

# average the numbers in vector x using a while() loop
sum.x <- 0
index <- 1 # set index
n <- length(x)
while(index <= n){ # loop repeated as long as index
  # is less than or equal to 6
  
  # compute cumulative sum
  sum.x <- sum.x + x[index]
  
  print(index)   # print index value (for debugging)
  
  # increase index
  index <- index+1
  
} # end while loop

sum.x/n    # compute average
mean(x)    # verify with R function


##################################
# examples of if/else statements #
##################################

# Try the following:

x <- 145

if(x/3 > 10){
  print("Yes!")
}else{
  print("No")
} # end if/else

# ifelse() function can sometimes replace loops or if statements
# ifelse() is very fast iterating over values

# ifelse(logical_statement, value_if_yes, value_if_no)
x <- seq(from = 1, to = 100, by = 1)
ifelse(x/3 > 10, "Yes!", "No")


##############################
# measures of center: median #
##############################

x <- c(1, 3, 5, 2, 7)
n <- length(x)

if(n%%2==0){
  # n is even
  median.x <- sum(sort(x, decreasing=FALSE)[ (n/2):(n/2+1) ] ) /2
}else{
  # n is odd
  median.x <- sort(x, decreasing=FALSE)[ceiling(n/2)]
} # end if/else
median.x

# verify with R functions
median(x)
quantile(x, probs=0.5)



########################################################
# Data: Old Faithful Geyser, Yellowstone National Park #
########################################################

# show first/last 6 rows      
head(faithful)
tail(faithful)

# number of rows in data frame
nrow(faithful)   	

# column names
colnames(faithful)


# help() will automatically populate information (if available)
help(faithful)


###################
# mean and median #
###################

mean(faithful$eruptions)        # mean of each variable
mean(faithful$waiting)

median(faithful$eruptions)      # median of each variable
median(faithful$waiting)

# Does our code give us the same answer?

summary(faithful)     # 6 number summary
# min, Q1, median (Q2), mean, Q3, max 

# what do these do?
summary(faithful$eruptions)  
summary(faithful[,2])
summary(faithful[,"eruptions"])


###################################
# variance and standard deviation #
###################################

x <- faithful$eruptions
n <- length(x)

sum_of_sq <- 0
for(i in 1:n){
  sum_of_sq <- sum_of_sq + (x[i]-mean(x))^2
} # end for loop
sum_of_sq/(n-1)            # variance
sqrt(sum_of_sq/(n-1))      # standard deviation

# shorter version:
sum((x-mean(x))^2)/(length(x)-1)          # variance
sqrt(sum((x-mean(x))^2)/(length(x)-1))    # standard deviation

# shortest:
var(x)        # variance
sd(x)         # standard deviation

