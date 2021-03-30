#########################################
####### R Statistical Programming #######
####  Lesson 12: Loops and Strings  #####
#########################################

library(tidyverse)
data(mtcars)

# Applying R functions to a column of data is easy and can be done quickly
mean(mtcars$mpg, na.rm = T)
# What about multiple columns, though?
mean(mtcars[, c(1, 2)], na.rm = T)

#### For Loops ####

# We need to loop over our dataset and apply our function to each element
# We begin by specifying the for argument
for(i in 1:10){
  # Everything inside the curly braces is the function we are applying to each
  # value of i
  print(5 * i)
}

# Let's find the mean of columns 1 through 5 of mtcars
for(i in 1:5){
  print(mean(mtcars[, i], na.rm = T))
}

#### The Apply Family ####

# Apply family is a bit more straightforward in its syntax

# apply takes what you want to loop over, and applies a function either
# by row or by column
apply(mtcars[, c(1:5)], 2, function(x) mean(x, na.rm = T))
# It can change the dimension of where we apply the function
apply(mtcars[, c(1:5)], 1, function(x) sum(x, na.rm = T))

# lapply and sapply are mainly used to edit multiple elements of a list
mtcars.list <- list(mpg = mtcars$mpg, cyl = mtcars$cyl, disp = mtcars$disp)
mtcars.list
# We can go over each of these elements and perform a function
lapply(mtcars.list, function(x) x / 2)
# sapply does the same thing, but returns the output in a simplified format
sapply(mtcars.list, function(x) x / 2)


#### Editing in dplyr ####

# We can also edit multiple columns using mutate_at or mutate_all
# Let's say we wanted to change every column to a character
mtcars %>%
  mutate_all(.funs = list(~ as.character(.))) %>%
  str()
# We could also use mutate_at to change specific variables
mtcars %>%
  mutate_at(.vars = vars(mpg:drat), .funs = list(~ as.character(.))) %>%
  str()


#### purrr ####

# purrr is a tidyverse package that works similar to lapply and sapply
# The map function produces a list output
mtcars %>%
  map(mean)
# map_dbl and map_chr returns a vector
mtcars %>%
  map_dbl(mean)
mtcars %>%
  map_chr(mean)
# map_dfr returns a dataframe
mtcars %>%
  map_dfr(mean)
