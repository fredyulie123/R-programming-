###########################################
####     R Statistical Programming     ####
####      Lesson 2: Data Objects       ####
###########################################


#### Data Types ####

# Use class() to identify data type
class(100)
class("Jake")
class(factor(c("second", "first"), levels = c("first", "second")))
class(FALSE)

# Factors can be a bit confusing
# They have labels and levels
test_factor <- factor(c("second", "first", "first", "second"), 
                      levels = c("first", "second"))
test_factor
# The levels provide a specific order
sort(test_factor)

# We can change between data types using the as.x functions
# Changing from numeric to character
test_character <- as.character(100)
class(test_character)
# Changing from character to numeric
test_number <- as.numeric(test_character)
class(test_number)
# Going from factor directly to numeric bases the change on the levels
test_factor <- factor(1:3, levels = c(2, 3, 1))
test_factor
as.numeric(test_factor)
# If we want to base it on the labels, we have to use as.character first, then
as.numeric(as.character(test_factor))


#### Data Objects ####

# Vectors hold one data type in one dimension
test_vector <- c(1:10)
test_vector

# Matrices hold one data type in two dimensions
# We have to specify the number of rows and columns in a matrix
test_matrix <- matrix(data = 1:9, nrow = 3, ncol = 3)
test_matrix
t(test_matrix)

# Lists hold multiple data types
test_list <- list(name = c("Jake", "Hannah", "Jim"), 
                  age = c(25, 26, 33))
test_list

# Data frames hold multiple data types in two dimensions
test_df <- data.frame(name = c("Jake", "Hannah", "Jim"), 
                      age = c(25, 26, 33))
test_df
# In console input view(test_df) will have a table view
# We can use brackets to index specific parts of a data object
test_vector[3]
test_list[1]
# Multi-dimensional objects need to be indexed by row and columns
test_df[1, ]
test_matrix[, 3]
test_df[1:2,]
# We can index named columns or elements using $
test_df$name
## test_list[1] is different with test_list[[1]],because first one means first row with explan, 
## for second one, means special list row[1]

#### Functions ####

# Functions are created using function()
# We specify argument names inside function()
test_function <- function(input) {
  #Whatever is inside the {} is what the function does
  a<-input + 1
  
  b<-a*2
  return(a)
}

test_function(input = 1)


#### Packages ####

# We can load up packages using library()
library(MASS)
# We install packages with install.packages()
install.packages("tidyverse")
# We need to install a package once, but load it up every new session
library(tidyverse)
install.packages("stringr")
install.packages("forcats")
install.packages("ggplot2")

test_function<-function(input){a<-input+1
   a*2
   }
test_function(1)
library(tidyverse)
library(tidyselect)
mpg
ggplot()

