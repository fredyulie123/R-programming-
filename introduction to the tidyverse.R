#######################################################
####          R Statistical Programming           #####
####   Lesson 3: Introduction to the Tidyverse    #####
#######################################################


# Tidyverse contains several outside packages, so we need to install it first
install.packages("tidyverse")
# Bring up the package with library()
library(tidyverse)

# We'll be using a dataset documenting the number of TB cases documented by WHO 
# in several countries in 1999 and 2000
initial.tb<- 
  table4a %>%
  rename(Year1=`1999`, Year2=`2000`)
# We'll also be using the starwars dataset
initial.starwars<- starwars[, 1:10]
data("table4a")
rm(table4a)
view(initial.tb)
#### tidyr ####

# tidyr has a couple of functions developed to reshape data
# initial.tb is in wide-format; each time variable is a column
# We can convert from wide to long by gathering our data
long.tb <- gather(Year1, Year2, data = initial.tb, key = Year, value = Cases)
long.tb
# We can convert from long to wide by spreading our data
wide.tb<- spread(data = long.tb, key = Year, value = Cases)
wide.tb
spread(data=long.tb,key=country,value=Cases)
#### dplyr ####

# While tidyr was focused on data reshaping, dplyr is more about data 
# manipulation
# It makes heavy use of the pipe operator (%>%)
  # This pipe operator is used extensively across all tidyverse packages
  # It combines multiple lines of code into one code chunk

# We can select (or index) specific columns using select
initial.starwars %>%
  # Select height and birth year of all characters
  select(height, birth_year)
# Select still works
select(.data=initial.starwars,height)

head(starwars)
# We can filter a dataframe with filter  == means equal; != means not equal
initial.starwars %>%
  # Filter on only males
  filter(gender == "male")
initial.starwars.select<-initial.starwars%>%
  filter(gender=="female")
initial.starwars %>%
  .[,1:4]
initial.starwars %>%
  # Filter on only males
  filter(height>165)
# We can order rows in a specific order with arrange
initial.starwars %>%
  # Arrange by height
  arrange(height)
# To arrange by descending height, we have to specify desc()
initial.starwars %>%
  arrange(desc(height))

# We create new columns using mutate
new.starwars<-
  initial.starwars %>%
  # Creating a new column that is the square root of height
  mutate(sqrt_height = sqrt(height))

# We create new columns using mutate
new.starwars<-
  initial.starwars %>%
  # Creating a new column that is the square root of height
  mutate(sqrt_height = sqrt(height))

# We can rename columns using rename
initial.starwars %>%
  # Rename name to full_name
  rename(full_name = name)

# We can summarize info using summarize
initial.starwars %>%
  # Here we are finding the average height of all characters
  summarize(avg.height = mean(height, na.rm=T))
  mean(initial.starwars$height, na.rm=T)
mean()
# We can use the pipe operator to chain multiple functions together
# What if we want to select the mass of all characters taller than 200 cm in 
# desc order?
initial.starwars %>%
  # Filter characters taller than 200 cm
  filter(height > 200) %>%
  # Select the mass column
  select(mass) %>%
  # Order mass in descending order
  arrange(desc(mass))

# We can group functions by other variables using group_by
initial.starwars %>%
  # Grouping by gender
  group_by(gender) %>%
  # Finding average height
  summarize(avg.height = mean(height, na.rm=T)) %>%
  
  # Remember to ungroup if you want to perform non-grouped functions after 
  # grouping
  ungroup() %>%
  summarize(avg.height =mean (avg.height, na.rm=T))

