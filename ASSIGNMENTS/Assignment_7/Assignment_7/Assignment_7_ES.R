library(tidyverse)
library(dplyr)
library(janitor)

#reading in the data
data <- read.csv("./Utah_Religions_by_County.csv")

#cleaning up the names
data <- clean_names(data)

#pivoting data longer
data_longer <- data %>%
  pivot_longer(cols = -c("county", "pop_2010", "religious", "non_religious"), 
               names_to = "church", 
               values_to = "percent") 


#plotting the data to answer the questions

data_longer %>%
  ggplot(aes(x = religious, y = non_religious)) +
  geom_point() 
#There is a negative correlation between the percent religious and the percent
#nonreligious. Which makes sense because as you become non religious, the amount 
#of religious people goes down

data_longer %>%
  ggplot(aes(x = pop_2010, y = religious)) +
  geom_point()
#There is no correlation between the amount of religious people and the population. 
#based on the graph it's more variable. 

data %>%
  ggplot(aes(x = pop_2010, y = lds, color = county)) +
  geom_point()
#I figured that since it was Utah, we might see some correlation between the population
#and the LDS religion, but there wasn't any correlation either. 

#So in conclusion the only correlation that really occurs, is when we look at the 
#percent of people that are religious and the percent of people that are non-religious