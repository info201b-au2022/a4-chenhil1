library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

data <- read.csv("../../../data/incarceration_trends.csv")
View(data)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# 'poc' dataframe with a column summing the poc 15-to-64 population for each row and a column calculating the ratio of poc to white prisoners for each row
poc <- data %>% 
  replace(is.na(.), 0) %>% 
  mutate(poc_pop_15to64 = rowSums(select(., aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64))) %>% 
  mutate(poc_white_ratio_15to64 = poc_pop_15to64 / white_pop_15to64)
View(poc)

# Find county, state, year with the highest ratio of poc to white prisoners
# Find ratio
max_ratio <- poc %>% 
  select(poc_white_ratio_15to64) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio_15to64 == max(poc_white_ratio_15to64)) %>% 
  pull(poc_white_ratio_15to64)

# Find county
county_max_ratio <- poc %>% 
  select(county_name, poc_white_ratio_15to64) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio_15to64 == max(poc_white_ratio_15to64)) %>% 
  pull(county_name)

# Find state
state_max_ratio <- poc %>% 
  select(state, poc_white_ratio_15to64) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio_15to64 == max(poc_white_ratio_15to64)) %>% 
  pull(state)

# Find year
year_max_ratio <- poc %>% 
  select(year, poc_white_ratio_15to64) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio_15to64 == max(poc_white_ratio_15to64)) %>% 
  pull(year)

# same as 'poc' dataframe, with NAs included
poc_na <- data %>% 
  mutate(poc_pop_15to64 = rowSums(select(., aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64))) %>% 
  mutate(poc_white_ratio_15to64 = poc_pop_15to64 / white_pop_15to64)


# Find county, state with the lowest ratio of poc to white prisoners
# Find ratio
min_ratio <- poc_na %>% 
  select(poc_white_ratio_15to64) %>% 
  drop_na() %>% 
  filter(poc_white_ratio_15to64 != 0) %>% 
  filter(poc_white_ratio_15to64 == min(poc_white_ratio_15to64)) %>% 
  pull(poc_white_ratio_15to64)

# Find county
county_min_ratio <- poc_na %>% 
  select(county_name, poc_white_ratio_15to64) %>% 
  drop_na() %>% 
  filter(poc_white_ratio_15to64 != 0) %>% 
  filter(poc_white_ratio_15to64 == min(poc_white_ratio_15to64)) %>% 
  pull(county_name)

# Find state
state_min_ratio <- poc_na %>% 
  select(state, poc_white_ratio_15to64) %>% 
  drop_na() %>% 
  filter(poc_white_ratio_15to64 != 0) %>% 
  filter(poc_white_ratio_15to64 == min(poc_white_ratio_15to64)) %>% 
  pull(state)

# Find year
year_min_ratio <- poc_na %>% 
  select(year, poc_white_ratio_15to64) %>% 
  drop_na() %>% 
  filter(poc_white_ratio_15to64 != 0) %>% 
  filter(poc_white_ratio_15to64 == min(poc_white_ratio_15to64)) %>% 
  pull(year)

# Find avg ratio of poc to white prisoners in King County, WA from 1970-2018
avg_ratio_KC <- poc %>% 
  select(county_name, state, poc_white_ratio_15to64) %>% 
  filter(county_name == "King County", state == "WA") %>% 
  replace(is.na(.), 0) %>% 
  summarise(poc_white_ratio_15to64 = sum(poc_white_ratio_15to64) / 49) %>% 
  pull(poc_white_ratio_15to64)

# Store info in a list for Rmd access
summary_data <- list()
summary_data$max <- max_ratio
summary_data$max_county <- county_max_ratio
summary_data$max_state <- state_max_ratio
summary_data$max_year <- year_max_ratio
summary_data$min <- min_ratio
summary_data$min_county <- county_min_ratio
summary_data$min_state <- state_min_ratio
summary_data$min_year <- year_min_ratio
summary_data$avg_KC <- avg_ratio_KC


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


