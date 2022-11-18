library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

data <- read.csv("../../../data/incarceration_trends.csv")
View(data)

## Section 2  ---- 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# 'poc' dataframe with a column summing the poc population for each row and a column calculating the ratio of poc to white prisoners for each row
poc <- data %>% 
  replace(is.na(.), 0) %>% 
  mutate(poc_prison_pop = rowSums(select(., aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop))) %>% 
  mutate(poc_white_ratio = poc_prison_pop / white_prison_pop)
View(poc)

# Find county, state, year with the highest ratio of poc to white prisoners
# Find ratio
max_ratio <- poc %>% 
  select(poc_white_ratio) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio != Inf) %>% 
  filter(poc_white_ratio == max(poc_white_ratio)) %>% 
  pull(poc_white_ratio)

# Find county
county_max_ratio <- poc %>% 
  select(county_name, poc_white_ratio) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio != Inf) %>% 
  filter(poc_white_ratio == max(poc_white_ratio)) %>% 
  pull(county_name)

# Find state
state_max_ratio <- poc %>% 
  select(state, poc_white_ratio) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio != Inf) %>% 
  filter(poc_white_ratio == max(poc_white_ratio)) %>% 
  pull(state)

# Find year
year_max_ratio <- poc %>% 
  select(year, poc_white_ratio) %>% 
  replace(is.na(.), 0) %>% 
  filter(poc_white_ratio != Inf) %>% 
  filter(poc_white_ratio == max(poc_white_ratio)) %>% 
  pull(year)

# same as 'poc' dataframe, with NAs included
poc_na <- data %>% 
  mutate(poc_prison_pop = rowSums(select(., aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop))) %>% 
  mutate(poc_white_ratio = poc_prison_pop / white_prison_pop)


# Find county, state with the lowest ratio of poc to white prisoners
# Find ratio
min_ratio <- poc_na %>% 
  select(poc_white_ratio) %>% 
  drop_na() %>% 
  filter(poc_white_ratio != 0) %>% 
  filter(poc_white_ratio == min(poc_white_ratio)) %>% 
  pull(poc_white_ratio)

# Find county
county_min_ratio <- poc_na %>% 
  select(county_name, poc_white_ratio) %>% 
  drop_na() %>% 
  filter(poc_white_ratio != 0) %>% 
  filter(poc_white_ratio == min(poc_white_ratio)) %>% 
  pull(county_name)

# Find state
state_min_ratio <- poc_na %>% 
  select(state, poc_white_ratio) %>% 
  drop_na() %>% 
  filter(poc_white_ratio != 0) %>% 
  filter(poc_white_ratio == min(poc_white_ratio)) %>% 
  pull(state)

# Find year
year_min_ratio <- poc_na %>% 
  select(year, poc_white_ratio) %>% 
  drop_na() %>% 
  filter(poc_white_ratio != 0) %>% 
  filter(poc_white_ratio == min(poc_white_ratio)) %>% 
  pull(year)

# Find avg ratio of poc to white prisoners in King County, WA from 1970-2018
avg_ratio_KC <- poc %>% 
  select(county_name, state, poc_white_ratio) %>% 
  filter(county_name == "King County", state == "WA") %>% 
  replace(is.na(.), 0) %>% 
  summarise(poc_white_ratio = sum(poc_white_ratio) / 49) %>% 
  pull(poc_white_ratio)

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
#----------------------------------------------------------------------------#

# This function gets a dataframe of the US total jail population by year
get_year_jail_pop <- function() {
  us_jail_pop <- data %>% 
    replace(is.na(.), 0) %>% 
    select(total_jail_pop, year) %>%
    group_by(year) %>% 
    summarise(sum(total_jail_pop)) %>% 
    rename("total_jail_pop_sum" = "sum(total_jail_pop)")
return(us_jail_pop)   
}

# This function plots and formats a bar chart of the dataframe of the total US jail population by year
plot_jail_pop_for_us <- function()  {
  p <- ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop_sum)) +
    geom_bar(stat = "identity") + 
    scale_y_continuous(
      breaks = c(0, 200000, 400000, 600000, 800000),
      labels = c("0", "200,000", "400,000", "600,000", "800,000")
    ) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Prison Population in the U.S. (1970-2018)",
      caption = "Bar chart showing a significant increase in the U.S. prison population, 1970 to 2018."
    )
  return(p)   
} 

# Find plot min population value
plot_summary_min <- get_year_jail_pop() %>% 
  summarise(min_pop = min(total_jail_pop_sum)) %>% 
  pull(min_pop)

# Find year of plot min population value
plot_summary_min_year <- get_year_jail_pop() %>% 
  filter(total_jail_pop_sum == plot_summary_min) %>% 
  pull(year)

# Find plot max population value
plot_summary_max <- get_year_jail_pop() %>% 
  summarise(max_pop = max(total_jail_pop_sum)) %>% 
  pull(max_pop)

# Find year of plot max population value
plot_summary_max_year <- get_year_jail_pop() %>% 
  filter(total_jail_pop_sum == plot_summary_max) %>% 
  pull(year)

# Store plot summary data in list for Rmd access
plot_summary_data <- list()
plot_summary_data$min_plot <- plot_summary_min
plot_summary_data$max_plot <- plot_summary_max
plot_summary_data$year_min <- plot_summary_min_year
plot_summary_data$year_max <- plot_summary_max_year

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# Get dataframe of total jail populations by year in the specified states
get_jail_pop_by_states <- function(states){
  states_jail_pop <- data %>% 
    replace(is.na(.), 0) %>% 
    select(year, state, total_jail_pop) %>%
    filter(state %in% states) %>%
    group_by(year, state) %>% 
    summarise(sum(total_jail_pop)) %>% 
    rename("state_total_jail_pop" = "sum(total_jail_pop)") 
  return(states_jail_pop)
}

# Plot and format a line chart of total jail populations by year in the specified states
plot_jail_pop_by_states <- function(states){
  p <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = state_total_jail_pop, group = state, color = state)) + 
  geom_line() +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Growth of Jail Population by U.S. State (1970-2018)",
      caption = "Line chart showing changes in various U.S. state jail populations, 1970 to 2018."
    )
  return(p)
}


# Find state, year, value of minimum prison population
state_summary_min <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == min(state_total_jail_pop)) %>% 
  pull(state_total_jail_pop)

state_summary_min_state <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == min(state_total_jail_pop)) %>% 
  pull(state)

state_summary_min_year <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == min(state_total_jail_pop)) %>% 
  pull(year)

# Find state, year, value of maximum prison population
state_summary_max <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == max(state_total_jail_pop)) %>% 
  pull(state_total_jail_pop)

state_summary_max_state <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == max(state_total_jail_pop)) %>% 
  pull(state)

state_summary_max_year <- get_jail_pop_by_states(c("WA", "OR", "CA", "VA", "NY")) %>% 
  ungroup() %>% 
  filter(state_total_jail_pop == max(state_total_jail_pop)) %>% 
  pull(year)

# Store state summary data in list for Rmd access
state_summary_data <- list()
state_summary_data$min <- state_summary_min
state_summary_data$min_state <- state_summary_min_state
state_summary_min_state$min_year <- state_summary_min_year
state_summary_data$max <- state_summary_max
state_summary_data$max_state <- state_summary_max_state
state_summary_min_state$max_year <- state_summary_max_year

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#

# Get dataframe of total POC population and proportion POC population imprisoned in WA
poc_pop_data <- function() {
  total <- poc %>%
    mutate(poc_total_pop = rowSums(select(., aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64))) %>%
    filter(state == "WA") %>%
    group_by(year) %>%
    summarise(sum(poc_total_pop)) %>%
    rename("poc_total" = "sum(poc_total_pop)")

  prop <- poc %>%
    mutate(poc_total_pop = rowSums(select(., aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64))) %>%
    mutate(poc_prop = poc_prison_pop / poc_total_pop) %>%
    replace(is.na(.), 0) %>%
    filter(state == "WA") %>%
    group_by(year) %>%
    summarise(sum(poc_prop)) %>%
    rename("poc_prop" = "sum(poc_prop)")

  combine <- left_join(total, prop, by = "year")
  is.na(combine) <- sapply(combine, is.infinite)
  combine <- combine %>% replace(is.na(.), 0)

  return(combine)
}

white_pop_data <- function(){
  white_prop <- poc %>% 
    mutate(white_prop = white_prison_pop / white_pop_15to64) %>% 
    replace(is.na(.), 0) %>% 
    filter(state == "WA") %>% 
    group_by(year) %>% 
    summarise(sum(white_prop)) %>% 
    rename("white_prop" = "sum(white_prop)")
  
  white_total <- poc %>% 
    filter(state == "WA") %>% 
    group_by(year) %>% 
    summarise(sum(white_pop_15to64)) %>% 
    rename("white_pop" = "sum(white_pop_15to64)")
  
  combine <- left_join(white_prop, white_total, by = "year")
  is.na(combine) <- sapply(combine, is.infinite)
  combine <- combine %>% replace(is.na(.), 0)
  
  return(combine)
}

# Plot and format scatterplot of relationship between total POC population and proportion POC population imprisoned in WA
plot_poc_data <- function(){
  p <- ggplot(poc_pop_data(), aes(x = poc_total, y = poc_prop)) +
    geom_point() + 
    labs(
      x = "Total POC Population",
      y = "Proportion of POC Population Imprisoned",
      title = "Imprisoned POC Proportion vs. Total POC Population in Washington",
      caption = "Relationship between POC population size and proportion of POC population imprisoned, using data from 1970-2018."
    )
  return(p)
}

plot_white_data <- function() {
  p <- ggplot(white_pop_data(), aes(x = white_pop, y = white_prop)) +
    geom_point() + 
    scale_x_continuous(
      breaks = c(0, 1000000, 2000000, 3000000),
      labels = c("0", "1,000,000", "2,000,000", "3,000,000")
    ) +
    labs(
      x = "Total White Population",
      y = "Proportion of White Population Imprisoned",
      title = "Imprisoned White Proportion vs. Total White Population in Washington",
      caption = "Relationship between White population size and proportion of White population imprisoned, using data from 1970-2018."
    )
  return(p)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 
