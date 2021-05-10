# Tidy Tuesday 05_04_21
# Amanda Chiachi

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)

# Read in the Data
tuesdata <- tidytuesdayR::tt_load('2021-05-04')
water <- tuesdata$water # select the data sheet we want to look at

view(water)# view it

water_clean <- water[complete.cases(water),] # remove any NAs 

water_final <- water_clean%>%
  select(country_name, water_source, install_year)%>% # only keep the columns that we want to look at
  group_by(install_year, country_name)%>% # group by the year installed and the country name
  summarise(water_source) # summarise by water source to plot easier

view(water_final)

### Plot the data ####
# I want to plot a graph with year on the x axis, amount of each water source on the y and faceted by country
water_final%>%
  ggplot(aes(x = install_year, # year is on my x axis 
             y = water_source))+ # the total sum of the ratings per year is on my y axis 
  geom_boxplot()+ # make it a line graph
  xlim(NA, 2020) + # only show information up to 2020 (2021 will skew data because we are only in the beginning)
  facet_wrap(~country_name)+
  labs(title = "Number of Each Type of Water Source by Year)", # Label the title
       x = "Year", # Label the x axis 
       y = "Type of Water Source", # label Y axis 
       caption = "data from rfordatascience/tidytuesday")+ # add a caption at the bottom
  theme_light()+ # change theme, remove background color etc.
  theme(axis.title = element_text(size = 13))+ # make the text size 13
  ggsave(here("05_04_21", "Output", "Water_source.png"), width = 7 , height = 5) # save the plot

