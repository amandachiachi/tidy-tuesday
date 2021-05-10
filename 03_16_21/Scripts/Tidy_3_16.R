###Tidy Tuesday for March 16th 2021####
# 

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(lubridate)

here()
#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-03-16') # load in the data 
game<-tuesdata$games

view(game)
game_clean <- game[complete.cases(game),] # remove any NAs 

game_final <- game_clean%>% 
  select(year, month, avg)%>% # select which columns I want to use
  group_by(year, month)%>% #group by year and month to sort
  summarise(avg) # add up the average amount of players

### Plot the data ####
# I want to plot a graph with year on the x axis, amount of each water source on the y and faceted by country
game_final%>%
  ggplot(aes(x = year, # year is on my x axis 
             y = avg))+ # the total sum of the ratings per year is on my y axis 
  geom_boxplot()+ # make it a line graph
  xlim(NA, 2020) + # only show information up to 2020 (2021 will skew data because we are only in the beginning)
  facet_wrap(~month)+
  labs(title = "Average Number of Videogame Players Per Month", # Label the title
       x = "Year", # Label the x axis 
       y = "Average Number of VideoGame Players", # label Y axis 
       caption = "data from rfordatascience/tidytuesday")+ # add a caption at the bottom
  theme_light()+ # change theme, remove background color etc.
  theme(axis.title = element_text(size = 13))+ # make the text size 13
  ggsave(here("03_16_21", "Output", "videogame_players.png"), width = 7 , height = 5) # save the plot

