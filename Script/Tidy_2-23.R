###Tidy Tuesday for Feb 23 2021####
# The data this week comes from BLS
# Employed persons by industry, sex, race, and occupation 

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)

#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
employed <- tuesdata$employed

#Goal: industry on x axis and average number of people employed in each industry on the y axis
view(employed)

####Analyze Data####
employed_clean <- employed%>% # make new data frame
  select(industry, employ_n, year, race_gender)%>% #only pick out what I want to start looking at
  group_by(industry)%>% #group first before you summarise
  summarise(mean_employ_n = mean(employ_n, na.rm = TRUE))%>% # take the means of the number of people employed per industry
  filter(complete.cases(.))%>% # remove n/a
  pivot_wider(names_from = industry, # make the data set wider to test
              values_from = mean_employ_n)%>% #columns for each industry
  pivot_longer(cols = 1:20)%>% #easier to plot
  rename(industry = name, number_employed = value)%>% #rename the columms
  write_csv(here("Output", "Employed_Clean.csv")) #save data sheet 


employed_clean<-read.csv(here("Output", "Employed_Clean.csv")) # bruoght in data to make sure all was looking good, was having issues plotting

### Plot ####
employed_clean%>% #data frame
  ggplot(aes(x = industry, # x axis 
             y = number_employed, # y axis 
             color = industry))+ # each bar will be a different color 
  geom_col(show.legend = FALSE, fill = "white")+ # remove legend and remove fill 
  labs(title = "Average Number of People Employed in Each Industry (2015 - 2020)", # title the plot
       x = "Industry", # x axis 
       y = "Average Number of People Employed", # y axis 
       caption = "Taken from Current Population Suvery (past 6 years), rfordatascience/tidytuesday")+ # bottom caption
  theme_light()+ # make background light
  theme(axis.title = element_text(size = 13), # size of plot axis title font
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5), # make plot title centered
        axis.title.x = element_blank()) # change the size of the axis 
ggsave(here("Output", "Avg_Employed.png"), #save the plot!
       width = 7 , height = 5) 

