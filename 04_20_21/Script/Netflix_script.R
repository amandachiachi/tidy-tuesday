###Tidy Tuesday for April 20th 2021####

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)

here()

#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-04-20') # load in the data
netflix <- tuesdata$netflix # parse out what data I want to use

netflix_clean <- netflix %>% 
  select(type, release_year, date_added)%>% # select the columns that he wanted to use 
  separate(date_added, #separate the date into different columns to see what I want to work with
           into = c("Month.Day", "Year"),
           sep = ", ", remove = TRUE)%>% # remove the extra column
  select(-Month.Day, -release_year)%>% # remove two columns
  group_by(type, Year)%>% # group by type and year
  summarise(type_count = n())%>% # make a new column adding up each entry for type_count

view(netflix_clean) # view the data 


netflix_clean%>%
  ggplot(aes(x = Year, # x axis will show the year
             y = type_count, # y axis will show the number of releases each year
             color = Year))+ # make each Year a different color
  geom_boxplot(show.legend = FALSE)+ # remove the legend 
  labs(title = "Number of Netflix Relseases per Year", # title whole plot
       x = "Year",  # title x axis
       y = "Number of Movie or TV Relseases each Year", # title y axis 
       caption = "data from rfordatascience/tidytuesday")+ # give it a caption
  theme_light()+ # make the theme clean
  scale_y_continuous()+ # make sure y axis is continuous
  theme(axis.title = element_text(size = 15), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+
  ggsave(here("04_20_21", "Output", "Netflix.png"), width = 7 , height = 5) 

