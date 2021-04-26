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
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix

pal <- PNWColors::("BottleRocket1", 6, type = "discrete")

?pivot_wider
netflix_clean <- netflix %>%
  select(type, release_year, date_added)%>%
  separate(date_added, #separate year from month and year
           into = c("Month.Day", "Year"),
           sep = ", ", remove = TRUE)%>%
  select(-Month.Day, -release_year)%>%
  group_by(type, Year)%>%
  summarise(type_count = n())%>%

view(netflix_clean)


netflix_clean%>%
  ggplot(aes(x = Year, 
             y = type_count, 
             color = Year))+
  geom_boxplot(show.legend = FALSE)+
  labs(title = "Number of Netflix Relseases per Year",
       x = "Year", 
       y = "Number of Movie or TV Relseases each Year", 
       caption = "data from rfordatascience/tidytuesday")+
  theme_light()+
  scale_y_continuous()+ # make sure y axis is continuous
  theme(axis.title = element_text(size = 15), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+
  ggsave(here("04_20_21", "Output", "Netflix.png"), width = 7 , height = 5) 

