###Tidy Tuesday for March 30th 2021####


#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)
library(wesanderson)

here()
#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-03-30') 
allNumbers<-tuesdata$allNumbers


####Analyze Data####
view(allNumbers)

makeup_clean <- allNumbers[complete.cases(allNumbers),]
view(makeup_clean)

makeup_cleaner<- makeup_clean%>%
  select(brand, specific, lightness)%>%
  group_by(brand)%>%
  summarise(avg_color = mean(lightness))

view(makeup_cleaner)

### Plot the data ####
# I want to plot a line graph showing the total numenr of bechdel points awarded each year
makeup_cleaner%>%
  ggplot(aes(x = brand, 
             y = avg_color))+
  geom_point()+
  labs(title = "Average Color Value of Popular Makeup Brands (0-1 Scale)",
       x = "Brand",
       y = "Average Color Rating", 
       caption = "data from rfordatascience/tidytuesday")+
  theme_classic()+
  theme(axis.title = element_text(size = 13), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#  ggsave(here("03_09_21", "Output", "BechdelTest.png"), width = 7 , height = 5) # save the plot

