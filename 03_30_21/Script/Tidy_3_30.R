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
tuesdata <- tidytuesdayR::tt_load('2021-03-30') # bring data into the environment
allNumbers<-tuesdata$allNumbers # filter out what data I want to work with 


####Analyze Data####
view(allNumbers) # view data set to see what I am working with 

makeup_clean <- allNumbers[complete.cases(allNumbers),] # remove all NAs from the data set 
view(makeup_clean) # view

makeup_cleaner<- makeup_clean%>% # clean up the data
  select(brand, specific, lightness)%>% # select for the columns that I want to use 
  group_by(brand)%>% # group by brand, so that I can later average the brands
  summarise(avg_color = mean(lightness)) # take the average of the lightness color for each brand 

view(makeup_cleaner)

### Plot the data ####
# I want to plot a line graph showing the total numenr of bechdel points awarded each year
makeup_cleaner%>% # this is the data set I will use
  ggplot(aes(x = brand, # make a ggplot with x data being brand name
             y = avg_color))+  # y data is average color shade 
  geom_point()+ # make a point graph 
  labs(title = "Average Color Value of Popular Makeup Brands (0-1 Scale)", # title name 
       x = "Brand Name", # x axis name 
       y = "Average Color Shade (Scale 0 - 1)", # y axis name 
       caption = "data from rfordatascience/tidytuesday, 0.0 lightest shade, 1.0 darkest shade")+ # caption, add a bit of extra information
  theme_classic()+ # remove background lines, simple theme
  theme(axis.title = element_text(size = 13),  # make axis font larger
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # change the angle of the x axis text to see it all
        plot.title = element_text(hjust = 0.5))+ # center the plot title 
  ggsave(here("03_30_21", "Output", "Average_Makeup_Shades.png"), width = 7 , height = 5) # save the plot

