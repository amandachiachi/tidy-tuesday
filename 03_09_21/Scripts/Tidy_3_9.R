###Tidy Tuesday for March 9th 2021####
# Data from the Bechdel Test

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
tuesdata <- tidytuesdayR::tt_load('2021-03-09') 
bechdel <- tuesdata$raw_bechdel # shows scores for the bechdel test
movies <- tuesdata$movies # lists the movies that the bechdel test was rated for 

####Analyze Data####
view(movies) 
view(bechdel)

bechdel_clean[bechdel_clean == 0] <- NA # make all 0s NAs 
bechdel_clean_no0 <- bechdel_clean[complete.cases(bechdel_clean),] # remove every row where this is an NA
view(bechdel_clean_no0) # view new data set 

bechel_clean<- bechdel_clean_no0%>% #rename so that it saves
  select(-(id:title))%>% # removed the columns that I didn't need 
  group_by(year)%>% # merged all of the data from the columns that had the same year into one 
  summarise(rating = sum(rating)) # these combined columns are added together

 view(bechel_clean) # great, this is what I want to plot 

### Plot the data ####
 # I want to plot a line graph showing the total numenr of bechdel points awarded each year
bechel_clean%>%
  ggplot(aes(x = year, # year is on my x axis 
             y = rating))+ # the total sum of the ratings per year is on my y axis 
   geom_line()+ # make it a line graph
  xlim(NA, 2020) + # only show information up to 2020 (2021 will skew data because we are only in the beginning)
   labs(title = "Total Bechdel Points for all Movies per year (1899 - 2020)", # Label the title
        x = "Year", # Label the x axis 
        y = "Total Number of Bechdel Points Awarded", # label Y axis 
        caption = "data from rfordatascience/tidytuesday (downward trend in female roles since 2014)")+ # add a caption at the bottom
   theme_light()+ # change theme, remove background color etc.
   theme(axis.title = element_text(size = 13))+ # make the text size 13
  ggsave(here("03_09_21", "Output", "BechdelTest.png"), width = 7 , height = 5) # save the plot

  