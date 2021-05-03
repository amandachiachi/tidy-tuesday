# Tidy Tuesday 04_27_21

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)

# Read in the data
tuesdata <- tidytuesdayR::tt_load('2021-04-27') 
departures<- tuesdata$departures

head(departures) # view the data 

# I want to make a plot faceted by departure code
# x axis will be the date
# y axis will be the count of individuals

departures_clean <-departures%>% # indicate what data I am using
  select(departure_code, fyear)%>% # remove other columns that I don't need
  group_by(departure_code, fyear)%>% # lump data together by year 
  summarise(count = sum(departure_code))%>% # and count the amount of each department codes for each year
  na.exclude() # remove the NAs


view(departures_clean) # view the cleaned data

departures_clean%>% 
  ggplot(aes(x = fyear, # indicate data for x
             y = count, # indicate data for y 
             fill = departure_code))+
  geom_area(show.legend = FALSE)+ # remove the legend 
  facet_wrap(~departure_code)+ # make each departure code a different plot 
  labs(title = "Overview of Reasons CEO's Left Their Job Each Year", # title whole plot
       x = "Year",  # title x axis
       y = "Number of CEO's", # title y axis 
       caption = "1:Death 2:Illness 3: Dismissal for job performance 4: Dismissal for legal violations 5: CEO Retired 6: New Job Opportunity 7: Other 8: Missing")+ # give it a caption
  theme_light()+ # make the theme clean
  scale_y_continuous()+ # make sure y axis is continuous
  theme(axis.title = element_text(size = 15), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+
  ggsave(here("04_27_21", "Output", "CEO.png"), width = 7 , height = 5)  # save the data!

