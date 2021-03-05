###Tidy Tuesday for March 2nd 2021####
# The data this week comes from Superbowl commercial

#Load Libraries####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(dplyr)
library(PNWColors)
library(lubridate)
library(wesanderson)

#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube
pal <- wes_palette("BottleRocket1", 6, type = "discrete")

####Analyze Data####
 youtube_clean <- youtube %>%
  select(year, brand, funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex, like_count, dislike_count, favorite_count)%>%
  pivot_longer(funny:use_sex, 
               names_to = "theme", 
               values_to = "values") %>%
  filter(values == "TRUE")%>%
  filter(year == "2019" |
         year == "2020") %>%
  filter(brand == "Coca-Cola" |
         brand == "NFL" |
         brand == "Doritos" |
         brand == "E-Trade" |
         brand == "Budweiser") 
  view(youtube_clean)
  
# facet wrap by brand 
# x axis is theme
# y axis is like count 

### Plot ####
  
youtube_clean %>% 
    ggplot(aes(x = theme, 
               y = like_count, 
               color = theme))+
    geom_boxplot(show.legend = FALSE)+
    geom_jitter(show.legend = FALSE) +
    labs(title = "Which Superbowl Commercial Theme Do Consumers Enjoy More?", 
          x = "", 
          y = "Number of Likes", 
          caption = "data from rfordatascience/tidytuesday")+
    theme_light()+
    scale_y_continuous()+
    scale_color_brewer(palette = 17)+ 
    theme(axis.title = element_text(size = 15), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+
    ggsave(here("Output", "SuperBowl_Ads.png"), width = 7 , height = 5) 

