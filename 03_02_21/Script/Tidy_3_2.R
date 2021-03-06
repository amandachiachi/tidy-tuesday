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

here()

#Upload Data####
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube
pal <- wes_palette("BottleRocket1", 6, type = "discrete")

####Analyze Data####
 youtube_clean <- youtube %>%
  select(year, brand, funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex, like_count, dislike_count, favorite_count)%>% # select the columns I want to look at 
  pivot_longer(funny:use_sex, # make the "use" columns one 
               names_to = "theme",  # title of the column is "theme"
               values_to = "values") %>% #TRUE or FALSE are values
  filter(values == "TRUE")%>% # filter out only TRUE, the years I want, and the brands I want
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
  
youtube_clean %>% # choose data set to plot 
    ggplot(aes(x = theme, # x axis theme
               y = like_count, # y axis numner of likes 
               color = theme))+ # make each theme a different color 
    geom_boxplot(show.legend = FALSE)+ # no legend 
    geom_jitter(show.legend = FALSE) + # no legend 
    labs(title = "Which Superbowl Commercial Theme Do Consumers Enjoy More?", # title of the plot
          x = "", # no x axis
          y = "Number of Likes", # label the y axis 
          caption = "data from rfordatascience/tidytuesday")+ # lower caption 
    theme_light()+ # light theme to clean it up 
    scale_y_continuous()+ # make sure y axis is continuous
    scale_color_brewer(palette = 17)+ # choose colors for the plot 
    theme(axis.title = element_text(size = 15), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+
    ggsave(here("03_02_21", "Output", "SuperBowl_Ads.png"), width = 7 , height = 5) 

