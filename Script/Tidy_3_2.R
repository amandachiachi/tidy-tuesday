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

youtube_plot <- youtube_clean %>% #data frame
  ggplot(aes(x = theme, # x axis 
             y = like_count, 
             color = theme))+ # each bar will be a different color 
  geom_boxplot(show.legend = FALSE)+
  geom_jitter(show.legend = FALSE) +# remove legend and remove fill 
  labs(title = "", # title the plot
       x = "", # x axis 
       y = "Number of Likes Per Video", # y axis 
       caption = "rfordatascience/tidytuesday")+ # bottom caption
  theme_light()+ 
    scale_y_continuous(limits = c(2500,6500))+ 
    scale_color_brewer()+
  theme(axis.title = element_text(size = 13), # size of plot axis title font
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), # make plot titles vertical 
        plot.title = element_text(hjust = 0.5))+

    # change the size of the axis 
#ggsave(here("Output", "Avg_Employed.png"), #save the plot!
      # width = 7 , height = 5) 

