#Tidy tuesday 
#Week 32 


library(readr)
library(tidyverse)
library(tidytuesdayR)
library(dplyr)
library(png)
library(ggimage)
library(magick)


#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
athletes <- tuesdata$athletes

 
#filter only gold medals in swimming
swim <- athletes %>% filter(type == "Swimming", 
                            medal == "Gold") %>% 
  mutate(abb = fct_lump_n(abb,10))  %>% #lump 10 highest and drop other
  filter(abb != "Other")
  

swim <- swim %>%  group_by(abb) %>% 
  summarize( count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(abb = fct_recode(abb,
                                 "Spain" = "ESP",
                                 "China" = "CHN",
                                 "Great Britain" = "GBR",
                                 "Canada" = "CAN",
                                 "Australia" = "AUS",
                                 "USA" = "USA",
                                  "France" = "FRA", 
                                  "Sweden" = "SWE",
                                  "Poland" = "POL",
                                  "Netherlands" = "NED")) 

#ad png of swimmer to data table
swim$swimmer <- "Swim..png"


swim %>% ggplot(aes(count, fct_reorder(abb,count)))+
  geom_col(fill = "#D4AF37")+
  #ad swimmer on column end 
  geom_image(aes(image = swimmer), size = 0.1)+
  theme(plot.title = element_text(size = 25, family = "mono", face = "bold"),
        plot.subtitle = element_text(size = 10, family = "mono", face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13, family = "Nunito Bold", color = "#A13941FF", face = "bold"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#A8DBFF"))+
  #ad horizontal lines
  geom_hline(yintercept = seq(1.52,9.52,1), color = "#06429C", linetype =3, size = 1.2)+
  #ad country names
  geom_text(aes(x = 5, y = fct_reorder(abb,count), label = abb), 
            color = "#A13941FF", hjust = 0, position = position_nudge(y = 0.1),
            fontface = "bold", family = "Nunito Bold", size  = 5)+
  labs(title = "Paralympic swimming",
        subtitle = "10 best countries in terms of the number of gold medals won")
            
  

  

