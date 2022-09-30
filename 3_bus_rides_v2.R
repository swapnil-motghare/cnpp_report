library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

Buspass_data_edit <- read_excel("data/ride data/Buspass data 05_22.xlsx", 
                                sheet = "Rides")


bus_rides_df <- Buspass_data_edit %>%
  filter(Organization=="CNPP") 

b <- bus_rides_df %>%
  group_by(mnth=floor_date(date(Month), unit="month")) %>%
  summarise(total_transpo_rides=n(),total_transpo_riders=n_distinct(Pass)) %>%
  filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  mutate(avg_rides_per_user=total_transpo_rides/total_transpo_riders) %>% .$avg_rides_per_user

bus_rides_df %>%
  group_by(mnth=floor_date(date(Month), unit="month")) %>%
  summarise(`Total Transpo rides`=n(),`Total Transpo riders`=-1*n_distinct(Pass)) %>%
  filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  gather(key,value,-mnth) %>%
  ggplot(aes(x=mnth,y=value, fill=key)) +
  geom_bar(stat="identity")+ theme_minimal()+ 
  theme(legend.title = element_blank(), axis.title = element_blank()) +
  geom_text(aes(y=0,label = rep(round(b,0),2)), vjust = -0.2)+
  scale_x_date(labels = date_format("%b-%y")) +
  labs(title="Rides and riders by month - Transpo",
       caption = paste0("Total rides=",nrow(bus_rides_df),"\n",
                        "Total unique riders=",n_distinct(bus_rides_df$Pass)))

ggsave("rides_and_riders_transpo.png", width = 150*1.25, height = 75*1.25,units = "mm", dpi = 500, 
       path="images")

library(treemapify)

bus_rides_df %>%
  group_by(mnth=floor_date(date(Month), unit="month")) %>% filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  group_by(Name) %>%
  summarise(total_transpo_rides=n()) %>%
  ggplot(aes(area = total_transpo_rides, fill = Name, label=paste0(Name,"\n","(",total_transpo_rides,")"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15, reflow=T)+ 
  theme_minimal() + theme(legend.position = "none")+ 
  labs(title=paste("Total Transpo rides by Organization"),
       caption = paste("Total rides from","Oct-2022","through","May-2022 =",nrow(bus_rides_df)))

ggsave("rides_by_organization_transpo.png", width = 150*1.25, height = 150*1.25,units = "mm", dpi = 500, 
       path="images")

