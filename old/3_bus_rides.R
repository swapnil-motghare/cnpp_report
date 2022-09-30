library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

Buspass_data_edit <- read_excel("data/ride data/Buspass data_edit.xlsx", 
                                sheet = "Rides")


bus_rides_df <- Buspass_data_edit %>%
  filter(Organization=="CNPP") 

t1 <- bus_rides_df %>%
  group_by(mnth=floor_date(date(Month), unit="month"),Organization) %>%
  summarise(rides=n()) %>%
  ggplot(aes(x=mnth, y=rides)) + geom_bar(stat="identity")+ 
  scale_x_date(labels = date_format("%b-%y")) +
  theme_minimal() + theme(axis.title = element_blank()) +
  labs(title="Rides by month - Transpo",
       caption = paste0("Total rides=",nrow(bus_rides_df)))

t2 <- bus_rides_df %>%
  group_by(mnth=floor_date(date(Month), unit="month"),Organization) %>%
  summarise(total_transpo_riders=n_distinct(Pass)) %>%
  ggplot(aes(x=mnth, y=total_transpo_riders)) + geom_bar(stat="identity")+ 
  scale_x_date(labels = date_format("%b-%y")) +
  theme_minimal() + theme(axis.title = element_blank()) +
  labs(title="Riders by month - Transpo",
       caption = paste0("Total unique riders=",n_distinct(bus_rides_df$Pass)))

t1+t2

ggsave("rides_and_riders_transpo.png", width = 150*1.25, height = 75*1.25,units = "mm", dpi = 500, 
       path="images")