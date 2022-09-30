# we need a way to get rides by id
# the file that jamison shared, rides are identified by voucher id
# so we need a way to link voucher id to id - we rely on jamison matching in rides file

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

Uber_CNPP <- read_excel("data/ride data/Uber CNPP.xlsx", 
                        sheet = "CNPP Rides")

# average rides
a <- Uber_CNPP %>%
  group_by(mnth=as.Date(floor_date(`Transaction Timestamp (UTC)`, unit="month"))) %>%
  summarise(total_uber_rides=n(),total_uber_riders=n_distinct(`Full Name`)) %>%
  filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  mutate(avg_rides_per_user=total_uber_rides/total_uber_riders) %>% .$avg_rides_per_user
  
# rides, riders, average rides per rider
Uber_CNPP %>%
  group_by(mnth=as.Date(floor_date(`Transaction Timestamp (UTC)`, unit="month"))) %>%
  summarise(`Total Uber rides`=n(), `Total Uber riders`=-1*n_distinct(`Full Name`)) %>%
  filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  gather(key,value,-mnth) %>%
  ggplot(aes(x=mnth,y=value, fill=key)) +
  geom_bar(stat="identity") + theme_minimal() +
  theme(legend.title = element_blank(), axis.title = element_blank()) +
  geom_text(aes(y=0,label = rep(round(a,0),2)), vjust = -0.2)+
  scale_x_date(labels = date_format("%b-%y")) +
  labs(title="Rides and riders by month - Uber",
       caption = paste0("Average rides per rider is shown at base of the bars","\n",
         "Total unique riders=",n_distinct(Uber_CNPP$`Full Name`),"\n",
                        "Total rides = ",nrow(Uber_CNPP)))

ggsave("rides_and_riders.png", width = 150*1.25, height = 75*1.25,units = "mm", dpi = 500, 
       path="images")

# rides by employer
library(treemapify)

Uber_CNPP %>%
  group_by(mnth=as.Date(floor_date(`Transaction Timestamp (UTC)`, unit="month"))) %>% filter(mnth>="2021-10-01", mnth<="2022-07-01") %>%
  mutate(Organization=ifelse(Organization=="Real Services","REAL Services",Organization),
         Organization=ifelse(Organization=="YWCA","YWCA North Central Indiana",Organization)) %>%
  group_by(Organization) %>%
  summarise(total_uber_rides=n()) %>%
  ggplot(aes(area = total_uber_rides, fill = Organization, label=paste0(Organization,"\n","(",total_uber_rides,")"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15, reflow=T)+ 
  theme_minimal() + theme(legend.position = "none")+ 
  labs(title=paste("Total Uber Rides by Organization"),
       caption = paste("Total rides from","Oct-2022","through","July-2022 =",nrow(Uber_CNPP)))

ggsave("rides_by_organization.png", width = 150*1.25, height = 150*1.25,units = "mm", dpi = 500, 
       path="images")
