# we need a way to get rides by id
# the file that jamison shared, rides are identified by voucher id
# so we need a way to link voucher id to id

# Step 1: Get Jamison's file, do a vlookup on the first sheet to add id

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Step 2 - get monthly rides by voucher
Uber_CNPP_2 <- read_excel("data/ride data/Uber CNPP_edit.xlsx", 
                             sheet = "CNPP Rides") %>%
  select(`Transaction Timestamp (UTC)`, `Voucher Link`)

rides_by_voucher <- Uber_CNPP_2 %>%
  group_by(voucher_link=`Voucher Link`, mnth=floor_date(date(`Transaction Timestamp (UTC)`), unit = "month")) %>%
  summarise(rides=n())

rm(Uber_CNPP_2)

# Step 3 - match voucher to id

Uber_CNPP_1 <- bind_rows(
  read_excel("data/ride data/Uber CNPP_edit.xlsx", 
             sheet = "Enrollees") %>%
    select(voucher_link=`Anytime`,id),
  read_excel("data/ride data/Uber CNPP_edit.xlsx", 
             sheet = "Enrollees") %>%
    select(voucher_link=`N&W`,id)) %>% filter(!is.na(id))

df <- left_join(rides_by_voucher,Uber_CNPP_1, by="voucher_link") %>%
  group_by(id,mnth) %>%
  summarise(rides=sum(rides)) %>% # this gives monthly uber rides by id which can be linked with user # na id could be unmatched ids or notre dame employees
  ungroup() %>%
  filter(mnth>=ymd("2021-10-01"))

rm(rides_by_voucher,Uber_CNPP_1)

# monthly rides
n_distinct(df$id) # 66 ids are matched out of 125

p1 <- df %>%
  filter(!is.na(id)) %>%
  group_by(mnth) %>%
  summarise(total_uber_rides=sum(rides)) %>%
  ggplot(aes(x=mnth, y=total_uber_rides)) + geom_bar(stat="identity")+ 
  scale_x_date(labels = date_format("%b-%y")) +
  theme_minimal() + theme(axis.title = element_blank()) +
  labs(title="Rides by month - Uber",
       caption = paste0("Total rides=",sum(filter(df,!is.na(id))$rides)))

q1 <- df %>%
  filter(!is.na(id)) %>%
  group_by(mnth) %>%
  summarise(total_uber_riders=n_distinct(id)) %>%
  ggplot(aes(x=mnth, y=total_uber_riders)) + geom_bar(stat="identity")+ 
  scale_x_date(labels = date_format("%b-%y")) +
  theme_minimal() + theme(axis.title = element_blank()) +
  labs(title="Riders by month - Uber",
       caption = paste0("Total unique riders=",n_distinct(df$id)))

p1+q1

ggsave("rides_and_riders.png", width = 150*1.25, height = 75*1.25,units = "mm", dpi = 500, 
       path="images")

# average rides
df %>%
  filter(!is.na(id)) %>%
  group_by(mnth) %>%
  summarise(total_uber_rides=sum(rides),total_uber_riders=n_distinct(id)) %>%
  mutate(total_uber_rides/total_uber_riders)

# average rides is problematic - look into this

# can do by employer - not used
master_data <- read_excel("data/master_data_v3.xlsx", sheet = "all_enrolees")

p <- df %>%
  left_join(select(master_data,id,org_name)) %>%
  filter(!is.na(id)) %>%
  group_by(mnth,org_name) %>%
  summarise(total_uber_rides=sum(rides)) %>%
  ggplot(aes(x=mnth, y=total_uber_rides, fill=org_name)) + geom_bar(stat="identity")

q <- df %>%
  left_join(select(master_data,id,org_name))%>%
  filter(!is.na(id)) %>%
  group_by(mnth,org_name) %>%
  summarise(total_uber_riders=n_distinct(id)) %>%
  ggplot(aes(x=mnth, y=total_uber_riders, fill=org_name)) + geom_bar(stat="identity")

library(patchwork)
p+q