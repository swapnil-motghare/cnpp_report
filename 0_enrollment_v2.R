# this code gives the number of enrollments at any point in time
# we have two files i) from codeworks which is probably used to send uber/ lyft ride codes every month
# ii) CNPP file which is th one used to distribute bus pass every month

library(tidyverse)
library(readxl)
library(lubridate)

#### codeworks enrollments ####
# work with most recent file as it seems to contain all old data
rs_vouchers <- read_csv("data/enrollment/July Link Extract.csv") # 1588

rs_voucher_clean <- rs_vouchers  %>% 
  mutate(full_name=paste(first_name,last_name),
         mnth=my(paste0(month,year))) %>%
  select(full_name,org=name,mnth,rs_voucher=url) %>%
  filter(!str_detect(full_name, fixed('test', ignore_case=TRUE)), # remove test observations
         !duplicated(rs_voucher))# remove duplicated vouchers - most likely test 

#### bus pass ####
bus_passes <- read_csv("data/enrollment/CNPP Bus Pass Distribution_August 17, 2022_07.18.csv", skip = 1)%>% slice(-1)

bus_pass_clean <- bus_passes  %>%
  mutate(mnth=my(`Select the month for which the pass is provided.`)) %>%
   select(full_name=`Enter the name of the client you are providing a 31-day bus pass.`,
           org=`Select your organization.`, mnth,
          bus_pass_no=`Select the number of the bus pass provided.`,
          bus_pass_type=`Select from the following.`) %>%
  filter(!str_detect(full_name, fixed('test', ignore_case=TRUE)), # remove test observations
         nchar(bus_pass_no)>=5, # bus pass code is 6 characters long. allowing for minor mistakes with cutoff at 5
         !duplicated(bus_pass_no)) # ignore duplicatd bus pass - check if this can be improved

# total enrollees cannot be simple sumof the two as some are enrolled in both bus pass and uber rides 
# Nakeisha Caston
# use full join

rs_bus <- full_join(rs_voucher_clean,bus_pass_clean, by = c("full_name", "org", "mnth")) %>%
  arrange(org,full_name,mnth)
# this df shows for each month, if the enrollee was sent the bus pass or uber voucher.

nrow(rs_voucher_clean) + nrow(bus_pass_clean) > nrow(rs_bus)

rm(rs_voucher_clean,bus_pass_clean,rs_vouchers,bus_passes)

#### cleaning steps ####

unique(rs_bus$org)

# 1. focus on cnpp enrolees only
rs_bus_clean <- rs_bus %>%
  mutate(org=ifelse(org=="South Bend Heritage","South Bend Heritage Foundation",org),
         org=ifelse(org=="Center For the Homeless","Center for the Homeless",org)) %>%
  filter(org=="REAL Services"|org=="Hope Ministries"| org=="St. Margaret's House"| org=="South Bend Heritage Foundation"|
         org=="Center for the Homeless" |org=="RiverBend Cancer Services"| org=="La Casa de Amistad"| org=="YWCA North Central Indiana"|
           org=="United Health Services"| org=="Green Bridge Growers"|org=="Center for Positive Change"| org=="United Religious Community"|
           org=="Catholic Charities") %>%
  filter(mnth>="2021-11-01") %>%# focus on months after Nov 2022
  filter(full_name!="124100") # doesn't look like a name
# # 3, fix names spelled diffeently
  
rs_bus_clean %>%
  group_by(mnth) %>%
  summarise(n_uber_pass=sum(!is.na(rs_voucher)), n_bus_pass=sum(!is.na(bus_pass_no)),
            renewal_bus_pass=sum(bus_pass_type=="This is a new monthly bus pass for an existing participant.", na.rm=T),
            new_bus_pass=sum(str_detect(bus_pass_type,"This is the first enrollment"),na.rm = T)) %>% ungroup() %>%
  select(mnth:n_bus_pass) %>%
  gather(key,value,-mnth) %>%
  ggplot(aes(x=mnth, y=value, group=key, fill=key)) + geom_bar(stat="identity", position = position_dodge())

rs_bus_clean %>%
  group_by(org) %>%
  summarise(enrolees=n_distinct(full_name)) %>% arrange(desc(enrolees))
# 

# this files gives enrollment info which should be updated in master data file
rs_bus_clean %>% write_rds("data/rs_bus_clean.Rds")