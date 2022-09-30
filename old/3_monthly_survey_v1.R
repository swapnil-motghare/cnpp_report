library(tidyverse)
library(readxl)
library(lubridate)

CNPP_Monthly_Survey_2102022_edit <- read_csv("data/surveys/CNPP Monthly Survey_7.4.2022_edit.csv", skip = 1) %>%
  select(`Start Date`, TSI1=22,TSI2=23,TSI3=24) %>% mutate(`Start Date`=mdy_hm(`Start Date`))

tsi_df <- CNPP_Monthly_Survey_2102022_edit %>%
  mutate(TSI1=ifelse(TSI1=="Never",0,
                     ifelse(TSI1=="Sometimes",1,
                            ifelse(TSI1=="Often",2,NA))),
         TSI2=ifelse(TSI2=="Never",0,
                     ifelse(TSI2=="Sometimes",1,
                            ifelse(TSI2=="Often",2,NA))),
         TSI3=ifelse(TSI3=="Never",0,
                     ifelse(TSI3=="Sometimes",1,
                            ifelse(TSI3=="Often",2,NA))),
         TSI_score=TSI1+TSI2+TSI3,
         TSI_category=ifelse(TSI_score==0,"Transportation Secure",
                             ifelse(TSI_score<=2,"Minimally Insecure",
                                    ifelse(TSI_score<=4,"Moderately Insecure",
                                           ifelse(TSI_score<=6,"Severly Insecure",NA))))) 

tsi_df %>%
  group_by(yr=year(`Start Date`),mnth=month(`Start Date`)) %>%
  summarise(avg_score=mean(TSI_score,na.rm=T),obs=n()) %>%
  mutate(yr_mnth=paste0(yr,"-",mnth)) %>%
  ggplot(aes(x=yr_mnth, y=avg_score)) + geom_point()

u <- tsi_df %>%
  group_by(month(`Start Date`),TSI=TSI_category) %>%
  filter(!is.na(TSI)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(TSI=factor(TSI,levels = c("Severly Insecure","Moderately Insecure","Minimally Insecure","Transportation Secure"))) %>%
  arrange(desc(TSI)) %>%
  mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=TSI)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(caption = paste0("Responses=",sum(!is.na(tsi_df$`TSI_category`))))