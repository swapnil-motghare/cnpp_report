library(tidyverse)
library(readxl)
library(lubridate)

cnpp_raw_df <- read_csv("data/surveys/CNPP Ride Guarantee Monthly Survey_8.11.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="True")%>% mutate(`Start Date`=ymd_hms(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month"))

cnpp_raw_english <- cnpp_raw_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="English") %>%
  mutate(ph_no=str_extract(`Please enter your phone number.`,"[0-9]+")) %>% filter(nchar(ph_no)==10) %>% # proper phone numbers
  group_by(ph_no,mnth) %>% filter(row_number()==1) %>% ungroup() # each phone number should only have one response each month

cnpp_raw_spanish <- cnpp_raw_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="Spanish/Español") %>%
  mutate(ph_no=str_extract(`Por favor escriba su número de teléfono si tiene uno.`,"[0-9]+")) %>% filter(nchar(ph_no)==10) %>% # proper phone numbers
  group_by(ph_no,mnth) %>% filter(row_number()==1) %>% ungroup() # each phone number should only have one response each month

# option 1: match by phone number #
tsi_monthly_df <- cnpp_raw_english %>%
  select(ph_no,mnth,TSI1=22,TSI2=23,TSI3=24) %>%
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
                                           ifelse(TSI_score<=6,"Severly Insecure",NA))))) %>%
  group_by(ph_no) %>%
  arrange(mnth) %>%
  mutate(grp=row_number())


# 

# baseline survey TSI
baseline_survey_df <- read_csv("data/surveys/CNPP - Baseline Survey_08.11.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="TRUE")%>% mutate(`Start Date`=mdy_hm(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month"))

baseline_english <- baseline_survey_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="English") %>%
  mutate(ph_no=str_extract(`Please enter your phone number, if you have one.`,"[0-9]+")) %>% filter(nchar(ph_no)==10) %>% # proper phone numbers
  group_by(ph_no,mnth) %>% filter(row_number()==1) %>% ungroup() # erliest response

tsi_baseline_df <- baseline_english %>%
  select(ph_no,mnth,TSI1=22,TSI2=23,TSI3=24) %>%
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
                                           ifelse(TSI_score<=6,"Severly Insecure",NA))))) %>%
  mutate(grp=0)

baseline_mnthly_df <- bind_rows(tsi_baseline_df,tsi_monthly_df) 

baseline_mnthly_df %>%
  group_by(grp) %>%
  summarise(avg_tsi=mean(TSI_score,na.rm=T)) %>%
  ggplot(aes(x=grp, y=avg_tsi)) + geom_point()

baseline_mnthly_df %>%
  group_by(grp) %>%
  summarise(n()) # very few respondents after month 5 - maybe stop at that?