library(tidyverse)
library(readxl)
library(lubridate)

cnpp_raw_df <- read_csv("data/surveys/CNPP Ride Guarantee Monthly Survey_8.11.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="True")%>% mutate(`Start Date`=ymd_hms(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month"))

cnpp_raw_english <- cnpp_raw_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="English") %>%
  mutate(ph_no=sapply(str_extract_all(`Please enter your phone number.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) # proper phone numbers
  
cnpp_raw_spanish <- cnpp_raw_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="Spanish/Español") %>%
  mutate(ph_no=sapply(str_extract_all(`Por favor escriba su número de teléfono si tiene uno.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) # proper phone numbers

# just deal with english for now
# option 1: match by phone number #
tsi_monthly_df <- bind_rows(cnpp_raw_english %>%
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
                                           ifelse(TSI_score<=6,"Severly Insecure",NA))))),
  cnpp_raw_spanish %>%
    select(ph_no,mnth,TSI1=35,TSI2=36,TSI3=37) %>%
    mutate(TSI1=ifelse(TSI1=="Nunca",0,
                       ifelse(TSI1=="Algunas Veces",1,
                              ifelse(TSI1=="A menudo",2,NA))),
           TSI2=ifelse(TSI2=="Nunca",0,
                       ifelse(TSI2=="Algunas Veces",1,
                              ifelse(TSI2=="A menudo",2,NA))),
           TSI3=ifelse(TSI3=="Nunca",0,
                       ifelse(TSI3=="Algunas Veces",1,
                              ifelse(TSI3=="A menudo",2,NA))),
           TSI_score=TSI1+TSI2+TSI3,
           TSI_category=ifelse(TSI_score==0,"Transportation Secure",
                               ifelse(TSI_score<=2,"Minimally Insecure",
                                      ifelse(TSI_score<=4,"Moderately Insecure",
                                             ifelse(TSI_score<=6,"Severly Insecure",NA)))))
  )%>%
  group_by(ph_no,mnth) %>% filter(row_number()==1) %>% ungroup() %>% # each phone number should only have one response in each month
  mutate(grp="monthly")

rm(cnpp_raw_df,cnpp_raw_english,cnpp_raw_spanish)

# 

# baseline survey TSI
baseline_survey_df <- read_csv("data/surveys/CNPP - Baseline Survey_08.11.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="TRUE")%>% mutate(`Start Date`=mdy_hm(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month"))

baseline_english <- baseline_survey_df %>%
  #filter(`Would you like to take this survey in English or Spanish?`=="English") %>%
  mutate(ph_no=sapply(str_extract_all(`Please enter your phone number, if you have one.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) # proper phone numbers

# no need to do the following as all TSI responses for those who took the survey in Spanish are also recorded in same columns as english
#baseline_spanish <- baseline_survey_df %>%
#  filter(`Would you like to take this survey in English or Spanish?`=="Spanish/Español") %>%
#  mutate(ph_no=str_extract(`Please enter your phone number, if you have one.`,"[0-9]+")) %>% filter(nchar(ph_no)==10) # proper phone numbers

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
  arrange(ph_no,mnth) %>% group_by(ph_no) %>% filter(row_number()==1) %>% ungroup() %>% # each phone number should only have one response in baseline
  mutate(grp="baseline")

rm(baseline_survey_df,baseline_english)

baseline_mnthly_df <- bind_rows(tsi_baseline_df,tsi_monthly_df) %>%
  arrange(ph_no,mnth,grp) %>% group_by(ph_no,mnth) %>% filter(row_number()==1) %>% ungroup() %>% # if repeated in baseline and monthly, pick the baseline response
  group_by(ph_no) %>% mutate(diff_mnth=interval(lag(mnth),mnth) %/% months(1),
                             diff_mnth=ifelse(is.na(diff_mnth),0,diff_mnth),
                             mnth_no=cumsum(diff_mnth)) %>% ungroup() %>%
  filter(ph_no!="1231231234") # doesn't look like phone number
  
rm(tsi_baseline_df,tsi_monthly_df)

# distribution of baseline surveys
baseline_mnthly_df %>%
  ggplot(aes(x=mnth)) + geom_histogram()

baseline_mnthly_df %>%
  group_by(mnth_no) %>%
  summarise(n()) # very few respondents after month 5 - maybe stop at that?

subset_bm_df <- baseline_mnthly_df %>%
  mutate(mnth_no=as.integer(mnth_no)) %>%
  group_by(ph_no) %>% filter(any(mnth_no==0)) %>%# must have responde to baseline
  filter(any(mnth_no==1) | any(mnth_no==2)) %>% # must have responded in month 1 or 2
  filter(any(mnth_no==3) | any(mnth_no==4)) %>%# must have responded in month 3 or 4
  filter(any(mnth_no==5) | any(mnth_no==6))
         
cut_off_mnth=6

small_subset_gm_df <- subset_bm_df %>%
  filter(mnth_no<=cut_off_mnth) 

small_subset_gm_df %>%
  group_by(mnth_no) %>%
  summarise(obs=n())

small_subset_gm_df %>%
  group_by(mnth_no) %>%
  summarise(avg_tsi=mean(TSI_score,na.rm=T)) %>%
  ggplot(aes(x=mnth_no, y=avg_tsi)) + geom_point() + ylim(c(0,6)) +
  geom_smooth(method="lm", se=F) +
  theme_minimal() +
  labs(title=paste0("TSI scores for",small_subset_gm_df$ph_no),
       x="Month since enrollment", y="TSI score",
    caption = paste0("Responses="))+
  annotate("rect", xmin = 0, xmax = cut_off_mnth, ymin = 0, ymax = 2,
           alpha = .1,fill = "green")+
  annotate("text", x=0.75, y= 0.5,label = "Transportation secure")+
  annotate("rect", xmin = 0, xmax = cut_off_mnth, ymin = 2, ymax = 4,
           alpha = .1,fill = "orange")+
  annotate("text", x=0.75, y= 2.5,label = "Moderately Insecure")+
  annotate("rect", xmin = 0, xmax = cut_off_mnth, ymin = 4, ymax = 6,
           alpha = .1,fill = "red") +
  annotate("text", x=0.75, y= 4.5,label = "Severely Insecure")

small_subset_gm_df %>% group_by(mnth_no) %>% summarise(obs=n())