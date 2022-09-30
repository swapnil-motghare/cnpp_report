# this is the first file trying to understand the preliminary data

library(tidyverse)
library(readxl)

#### Enrollment ####
# how many individuals have ever enrolled?
master_data <- read_excel("data/master_data_v3.xlsx", sheet = "all_enrolees")

n_distinct(master_data$Name) # 125
n_distinct(master_data$id) # 125

# individuals by organization
master_data %>%
  group_by(org_name) %>%
  summarise(n_enrolees=n_distinct(id)) %>%
  arrange(desc(n_enrolees))

# number of cumulative enrollments by time


# TSI scores for enrolled users
tsi_df <- master_data %>%
  select(contains("TSI")) %>%
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
  ggplot(aes(x=TSI_score,fill=TSI_category)) + geom_histogram(binwidth = 1)

tsi_df %>%
  group_by(TSI_category) %>%
  filter(!is.na(TSI_category)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) # 88% are moderately or severly insecure

p <- master_data %>% distinct(id,`Please indicate your age.`) %>%
  group_by(Age=`Please indicate your age.`) %>%
  filter(!is.na(Age)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(Age=factor(Age,levels = c("65 or older","55-64","45-54","35-44","25-34","18-24","Under 18"))) %>%
  ggplot(aes(x=1,y=prop, fill=Age)) + geom_bar(stat="identity") + 
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank()) +
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Please indicate your age.`))))

q <- master_data %>%
  group_by(Gender=`Please indicate your gender.`) %>%
  filter(!is.na(Gender)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  ggplot(aes(x=1,y=prop, fill=Gender)) + geom_bar(stat="identity") +
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Please indicate your gender.`))))

r <- master_data %>%
  group_by(Income=`Was your total household income in the past 12 months?`)  %>%
  filter(!is.na(Income)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(Income=factor(Income,levels = c("$30,000 - $39,999","$20,000 - $29,999","$10,000 - $19,999","Less than $10,000"))) %>%
  ggplot(aes(x=1,y=prop, fill=Income)) + geom_bar(stat="identity") + 
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Was your total household income in the past 12 months?`))))

s <- master_data %>%
  group_by(Race=`Which of the following best describes you?`)  %>%
  filter(!is.na(Race)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  ggplot(aes(x=1,y=prop, fill=Race)) + geom_bar(stat="identity") + 
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Which of the following best describes you?`))))

library(patchwork)
p+q+r+s + plot_layout(nrow=1)

master_data %>%
  group_by(`Ability to Travel`=`In the past 30 days, my ability to travel around has been...`) %>%
  filter(!is.na(`Ability to Travel`)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(`Ability to Travel`=factor(`Ability to Travel`,levels=c("Not stressful","Somewhat stressful","Very stressful"))) %>%
  ggplot(aes(x=1,y=prop, fill=`Ability to Travel`)) + geom_bar(stat="identity") + 
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`In the past 30 days, my ability to travel around has been...`))))

# this allows multiple responses + also text response - fix this
master_data %>%
  group_by(`Prevented`=`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`) %>%
  filter(!is.na(`Prevented`)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  #mutate(`Ability to Travel`=factor(`Ability to Travel`,levels=c("Not stressful","Somewhat stressful","Very stressful"))) %>%
  ggplot(aes(x=1,y=prop, fill=`Prevented`)) + geom_bar(stat="identity") + 
  theme_minimal() + theme(axis.title = element_blank(), axis.text.x = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`))))

#### Usage ####

