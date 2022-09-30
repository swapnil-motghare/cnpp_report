# this is the first file trying to understand the preliminary data

library(tidyverse)
library(readxl)
library(patchwork)

#### Enrollment ####
# how many individuals have ever enrolled?
master_data <- read_excel("data/master_data_v5.xlsx", sheet = "all_enrolees") # 246

# individuals by organization
master_data %>%
  group_by(org_name) %>%
  summarise(n_enrolees=n()) %>%
  arrange(desc(n_enrolees))

# number of cumulative enrollments by time

# demographic

p <- master_data %>%
  group_by(Age=`Please indicate your age.`) %>%
  filter(!is.na(Age)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(Age=factor(Age,levels = c("65 or older","55-64","45-54","35-44","25-34","18-24","Under 18"))) %>%
  arrange(desc(Age)) %>%
  mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=Age)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Please indicate your age.`))))

q <- master_data %>%
  group_by(Gender=`Please indicate your gender.`) %>%
  mutate(Gender=ifelse(Gender=="Male","Men",
                       ifelse(Gender=="Female","Women",Gender))) %>%
  filter(!is.na(Gender)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>% 
  mutate(Gender=factor(Gender,levels = c("Women","Men"))) %>% arrange(desc(Gender)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=Gender)) + geom_bar(stat="identity") +
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Please indicate your gender.`))))

r <- master_data %>%
  mutate(`Was your total household income in the past 12 months?`=ifelse(`Was your total household income in the past 12 months?`=="$10,000 - $19,000","$10,000 - $19,999",`Was your total household income in the past 12 months?`)) %>%
  mutate(Income=ifelse(`Was your total household income in the past 12 months?`=="Less than $10,000" | 
                         `Was your total household income in the past 12 months?`=="$10,000 - $19,999" |
                         `Was your total household income in the past 12 months?`=="$20,000 - $29,999" |
                         `Was your total household income in the past 12 months?`=="$30,000 - $39,999",
                       `Was your total household income in the past 12 months?`,"$40,000+")) %>%
  group_by(Income)  %>%
  filter(!is.na(Income)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(Income=factor(Income,levels = c("$40,000+","$30,000 - $39,999","$20,000 - $29,999","$10,000 - $19,999","Less than $10,000"))) %>%
  arrange(desc(Income)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=Income)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Was your total household income in the past 12 months?`))))

s <- master_data %>%
  group_by(Race=`Which of the following best describes you?`)  %>%
  filter(!is.na(Race)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(Race=factor(Race,levels = c("A race/ethnicity not listed here","Asian or Pacific Islander","Multiracial or Biracial","Hispanic or Latino","White or Caucasian","Black or African American"))) %>%
  arrange(desc(Race)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=Race)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`Which of the following best describes you?`))))


p+q+r+s + plot_layout(nrow=1)

ggsave("demo_enrolees.png", width = 300, height = 75,units = "mm", dpi = 500, 
       path="images")



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
                                           ifelse(TSI_score<=6,"Severely Insecure",NA))))) 


u <- tsi_df %>%
  group_by(TSI=TSI_category) %>%
  filter(!is.na(TSI)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(TSI=factor(TSI,levels = c("Severely Insecure","Moderately Insecure","Minimally Insecure","Transportation Secure"))) %>%
  arrange(desc(TSI)) %>%
  mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=TSI)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(caption = paste0("Responses=",sum(!is.na(tsi_df$`TSI_category`))))

# Ability to travel stressful?
t <- master_data %>%
  group_by(`Ability to Travel`=`In the past 30 days, my ability to travel around has been...`) %>%
  filter(!is.na(`Ability to Travel`)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(obs)) %>%
  mutate(`Ability to Travel`=factor(`Ability to Travel`,levels=c("Very stressful","More or less stressful","Somewhat stressful","Not stressful"))) %>%
  arrange(desc(`Ability to Travel`)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=`Ability to Travel`)) + geom_bar(stat="identity") + 
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank())+
  labs(caption = paste0("Responses=",sum(!is.na(master_data$`In the past 30 days, my ability to travel around has been...`))))

u+t + plot_layout(nrow=1)

ggsave("tsi_stress.png", width = 150, height = 75,units = "mm", dpi = 500, 
       path="images")

# prevented from achieving
b <- master_data %>%
  select(`Prevented`=`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`) %>%
  separate(`Prevented`,into=c("a","b","c","d","e","f","g"), sep = ",") %>%
  gather(key,`Prevented`) %>% 
  group_by(`Prevented`) %>%
  filter(!is.na(`Prevented`), `Prevented`!="None of the above") %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(!is.na(master_data$`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`))) %>%
  mutate(`Prevented`=factor(`Prevented`,levels = c("Work increased hours at a job","Obtain a new job","Keep a job","Complete a training / education program","Complete a medical treatment"))) %>%
  arrange(desc(`Prevented`)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=`Prevented`,y=prop, fill=`Prevented`)) + geom_bar(stat="identity", position = position_dodge()) + 
  geom_text(aes(vjust=-0.5,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.65)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title="Issues with transportation prevented",
    caption = paste0("Responses=",sum(!is.na(master_data$`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`)))) 

master_data %>%
  select(`Prevented`=`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`) %>%
  separate(`Prevented`,into=c("a","b","c","d","e","f","g"), sep = ",") %>%
  filter(!is.na(g)) # should be zero which means all categories heve been counted

# not been able to get to these places #
c <- bind_rows(master_data %>%
  select(`Not able to get to`=`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Selected Choice`),
master_data %>%
  select(`Not able to get to`=`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Other - Text`))%>%
  separate(`Not able to get to`,into=c("a","b","c","d","e","f","g","h","i"), sep = ",") %>%
  gather(key,`Not able to get to`) %>% 
  mutate(`Not able to get to`=ifelse(`Not able to get to`=="Work or a job fair/interview","Work or a job fair / interview",`Not able to get to`)) %>% # fix small inconsistency in options for cleaner bar plot
  mutate(`Not able to get to`=ifelse(`Not able to get to`!="Childcare facility" & `Not able to get to`!="Grocery store or food bank" & 
                                       `Not able to get to`!="Medical appointment" & `Not able to get to`!="Non-medical appointment" &
                                       `Not able to get to`!="Training / school / education" & `Not able to get to`!="Work or a job fair / interview",
                                     "Other",`Not able to get to`)) %>%
  group_by(`Not able to get to`) %>%
  filter(!is.na(`Not able to get to`)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/sum(!is.na(master_data$`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Selected Choice`))) %>%
  #mutate(`Not able to get to`=factor(`Prevented`,levels = c("Work increased hours at a job","Obtain a new job","Keep a job","Complete a training / education program","Complete a medical treatment"))) %>%
  arrange(desc(`Not able to get to`)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=`Not able to get to`,y=prop, fill=`Not able to get to`)) + geom_bar(stat="identity", position = position_dodge()) + 
  geom_text(aes(vjust=-0.5,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.65)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title="Unable to get to",
       caption = paste0("Responses=",sum(!is.na(master_data$`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Selected Choice`))))

b+c + plot_layout(nrow=1)

ggsave("prevented.png", width = 300, height = 75*1.25,units = "mm", dpi = 1000, 
       path="images")

bind_rows(master_data %>%
            select(`Not able to get to`=`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Selected Choice`),
          master_data %>%
            select(`Not able to get to`=`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Other - Text`))%>%
  separate(`Not able to get to`,into=c("a","b","c","d","e","f","g","h","i"), sep = ",") %>%
  filter(!is.na(i)) # should be zero which means all categories heve been counted

  #### Usage ####
# see code "uber_rides.R" and "bus_rides.R" 

