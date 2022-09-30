library(tidyverse)
library(readxl)
library(lubridate)

master_data <- read_excel("data/master_data_v5.xlsx", sheet = "all_enrolees") # 246

baseline_survey_df <- read_csv("data/surveys/CNPP - Baseline Survey_09.06.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="True")%>% mutate(`Start Date`=ymd_hms(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month")) 

baseline_survey_df_eng <- baseline_survey_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="English")%>%
  mutate(ph_no=sapply(str_extract_all(`Please enter your phone number, if you have one.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) %>%# actual phone numbers
  filter(!str_detect(`Please enter your name.`,"test")) %>% # remove test responses
  select(ph_no, org_name=`Please select your community organization.`,TSI1="In the past 30 days, how often were you not able to leave the house when you wanted to because of a problem with transportation?",
         TSI2="In the past 30 days, how often did problems with transportation affect your relationships with others?",
         TSI3="In the past 30 days, how often did you feel bad because you did not have the transportation you needed?",
         `In the last 30 days, have issues with transportation prevented you from achieving any of the following?`:`Which of the following best describes you?`)

baseline_survey_df_spn <- baseline_survey_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="Spanish/Español")%>%
  mutate(ph_no=sapply(str_extract_all(`Por favor escriba su número de teléfono si tiene uno.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) %>%
  filter(!str_detect(`Por favor escriba su nombre.`,"test")) 

spanish_translations_1 <- read_excel("spanish translations.xlsx", sheet = "Sheet1")

baseline_survey_df_spn %>%
  select(TSI1="En los últimos 30 días, ¿con que frecuencia no pudo salir de casa cuando quería debido a un problema de transporte? *",
         TSI2="En los últimos 30 días, ¿con que frecuencia los problemas con el transporte afectaron sus relaciones con los demás?  *",
         TSI3="En los últimos 30 días, ¿con que frecuencia se sintió mal porque no tenía el transporte que necesitaba? *",
         stress_q=`En los últimos 30 días, mi capacidad para viajar ha sido…`,
         gender_q=`Por favor indique su género. - Selected Choice`,
         `Please indicate your age.`=`Por favor indique su edad.`,
         `Was your total household income in the past 12 months?`=`¿Cual fue su ingreso familiar total en los últimos 12 meses?`) %>%
  left_join(spanish_translations_1,by=c("TSI1"="spanish")) %>% select(-TSI1) %>% rename(TSI1=english) %>%
  left_join(spanish_translations_1,by=c("TSI2"="spanish")) %>% select(-TSI2) %>% rename(TSI2=english) %>%
  left_join(spanish_translations_1,by=c("TSI3"="spanish")) %>% select(-TSI3) %>% rename(TSI3=english) %>%
  left_join(spanish_translations_1,by=c("stress_q"="spanish")) %>% select(-stress_q) %>% rename(`In the past 30 days, my ability to travel around has been...`=english)%>%
  left_join(spanish_translations_1,by=c("gender_q"="spanish")) %>% select(-gender_q) %>% rename(`Please indicate your gender.`=english) %>%
  mutate(`Please indicate your age.`=ifelse(`Please indicate your age.`=="Mayor de 65","65 or older",`Please indicate your age.`)) # add translation for less than 18 - no responses yet
  

# need to convert the rest of the columns to english

# create a file coparable to master data file so that the code does not have to be changed
master_data <- bind_rows(baseline_survey_df_eng,)




bind_rows(baseline_survey_df_eng %>%
  select(ph_no,mnth,TSI1,TSI2,TSI3)%>%
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
  baseline_survey_df_spn %>%
  select(ph_no,mnth,TSI1,TSI2,TSI3)%>%
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
  arrange(ph_no,mnth) %>% group_by(ph_no) %>% filter(row_number()==1) %>% ungroup() %>% # each phone number should only have one response in baseline
  mutate(grp="baseline")%>%
  filter(ph_no!="0000000000") # doesn't look like phone number