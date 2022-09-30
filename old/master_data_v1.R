library(tidyverse)
library(readxl)
library(lubridate)

baseline_survey_df <- read_csv("data/surveys/CNPP - Baseline Survey_09.06.2022.csv", skip = 1) %>% slice(-1) %>%
  filter(Finished=="True")%>% mutate(`Start Date`=ymd_hms(`Start Date`)) %>%
  mutate(mnth=floor_date(`Start Date`, unit = "month")) 

baseline_survey_df_eng <- baseline_survey_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="English")%>%
  mutate(ph_no=sapply(str_extract_all(`Please enter your phone number, if you have one.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) %>%# actual phone numbers
  filter(!str_detect(`Please enter your name.`,"test")) %>% # remove test responses
  select(ph_no, mnth,org_name=`Please select your community organization.`,TSI1="In the past 30 days, how often were you not able to leave the house when you wanted to because of a problem with transportation?",
         TSI2="In the past 30 days, how often did problems with transportation affect your relationships with others?",
         TSI3="In the past 30 days, how often did you feel bad because you did not have the transportation you needed?",
         `In the last 30 days, have issues with transportation prevented you from achieving any of the following?`:`Which of the following best describes you?`)

baseline_survey_df_spn <- baseline_survey_df %>%
  filter(`Would you like to take this survey in English or Spanish?`=="Spanish/Español")%>%
  mutate(ph_no=sapply(str_extract_all(`Por favor escriba su número de teléfono si tiene uno.`,"[0-9]+"),function(x) paste(x,collapse=""))) %>% filter(nchar(ph_no)==10) %>%
  filter(!str_detect(`Por favor escriba su nombre.`,"test")) %>%
  select(ph_no,mnth,org_name=`Por favor seleccione su organización comunitaria`,
         `En los últimos 30 días, ¿con que frecuencia no pudo salir de casa cuando quería debido a un problema de transporte? *`:`¿Cuál de las siguientes respuestas lo/la describe mejor?`) %>%
  select(-`Si esta dispuesto, ¿puede decirnos qué dificultades (si hay alguna) tiene con el transporte y el desplazamiento?`,-`Por favor indique su género. - Otro - Text`) # remove two extra columns only present in spanish

spanish_translations_1 <- read_excel("spanish translations.xlsx", sheet = "Sheet1")
spanish_translations_2 <- read_excel("spanish translations.xlsx", sheet = "Sheet2")
spanish_translations_3 <- read_excel("spanish translations.xlsx", sheet = "Sheet3")

baseline_survey_df_spn2 <- baseline_survey_df_spn %>%
  rename(TSI1="En los últimos 30 días, ¿con que frecuencia no pudo salir de casa cuando quería debido a un problema de transporte? *",
         TSI2="En los últimos 30 días, ¿con que frecuencia los problemas con el transporte afectaron sus relaciones con los demás?  *",
         TSI3="En los últimos 30 días, ¿con que frecuencia se sintió mal porque no tenía el transporte que necesitaba? *",
         stress_q=`En los últimos 30 días, mi capacidad para viajar ha sido…`,
         gender_q=`Por favor indique su género. - Selected Choice`,
         `Please indicate your age.`=`Por favor indique su edad.`,
         `Was your total household income in the past 12 months?`=`¿Cual fue su ingreso familiar total en los últimos 12 meses?`,
         ethnicity_q=`¿Cuál de las siguientes respuestas lo/la describe mejor?`) %>%
  left_join(spanish_translations_1,by=c("TSI1"="spanish")) %>% select(-TSI1) %>% rename(TSI1=english) %>%
  left_join(spanish_translations_1,by=c("TSI2"="spanish")) %>% select(-TSI2) %>% rename(TSI2=english) %>%
  left_join(spanish_translations_1,by=c("TSI3"="spanish")) %>% select(-TSI3) %>% rename(TSI3=english) %>%
  left_join(spanish_translations_1,by=c("stress_q"="spanish")) %>% select(-stress_q) %>% rename(`In the past 30 days, my ability to travel around has been...`=english)%>%
  left_join(spanish_translations_1,by=c("gender_q"="spanish")) %>% select(-gender_q) %>% rename(`Please indicate your gender.`=english) %>%
  left_join(spanish_translations_1,by=c("ethnicity_q"="spanish")) %>% select(-ethnicity_q) %>% rename(`Which of the following best describes you?`=english) %>%
  mutate(`Please indicate your age.`=ifelse(`Please indicate your age.`=="Mayor de 65","65 or older",`Please indicate your age.`)) %>% # add translation for less than 18 - no responses yet
  mutate(`Was your total household income in the past 12 months?` = ifelse(`Was your total household income in the past 12 months?`=="Menos de $10,000","Less than $10,000",`Was your total household income in the past 12 months?`)) %>%
  rename(resp=`En los últimos 30 días, ¿problemas con el transporte le han impedido lograr alguno de los siguientes? Marque todo lo que aplique.`) %>%
  mutate(resp=str_replace(resp,spanish_translations_2$spanish[1],spanish_translations_2$english[1]),
         resp=str_replace(resp,spanish_translations_2$spanish[2],spanish_translations_2$english[2]),
         resp=str_replace(resp,spanish_translations_2$spanish[3],spanish_translations_2$english[3]),
         resp=str_replace(resp,spanish_translations_2$spanish[4],spanish_translations_2$english[4]),
         resp=str_replace(resp,spanish_translations_2$spanish[5],spanish_translations_2$english[5]),
         resp=str_replace(resp,spanish_translations_2$spanish[6],spanish_translations_2$english[6]),
         resp=str_replace(resp,spanish_translations_2$spanish[7],spanish_translations_2$english[7]),
         resp=str_replace(resp,spanish_translations_2$spanish[8],spanish_translations_2$english[8]),
         resp=str_replace(resp,spanish_translations_2$spanish[9],spanish_translations_2$english[9]),
         resp=str_replace(resp,spanish_translations_2$spanish[10],spanish_translations_2$english[10])) %>%rename(`In the last 30 days, have issues with transportation prevented you from achieving any of the following?`=resp) %>%
  rename(resp2=`En los últimos 30 días, ¿NO ha podido ir a alguno de los siguientes lugares debido a un problema de transporte? Marque todo lo que aplique. - Selected Choice`) %>%
  mutate(resp2=str_replace(resp2,spanish_translations_3$spanish[1],spanish_translations_3$english[1]),
         resp2=str_replace(resp2,spanish_translations_3$spanish[2],spanish_translations_3$english[2]),
         resp2=str_replace(resp2,spanish_translations_3$spanish[3],spanish_translations_3$english[3]),
         resp2=str_replace(resp2,spanish_translations_3$spanish[4],spanish_translations_3$english[4]),
         resp2=str_replace(resp2,spanish_translations_3$spanish[5],spanish_translations_3$english[5])) %>%
  rename(`In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Selected Choice`=resp2,
         `In the last 30 days, have you NOT been able to get to any of the places below, due to a transportation issue? - Other - Text`=`En los últimos 30 días, ¿NO ha podido ir a alguno de los siguientes lugares debido a un problema de transporte? Marque todo lo que aplique. - Otro - Text`) # no responses so simply renaming

# need to convert the rest of the columns to english

# create a file coparable to master data file so that the code does not have to be changed
master_data <- bind_rows(baseline_survey_df_eng,baseline_survey_df_spn2) %>% distinct(ph_no,.keep_all = T)

rm(baseline_survey_df,baseline_survey_df_eng,baseline_survey_df_spn,baseline_survey_df_spn2,
   spanish_translations_1,spanish_translations_2,spanish_translations_3)

write_rds(master_data,"data/master_data.Rds")