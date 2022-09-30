# this code studies the weekly wurvey responses
# there are only two questions in the weekly survey - where did you use the rides and what would have appened if rides werent available?

library(tidyverse)
library(readxl)
library(lubridate)

weekly_raw <- read_csv("data/surveys/CNPP Weekly Survey_4.7.22_edit.csv")

# cleaning - english
weekly_raw_clean_eng <- weekly_raw %>%
  filter(`Would you like to take the survey in English or Spanish?`=="English") %>%
  filter(Finished=="TRUE") %>% # # remove incomplete surveys
  filter(`Response Type`=="IP Address") # # remove testing responses

# cleaning - spanish
weekly_raw_clean_spn <- weekly_raw %>%
  filter(str_detect(`Would you like to take the survey in English or Spanish?`,"Spanish")) %>%
  filter(Finished=="TRUE") %>% # # remove incomplete surveys
  filter(`Response Type`=="IP Address") # # remove testing responses


# delete repeated responses


# delete responses that cannot be matched to enrolments

# combine english and 


# total weekly survey responses
bind_rows(select(weekly_raw_clean_eng,`Recorded Date`),
          select(weekly_raw_clean_spn,`Recorded Date`)) %>% group_by(wk_no=floor_date(mdy_hm(`Recorded Date`),"week")) %>%
  summarise(obs=n()) %>%
  ggplot(aes(x=wk_no,y=obs)) + geom_line()

# rides used where? - standard responses

tot_resp <- sum(!is.na(weekly_raw_clean_eng$`Over the last 7 days, did you use your Uber rides to get to any of the following? Check all that apply - Selected Choice`)) +
sum(!is.na(weekly_raw_clean_spn$`Durante los últimos 7 días, ¿uso usted sus viajes en Uber para ir a algunos de los siguientes lugares? Marque todo lo que aplique - Selected Choice`))

recent_date <- weekly_raw$`Recorded Date` %>% str_extract("[0-9]+/[0-9]+/[0-9]+") %>% mdy() %>% max() %>% format("%b %d, %Y")

a <- bind_rows(
  weekly_raw_clean_eng %>% select(uber_rides_use_1=22) %>% # "Over the last 7 days, did you use your Uber rides to get to any of the following? Check all that apply - Selected Choice"
  separate(uber_rides_use_1, sep=",", into=c("a","b","c","d","e")) %>%
  gather(key,uber_rides_use_1) ,
  weekly_raw_clean_spn %>% select(uber_rides_use_1=`Durante los últimos 7 días, ¿uso usted sus viajes en Uber para ir a algunos de los siguientes lugares? Marque todo lo que aplique - Selected Choice`) %>%
    separate(uber_rides_use_1,into=c("a","b","c","d","e","f","g","h","i"), sep = ",") %>%
    gather(key,uber_rides_use_1) %>%
    mutate(uber_rides_use_1=ifelse(uber_rides_use_1=="Cita medica", "Medical appointment",
                                 ifelse(uber_rides_use_1=="Tienda de comestibles o banco de alimentos","Grocery store or food bank",
                                        ifelse(uber_rides_use_1=="Otro","Other",
                                               ifelse(uber_rides_use_1=="Trabajo o una feria de empleo/entrevista","Work or a job fair / interview",
                                                      ifelse(uber_rides_use_1=="Entrenamiento/escuela/educación","Training / school / education",
                                                             ifelse(uber_rides_use_1=="Lugar de cuidado de niños","Childcare facility",
                                                                    ifelse(uber_rides_use_1=="Cita no medica","Non-medical appointment",NA))))))))
  ) %>%
  group_by(uber_rides_use_1) %>%
  filter(!is.na(uber_rides_use_1)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/tot_resp) %>%
  arrange(desc(uber_rides_use_1)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=uber_rides_use_1,y=prop, fill=uber_rides_use_1)) + geom_bar(stat="identity", position = position_dodge()) + 
  geom_text(aes(vjust=-0.5,label = scales::percent(prop, accuracy = 1)),size = 3)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title="Used Uber rides to get to",
       caption = paste0("Responses=",tot_resp,"\n","Most recent survey=",recent_date))


# what would have happened if rides werent available?
tot_resp <- sum(!is.na(weekly_raw_clean_eng$`Thinking about the places you used the free Uber rides to get to in the past 7 days, if the free Uber rides weren't available, which of the following would apply to you? (Select 1)`)) +
  sum(!is.na(weekly_raw_clean_spn$`Pensando en los lugares a los que usó los viajes gratuitos para transporte en los últimos 7 días, si estos viajes gratuitos no estaban disponibles, ¿cuál de los siguientes describe mejor su situación? (Seleccione 1)*`))


b <- bind_rows(
  weekly_raw_clean_eng %>% select(rides_unavailable=24), # Thinking about the places you used the free Uber rides to get to in the past 7 days, if the free Uber rides weren't available, which of the following would apply to you? (Select 1)
  weekly_raw_clean_spn %>%
    select(rides_unavailable=`Pensando en los lugares a los que usó los viajes gratuitos para transporte en los últimos 7 días, si estos viajes gratuitos no estaban disponibles, ¿cuál de los siguientes describe mejor su situación? (Seleccione 1)*`) %>%
    mutate(rides_unavailable=ifelse(rides_unavailable=="NO hubiera podido llegar a estos lugares","I would NOT have been able to get to most of these places",
                                    ifelse(rides_unavailable=="Habría llegado a estos lugares de otra manera (como el autobús, viajar con alguien, caminar, andar en bicicleta)","I would have gotten to most of these places another way (such as the bus, ride from someone, walk, bike)",
                                           ifelse(rides_unavailable=="Hubiera pagado el precio complete para llegar a estos lugares usando Uber, Lyft, o un taxi.","I would have paid full price to get to  using Uber, Lyft or a taxi",NA))))
  
  )%>% # ,rides_use_2=23
  group_by(rides_unavailable) %>%
  filter(!is.na(rides_unavailable)) %>%
  summarise(obs=n()) %>% mutate(prop=obs/tot_resp) %>%
  mutate(rides_unavailable=ifelse(rides_unavailable=="I would have gotten to most of these places another way (such as the bus, ride from someone, walk, bike)",
                                  "I would have gotten to most of these places another way",rides_unavailable)) %>% # shorten the long option
  mutate(rides_unavailable=factor(rides_unavailable,levels=c("I would NOT have been able to get to most of these places",
                                                             "I would have gotten to most of these places another way",
                                                             "I would have paid full price to get to  using Uber, Lyft or a taxi"
                                                                 ))) %>%
  arrange(desc(rides_unavailable)) %>% mutate(cum_prop=cumsum(prop)) %>%
  ggplot(aes(x=1,y=prop, fill=rides_unavailable)) + geom_col() +  # fill=str_wrap(rides_unavailable,10) almost works!
  geom_text(aes(x=1,y=cum_prop-prop/2,label = scales::percent(prop, accuracy = 1)),size = 3) +
  theme_minimal() + theme(axis.title = element_blank(), axis.text = element_blank(), legend.title = element_blank()) +
  labs(title="If Uber rides were unavailable",
       caption = paste0("Responses=",tot_resp,"\n","Most recent survey=",recent_date))+ scale_fill_discrete(labels = function(x) str_wrap(x, width = 30))

library(patchwork)
a+b

ggsave("weekly_plot.png", width = 300*0.9, height = 150*0.9,units = "mm", dpi = 500, 
       path="images")