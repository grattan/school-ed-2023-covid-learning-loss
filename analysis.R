library(tidyverse)
library(readxl)
library(janitor)
library(readabsmicrodata)
library(readabs)
library(grattan)

n_groupings = 10

abs_data <- read_abs( 6302.0) 


sih_hh <- read_abs_microdata(survey = "sih", 
                             file = "household", 
                             refyear = "2017",
                             grattandata = TRUE,
                             create_html_dictionary = FALSE) 

sih_p <- read_abs_microdata(survey = "sih", 
                            file = "person", 
                            refyear = "2017",
                            grattandata = TRUE,
                            create_html_dictionary = FALSE) %>% 
  add_ages()


sih_earnings_ratios <- sih_p %>% 
  left_join(sih_hh %>% select(id_hh,state)) %>% 
  filter(ftptstat_2017 != "Not applicable",
         iwsucp_2017>0) %>% 
  group_by(state) %>% 
  mutate(n_tile = weighted_ntile(iwsucp_2017,
                                 weights = weight_person, 
                                 n=n_groupings),
         earnings_av = weighted.mean(iwsucp_2017,
                                     weight_person)) %>% 
  group_by(state, n_tile) %>%
  summarise(earnings_av_ntile = weighted.mean(iwsucp_2017,
                                              weight_person),
            earnings_av       = mean(earnings_av)) %>% 
  mutate(ratio_to_av = earnings_av_ntile / earnings_av) %>% 
  select(state,
         n_tile,
         ratio_to_av) 


abs_earnings <- abs_data %>%  
  filter(str_detect(series,"Earnings; Persons; Total earnings ;"),
         series_type == "Trend") %>%
  separate(series,c("earning", "gender","earnings_type","state","sector"),sep = ";") %>% 
  mutate(state_short = case_when(str_detect(state, "Victoria")   ~ "vic",
                                 str_detect(state, "New South")  ~ "nsw",
                                 str_detect(state, "Queensland") ~ "qld",
                                 str_detect(state, "South Aus")  ~ "sa",
                                 str_detect(state, "Northern")   ~ "nt",
                                 str_detect(state, "Australian Cap") ~ "act",
                                 str_detect(state, "Western")    ~ "wa",
                                 str_detect(state, "Tas")        ~ "tas",
                                 str_detect(state, "Australian Cap") ~ "act"),
         state = trimws(state)
                           ) %>% 
  filter(!is.na(state_short),
         sector == "",
         date == "2019-11-15") %>% 
  merge(tibble(year = seq(2019,2100))) %>% 
  group_by(state) %>% 
  mutate(year_index = year-first(year)) %>% 
  ungroup() %>% 
  select(state,
         state_short,
         year,
         year_index,
         wage_original_2020 = value )%>% 
  merge(tibble(n_tile = seq(1,n_groupings))) %>% 
  left_join(sih_earnings_ratios) %>% 
  mutate(state = state_short) %>%
  select(-state_short)

substringer <- function(x) {substring(x,3)}

student_numbers <- read_excel("data/table 42b number of full-time and part-time students, 2006-2019.xls",
                              sheet = 3,
                              skip = 4) %>% 
  clean_names() %>% 
  mutate(age = as.numeric(parse_number(age))) %>% 
  mutate_if(is.character,
            substringer) %>% 
  filter(!is.na(age)) %>% 
  group_by(state_territory,year,age) %>% 
  summarise(n = sum(all_full_time_and_part_time_student_count)) %>% 
  rename(state = state_territory) %>% 
  group_by(state,age) %>% 
  arrange(year) %>% 
  mutate(growth = n/lag(n)) %>%
  ungroup() %>% 
  filter(year == 2019) %>% 
  mutate(state = tolower(state),
         n = round(n*growth),
         year = 2020) %>% 
  select(-year) 


student_numbers%>% 
  full_join(abs_earnings) %>% 
  write_csv("shiny/data.csv")



  

