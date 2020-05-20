#Estimating the impact of COVID-19 on future income for those who fall behind in school. 
#COVID-book analysis. 

library(tidyverse)
library(readxl)
library(janitor)
library(readabsmicrodata)
library(readabs)
library(grattan)

#How many ntile groups will we eventually divide up the group into? 
n_groupings = 10


#Data on earnings and hours from the ABS
abs_data <- read_abs( 6302.0) 

#This is a script using jonathan nolan's readabsmicrodata package on github - it simply imports the SIH and renames the variables to something more appropriate. 

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

#What is the ratio of each decile's earnings to average earnings? 

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

#What are actual earnings (slightly different figures to the SIH data, but we assume a similar ratio. )

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

#helper function
substringer <- function(x) {substring(x,3)}

#How many students are there in Australia? 

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
  #Since we don't have 2020 data - we can calculate forward using growth from 2018. 
  mutate(growth = n/lag(n)) %>%
  ungroup() %>% 
  filter(year == 2019) %>% 
  mutate(state = tolower(state),
         n = round(n*growth),
         year = 2020) %>% 
  select(-year) 

#Join together earnings with student numbers to find the base dataset. 

student_numbers%>% 
  full_join(abs_earnings) %>% 
  write_csv("shiny/data.csv")



#Method 2 - converting pisa to GDP. Not used. 

pisa_maths_aust <- 494
pisa_math_std_dev <- 93
one_year_of_learning <- 30

low_growth_estimate  <- 0.0001	
high_growth_estimate <- 0.0002

aust_gdp <- 1814535000.0

total_pisa_points_lost_aust <- one_year_of_learning*2/12

total_pisa_points_lost_aust * low_growth_estimate * aust_gdp *37
