library(COVID19)
library(rvest)
library(tidyverse)
x <- covid19(c("US"), level = 2)
glimpse(x)
min(x$date)
### Sanity Check Data No.1 (Retrieved on Nov. 06, 2011)
x %>% 
  select(administrative_area_level_2) %>% unique

sort(unique(x$date))
dim(x) 

covid19_df <- x %>% 
  ungroup() %>%
  select(date, 
         vaccines, 
         people_vaccinated,
         people_fully_vaccinated,
         tests, 
         confirmed, 
         recovered, 
         deaths, 
         administrative_area_level_2, 
         gatherings_restrictions, 
         stay_home_restrictions, 
         workplace_closing, 
         school_closing)

# lag the date by sys.date

covid19_df <- covid19_df %>%
  filter(date < Sys.Date() - 7)

# Retrieve party info
html_obj <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_presidential_election_results_by_state")
html_obj


data_table <- html_nodes(html_obj, 
                         css = "ul+ .wikitable td:nth-child(60) , ul+ .wikitable tr+ tr td+ th a , ul+ .wikitable th:nth-child(66) a")

data_table_text <- html_text(data_table)

####### Political Variable Data (vector) to tibble
data_table_text <- data_table_text[-1]
data_table_text

political_state <- tibble(data_table_text) %>%
  mutate(case = rep(1:(length(data_table_text)/2), each = 2)) %>%
  mutate(key = case_when(row_number() %% 2 == 0 ~ "state", row_number() %% 2 != 0 ~ "political factor")) %>%
  pivot_wider(id_cols = case, names_from = key, values_from = data_table_text)

names(political_state) <- c("index", "party", "state")

political_state <- political_state %>% 
  mutate(party = str_replace_all(party,"\n", "")) %>%
  select(state, party)


# Merge data

covid19_data <- covid19_df %>%
  select(date,tests,confirmed,deaths, vaccines,people_vaccinated,people_fully_vaccinated,recovered, administrative_area_level_2) 

names(covid19_data) <- c("date", "tests", "confirmed","deaths", "vaccines","vaccined_people","fully_vaccined_people","recovered", "state" )

unique(covid19_data$state)
unique(political_state$state)
setdiff(unique(covid19_data$state), unique(political_state$state))
setdiff(unique(political_state$state), unique(covid19_data$state))

political_state <- political_state %>% 
  mutate(state = case_when(state == "D.C." ~ "District of Columbia",
                           TRUE   ~ state))

setdiff(unique(political_state$state), unique(covid19_data$state))

# Merge data and remove

covid19_data <- covid19_data %>% left_join(political_state, by = "state")


tests_data <- covid19_data %>% 
  select(date,tests, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = tests,
              values_fill = NA) %>%
  arrange(date)


confirmed_data <- covid19_data %>% 
  select(date,confirmed, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = confirmed,
              values_fill = NA) %>%
  arrange(date)

deaths_data <- covid19_data %>% 
  select(date,deaths, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = deaths,
              values_fill = NA) %>%
  arrange(date)

vaccines_data <- covid19_data %>% 
  select(date,vaccines, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = vaccines,
              values_fill = NA) %>%
  arrange(date)

vaccined_people_data <- covid19_data %>% 
  select(date,vaccined_people, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = vaccined_people ,
              values_fill = NA) %>%
  arrange(date)

fully_vaccined_people_data <- covid19_data %>% 
  select(date,fully_vaccined_people, state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = fully_vaccined_people ,
              values_fill = NA) %>%
  arrange(date)

recoverd_data <- covid19_data %>% 
  select(date,recovered , state)%>%
  pivot_wider(id_cols = c(date, state),
              names_from = state, 
              values_from = recovered ,
              values_fill = NA) %>%
  arrange(date)


daily_increment <- function(data = vaccines_data){
  output <- tibble(cbind(data$date[2:dim(data)[1]],data[2:dim(data)[1],2:dim(data)[2]] - 
                    data[1:(dim(data)[1]-1),2:dim(data)[2]]))
  names(output)[1] = "date"
  return(output)
}

tests_daily <- daily_increment(tests_data)
confirmed_daily <- daily_increment(confirmed_data)
deaths_daily <- daily_increment(deaths_data)
vaccines_daily <- daily_increment(vaccines_data)
vaccined_people_daily <- daily_increment(vaccined_people_data)
fully_vaccined_people_daily <- daily_increment(fully_vaccined_people_data)
recovered_daily <- daily_increment(recoverd_data)


### Above as 7 data frames to analyze daily increased cases in each variable for Dashboard info

R_s <- political_state %>%
  filter(party == "R") %>%
  select(state)

D_s <- political_state %>%
  filter(party == "D") %>%
  select(state)

split_party <- function(data = tests_data){

  
  R_data <- data[R_s$state] %>%
    replace(is.na(.), 0) %>%
    mutate(R = rowSums(.)) %>%
    select(R)
  
  D_data <- data[D_s$state] %>%
    replace(is.na(.), 0) %>%
    mutate(D = rowSums(.)) %>%
    select(D)
  
  return(tibble(cbind(date = data$date, R_data, D_data)))
  
  
}

split_party(tests_data)


### The 8th data frame include political factor from political_state data frame above
### Please included appropriate combination of those variables such as gathering_restrictions, school_restrictions, tests, people_fully_vaccinated 
### from the data frame named covid19_df. 





 
# # population data

url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
file_names <- c("time_series_covid19_deaths_US.csv")
popdf <- read_csv(paste(url_in,file_names, sep = "")) %>%
  select(Admin2, Province_State, Population)
state_pop <- popdf %>%
  group_by(Province_State) %>%
  mutate(State_Population = sum(Population)) %>%
  select(Province_State, State_Population) %>%
  unique()
  
# Check
setdiff(state_pop$Province_State, colnames(tests_daily))
setdiff( colnames(tests_daily),state_pop$Province_State)



pop_pct <- function(data = tests_data){
  pop_pct_data <- data %>%
                   replace(is.na(.), 0)
  
  for (i in colnames(pop_pct_data)[2:length(colnames(pop_pct_data))] ){
    pop_pct_data[i] <- pop_pct_data[i]/state_pop[which(state_pop$Province_State == i),]$State_Population
  }
  return(pop_pct_data)
}

tests_cumu_pct <- pop_pct(tests_data)
confirmed_cumu_pct <- pop_pct(confirmed_data)
deaths_cumu_pct <- pop_pct(deaths_data)
vaccines_cumu_pct <- pop_pct(vaccines_data)
vaccined_people_cumu_pct <- pop_pct(vaccined_people_data)
fully_vaccined_people_cumu_pct <- pop_pct(fully_vaccined_people_data)
recovered_cumu_pct <- pop_pct(recoverd_data)


# Dashboard - Plots part1: Above 9th data frame for Population % in each state for each variable. 
# i.e., for Variable confirmed cases in State California, 12.7% of total California population in 2021-11-16 has been confirmed.  



R_total_pop <- state_pop %>%
  filter(Province_State %in% R_s$state)

D_total_pop <- state_pop %>%
  filter(Province_State %in% D_s$state)

state_pop <- state_pop %>%
  ungroup() %>%
  add_row(Province_State = "R",State_Population = sum(R_total_pop$State_Population)) %>%
  add_row(Province_State = "D",State_Population = sum(D_total_pop$State_Population))



party_tests_cumu_pct <- pop_pct(split_party(tests_data))
party_confirmed_cumu_pct <- pop_pct(split_party(confirmed_data))
party_deaths_cumu_pct <- pop_pct(split_party(deaths_data))
party_vaccines_cumu_pct <- pop_pct(split_party(vaccines_data))
party_vaccined_people_cumu_pct <- pop_pct(split_party(vaccined_people_data))
party_fully_vaccined_people_cumu_pct <- pop_pct(split_party(fully_vaccined_people_data))
party_recovered_cumu_pct <- pop_pct(split_party(recoverd_data))

# Dashboard Plots - part2: Above 10th data frame for Population % in each party for each variable. 
# i.e., for Variable tests, Republican Party people averagely had 1.56 times of tests on 2021-11-16. 

