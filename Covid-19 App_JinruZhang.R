library(COVID19)
library(rvest)
library(tidyverse)
library(reshape2)
library(shiny)
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
  data <- data %>%
    replace(is.na(.), 0)
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

total_us_pop <- sum(state_pop$State_Population)
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
# Tab 1:


# Tab 2:
# State data analysis:
# 1: Daily data:
#     tests_daily 
#     confirmed_daily
#     deaths_daily
#     vaccined_people_daily

# 2: Cumulative data:
#     tests_data 
#     confirmed_data
#     deaths_data
#     vaccined_people_data

# 3: Cumulative Percentage data:
#     tests_cumu_pct 
#     confirmed_cumu_pct
#     deaths_cumu_pct
#     vaccined_people_cumu_pct


# Tab 3:
# Party data analysis:
# 1: Daily data:
#     split_party(tests_daily) 
#     split_party(confirmed_daily)
#     split_party(deaths_daily)
#     split_party(vaccined_people_daily)

# 2: Cumulative data:
#     split_party(tests_data)
#     split_party(confirmed_data)
#     split_party(deaths_data)
#     split_party(vaccined_people_data)

# 3: Cumulative Percentage data:
#     party_tests_cumu_pct
#     party_confirmed_cumu_pct
#     party_deaths_cumu_pct
#     party_vaccined_people_cumu_pct

library(shiny)
library(ggplot2)


data_selection <- function(tab = "US",  types = "Confirmed",  type2 = "Daily", type3 = "Pct"){
  
  if(tab ==  "US"){
    if(types == "Confirmed"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(tibble(date = confirmed_data$date, US = rowSums(confirmed_data %>% replace(is.na(.), 0) %>% select(-date))))
        }else if(type3 == "Pct"){
          return(tibble(date = confirmed_data$date, US = rowSums(confirmed_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(daily_increment(tibble(date = confirmed_data$date, US = rowSums(confirmed_data %>% replace(is.na(.), 0) %>% select(-date)))))
        }else if(type3 == "Pct"){
          return(daily_increment(tibble(date = confirmed_data$date, US = rowSums(confirmed_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop)))
        }
      }
    }else if(types == "Tests"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(tibble(date = tests_data$date, US = rowSums(tests_data %>% replace(is.na(.), 0) %>% select(-date))))
        }else if(type3 == "Pct"){
          return(tibble(date = tests_data$date, US = rowSums(tests_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(daily_increment(tibble(date = tests_data$date, US = rowSums(tests_data %>% replace(is.na(.), 0) %>% select(-date)))))
        }else if(type3 == "Pct"){
          return(daily_increment(tibble(date = tests_data$date, US = rowSums(tests_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop)))
        }
      }
    }else if(types == "Deaths"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(tibble(date = deaths_data$date, US = rowSums(deaths_data %>% replace(is.na(.), 0) %>% select(-date))))
        }else if(type3 == "Pct"){
          return(tibble(date = deaths_data$date, US = rowSums(deaths_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(daily_increment(tibble(date = deaths_data$date, US = rowSums(deaths_data %>% replace(is.na(.), 0) %>% select(-date)))))
        }else if(type3 == "Pct"){
          return(daily_increment(tibble(date = deaths_data$date, US = rowSums(deaths_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop)))
        }
      }
    }else if(types == "Vaccined_people"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(tibble(date = vaccined_people_data$date, US = rowSums(vaccined_people_data %>% replace(is.na(.), 0) %>% select(-date))))
        }else if(type3 == "Pct"){
          return(tibble(date = vaccined_people_data$date, US = rowSums(vaccined_people_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(daily_increment(tibble(date = vaccined_people_data$date, US = rowSums(vaccined_people_data %>% replace(is.na(.), 0) %>% select(-date)))))
        }else if(type3 == "Pct"){
          return(daily_increment(tibble(date = vaccined_people_data$date, US = rowSums(vaccined_people_data %>% replace(is.na(.), 0) %>% select(-date))/total_us_pop)))
        }
      }
    }
  }else if (tab ==  "State"){
    if(types == "Confirmed"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(confirmed_data)
        }else if(type3 == "Pct"){
          return(pop_pct(confirmed_data))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(confirmed_daily)
        }else if(type3 == "Pct"){
          return(pop_pct(confirmed_daily))
        }
      }
    }else if(types == "Tests"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(tests_data)
        } else if(type3 == "Pct"){
          return(pop_pct(tests_data))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(tests_daily)
        } else if(type3 == "Pct"){
          return(pop_pct(tests_daily))
        }
      }
    }else if(types == "Deaths"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(deaths_data)
        } else if(type3 == "Pct"){
          return(pop_pct(deaths_data))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(deaths_daily)
        } else if(type3 == "Pct"){
          return(pop_pct(deaths_daily))
        }
      }
    }else if(types == "Vaccined_people"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(vaccined_people_data)
        } else if(type3 == "Pct"){
          return(pop_pct(vaccined_people_data))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(vaccined_people_daily)
        } else if(type3 == "Pct"){
          return(pop_pct(vaccined_people_daily))
        }
      }
    }
  } else if (tab ==  "Party"){
    if(types == "Confirmed"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(split_party(confirmed_data))
        }else if(type3 == "Pct"){
          return(pop_pct(split_party(confirmed_data)))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(split_party(confirmed_daily))
        }else if(type3 == "Pct"){
          return(pop_pct(split_party(confirmed_daily)))
        }
      }
    }else if(types == "Tests"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(split_party(tests_data))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(tests_data)))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(split_party(tests_daily))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(tests_daily)))
        }
      }
    }else if(types == "Deaths"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(split_party(deaths_data))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(deaths_data)))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(split_party(deaths_daily))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(deaths_daily)))
        }
      }
    }else if(types == "Vaccined_people"){
      if( type2 == "Cumulative"){
        if (type3 == "Number"){
          return(split_party(vaccined_people_data))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(vaccined_people_data)))
        }
      }else if( type2 == "Daily") {
        if (type3 == "Number"){
          return(split_party(vaccined_people_daily))
        } else if(type3 == "Pct"){
          return(pop_pct(split_party(vaccined_people_daily)))
        }
      }
    }
  }
}

tail(data_selection(tab = "US",  types = "Confirmed",  type2 = "Daily", type3 = "Pct"))
tail(data_selection(tab = "State",  types = "Tests",  type2 = "Cumulative", type3 = "Number"))
tail(data_selection(tab = "Party",  types = "Deaths",  type2 = "Cumulative", type3 = "Pct"))

# Analysis Tests, Confirmed, Deaths, Vaccined_people single select
# 
library(TTR)
library(scales)
library(forecast)
library(lubridate)
types <- c("Tests", "Confirmed", "Deaths", "Vaccined_people")
states <- sort(colnames(tests_daily)[-1])
type2 <- c("Daily", "Cumulative")
type3 <- c("Number", "Pct")
parties <- c("R", "D")
model <- c("SMA", "EMA", "WMA","ALMA","HMA")
ui <- fluidPage(
  titlePanel("COVID 19 Tracking and Prediction"),
  tabsetPanel(type = "tabs",
              tabPanel("US", 
                       sidebarLayout(
                         sidebarPanel(
                           fillPage(
                             radioButtons("t1type", "Choose a variable to Analysis:", choices = types, selected = "Confirmed"),
                             radioButtons("t1type2", "Analysis Type:", choices = type2),
                             radioButtons("t1type3", "Total Number or Percentage:", choices = type3),
                             checkboxInput("t1smooth", "Smooth by Week and predict?"),
                             radioButtons("t1model", "Moving Average Model:", choices = model),
                             sliderInput("t1sw", "Number of weeks to smooth", value = 1, min = 1, max = 8),
                             sliderInput("t1pred", "Number of days to predict", value = 30, min = 1, max = 100)
                             
                           )),
                         mainPanel(plotOutput("t1plot1"))
                       )
              ),
              tabPanel("State", 
                       sidebarLayout(
                         sidebarPanel(
                           fillPage(
                             radioButtons("t2type", "Choose a variable to Analysis:", choices = types, selected = "Confirmed"),
                             selectInput("t2state", "State:", choices = states, selected = "District of Columbia",multiple = TRUE),
                             radioButtons("t2type2", "Analysis Type:", choices = type2),
                             radioButtons("t2type3", "Total Number or Percentage:", choices = type3),
                             checkboxInput("t2smooth", "Smooth by Week and predict?"),
                             radioButtons("t2model", "Moving Average Model:", choices = model),
                             sliderInput("t2sw", "Number of weeks to smooth", value = 1, min = 1, max = 8),
                             sliderInput("t2pred", "Number of days to predict", value = 30, min = 1, max = 100)
                             
                           )),
                         mainPanel(plotOutput("t2plot1"))
                       )
              ),
              tabPanel("Party", 
                       sidebarLayout(
                         sidebarPanel(
                           fillPage(
                             radioButtons("t3type", "Choose a variable to Analysis:", choices = types, selected = "Confirmed"),
                             checkboxGroupInput("t3party", "Party to Analysis (R: Republican  D: Democratic )", choices = parties, selected = c("R","D")),
                             radioButtons("t3type2", "Analysis Type:", choices = type2),
                             radioButtons("t3type3", "Total Number or Percentage:", choices = type3),
                             checkboxInput("t3smooth", "Smooth by Week and predict?"),
                             radioButtons("t3model", "Moving Average Model:", choices = model),
                             sliderInput("t3sw", "Number of weeks to smooth", value = 1, min = 1, max = 8),
                             sliderInput("t3pred", "Number of days to predict", value = 30, min = 1, max = 100)
                           )),
                         mainPanel(plotOutput("t3plot1"))
                       )
              )
              )

  
)

# 3. Define server logic to manipulate input objects, data, to produce output objects
server <- function(input, output) {
  
  output$t1plot1 <- renderPlot({
    
    data = data_selection(tab = "US",  types = input$t1type,  type2 = input$t1type2, type3 = input$t1type3)
    
    if (input$t1smooth == F ){
      p = ggplot(data = data, aes(x = date, y = US)) + geom_line() + ggtitle("US Covid-19 Analysis")
      if(input$t1type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
    } else {
      
      eval(parse(text = paste("model_data = ", input$t1model, "(data$US,n= 7 * input$t1sw)", sep = "")))
      sm_data = tibble(date = data$date, US = model_data)

      pred = as.data.frame(forecast(model_data, input$t1pred))
      
      pred_df <- tibble( date = as.Date((max(data$date) + 1):(max(data$date) + input$t1pred), origin = "1970-1-1"),
                         pred = pred$`Point Forecast`)
      
      p = ggplot(data = sm_data, aes(x = date, y = US)) + 
        geom_line() + 
        ggtitle("US Covid-19 Analysis with Moving Average Smooth & Prediction") +
        geom_line(data = pred_df, aes(date, pred), color = "Red", linetype = "twodash", size = 1.5)
      
      if(input$t1type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
    }
    
  })
  
  
  output$t2plot1 <- renderPlot({
    
    data2 = data_selection(tab = "State",  types = input$t2type,  type2 = input$t2type2, type3 = input$t2type3)
    # data2 = data_selection(tab = "State",  types = "Deaths",  type2 = "Cumulative", type3 = "Pct")
    data2_d = data2[,which(colnames(data2) %in% input$t2state)]
    # data2_d = data2[,which(colnames(data2) %in% c("Texas", "Alabama"))]
    data2 = cbind(date = data2$date, data2_d)
    
    if (input$t2smooth == F ){
      data2_melt = melt(data2 ,  id.vars = 'date', variable.name = 'state')
      p = ggplot(data = data2_melt, aes(date, value, color = state)) + geom_line() + ggtitle("US States Covid-19 Analysis")
      
      if(input$t2type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
    } else {
    
      pred_data2 <- data2[1:input$t2pred,]
      pred_data2$date =as.Date((max(data2$date) + 1):(max(data2$date) + input$t2pred), origin = "1970-1-1")
      
      for(i in 2:dim(data2)[2]){
        
        eval(parse(text = paste("data2[,i] = ", input$t2model, "(data2[,i],n= 7 * input$t2sw)", sep = "")))
        pred_data2[,i] = as.data.frame(forecast(data2[,i], input$t2pred))$`Point Forecast`
      }
      data2_melt = melt(data2 ,  id.vars = 'date', variable.name = 'state')
      pred_data2_melt = melt(pred_data2 ,  id.vars = 'date', variable.name = 'state')
      p = ggplot(data = data2_melt, aes(date, value, color = state)) + 
        geom_line() + 
        ggtitle("US States Covid-19 Analysis with Moving Average Smooth & Prediction") +
        geom_line(data = pred_data2_melt, aes(date, value, color = state), linetype = "twodash", size = 1.5)
        
      
      if(input$t2type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
      
    }
    
  })
  
  output$t3plot1 <- renderPlot({
    
    data3 = data_selection(tab = "Party",  types = input$t3type,  type2 = input$t3type2, type3 = input$t3type3)
    # data3 = data_selection(tab = "Party",  types = "Deaths",  type2 = "Cumulative", type3 = "Pct")
    data3_d = data3[,which(colnames(data3) %in% input$t3party)] #input$t3party
    data3 = cbind(date = data3$date, data3_d)
    
    if (input$t3smooth == F ){
      data3_melt = melt(data3 ,  id.vars = 'date', variable.name = 'party')
      p = ggplot(data = data3_melt, aes(date, value, color = party)) + geom_line() + ggtitle("US Parties Covid-19 Analysis")
      if(input$t3type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
      
    } else {
      
      pred_data3 <- data3[1:input$t3pred,]
      pred_data3$date =as.Date((max(data3$date) + 1):(max(data3$date) + input$t3pred), origin = "1970-1-1")
      
      
      for(i in 2:dim(data3)[2]){
        eval(parse(text = paste("data3[,i] = ", input$t3model, "(data3[,i],n= 7 * input$t3sw)", sep = "")))
        pred_data3[,i] = as.data.frame(forecast(data3[,i],input$t3pred))$`Point Forecast`
      }
      data3_melt = melt(data3 ,  id.vars = 'date', variable.name = 'party')
      pred_data3_melt = melt(pred_data3 ,  id.vars = 'date', variable.name = 'state')
      p = ggplot(data = data3_melt, aes(date, value, color = party)) + 
        geom_line() + 
        ggtitle("US Parties Covid-19 Analysis with Moving Average Smooth & Prediction") +
        geom_line(data = pred_data3_melt, aes(date, value, color = state), linetype = "twodash", size = 1.5)
      
      if(input$t3type3 == "Pct"){
        p + scale_y_continuous(labels = scales::percent)
      } else {
        p + scale_y_continuous(breaks= pretty_breaks())
      }
      
    }

    
  })
  
}

# Create the application
shinyApp(ui = ui, server = server)