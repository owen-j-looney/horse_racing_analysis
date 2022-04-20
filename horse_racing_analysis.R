library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(curl)
library(glue)
library(ggplot2)
library(caret)

#getting list of 
base_url <- "https://www.racingaustralia.horse/FreeFields/Calendar_Results.aspx"

state_list <- c("NSW",
                "QLD",
                "ACT",
                "VIC",
                "TAS",
                "SA",
                "WA",
                "NT")

#creating blank list so i can get a url for race results for each state
state_list_url <- c()

#generating urls for each states racing results over the last month
for (i in 1:length(state_list)) {
  state_list_url[[i]] <- paste0(base_url,"?State=", state_list[i])
}

#generating a list of urls to run through to get all race data for last 30 days
results <- function(url_list){
  paste0("https://www.racingaustralia.horse",
         read_html(url_list) %>%
           html_nodes("a") %>%       
           # find all links in the page
           html_attr("href") %>%
           unique() %>%
           str_subset("/FreeFields/Results")
  )
}

#getting list of all urls with race data and converting to one list
race_results_urls <- map(state_list_url, results) %>%
  flatten() 

#separating out trial and race data
trial_results_urls <- race_results_urls[grepl("Trial", race_results_urls)]

racing_results_urls <- race_results_urls[!grepl("Trial", race_results_urls)]

#creating function to extract all tables from each url 
read_html_tbls <- function(url) {
  url = url(url, 'rb')
  html <- read_html(url) %>%
    html_table()
  
  close(url)
  return(html)
}

#creating blank list for race tables to be appended to
race_tables <- c()

#appending all race tables
race_tables <- map(racing_results_urls,read_html_tbls)

#creating blank list for trial tables to be appended to
trial_tables <- c()

#appending all race tables
trial_tables <- map(trial_results_urls,read_html_tbls)

#naming each group of tables with their date and location
names(trial_tables) <- sub('.*Key=','',trial_results_urls)


#naming each group of tables with their date and location
names(race_tables) <- sub('.*Key=','',racing_results_urls)

#removing abandoned races with no data
race_tables_non_abndn <- Filter(function(x) length(x) > 1,race_tables)

#removing abandoned races with no data
trial_tables_non_abndn <- Filter(function(x) length(x) > 1,trial_tables)

#filtering out tables that have non race results or race information
#appending race info table as a character string to the relevant race so it can be used
for (n in 1:length(race_tables_non_abndn)){
  if (ncol(race_tables_non_abndn[[n]][[1]])==4){
    race_tables_non_abndn[[n]] <- race_tables_non_abndn[[n]][-1]
  } else {
    NULL
  }
  for (i in 1:length(race_tables_non_abndn[[n]])) {
    if ((i %% 2)!=0){
      NA
      } else {
        race_tables_non_abndn[[n]][[i]] <- race_tables_non_abndn[[n]][[i]] %>%
          mutate(character = as.character(race_tables_non_abndn[[n]][[i-1]]),
                 race_info = as.character(names(race_tables_non_abndn[[n]][[i-1]])),
                 info = glue("{names(race_tables_non_abndn)[[n]]}"))
      }
  }
}


#filtering out tables that have non trial results or race information
#appending race info table as a character string to the relevant race so it can be used
for (n in 1:length(trial_tables_non_abndn)){
  if (ncol(trial_tables_non_abndn[[n]][[1]])==4){
    trial_tables_non_abndn[[n]] <- trial_tables_non_abndn[[n]][-1]
  } else {
    NULL
  }
  for (i in 1:length(trial_tables_non_abndn[[n]])) {
    if (ncol(trial_tables_non_abndn[[n]][[i]])<2){
      NA
    } else {
      trial_tables_non_abndn[[n]][[i]] <- trial_tables_non_abndn[[n]][[i]] %>%
        mutate(character = as.character(trial_tables_non_abndn[[n]][[i-1]]),
               race_info = as.character(names(trial_tables_non_abndn[[n]][[i-1]])),
               info = glue("{names(trial_tables_non_abndn)[[n]]}"))
    }
  }
}

#flattening out both race and trial results
race_results_list <- flatten(race_tables_non_abndn)

trial_results_list <- flatten(trial_tables_non_abndn)

#keeping only tables that have anough columns to contain actual race data, not simply information regarding each race
filtered_race_results_list <- Filter(function(x) ncol(x) == 14,race_results_list)

filtered_trial_results_list <- Filter(function(x) ncol(x) == 14,trial_results_list)

#converting each list into a single df
race_results_combined <- do.call("rbind", filtered_race_results_list) 

trial_results_combined <- do.call("rbind", filtered_trial_results_list) 

#cleaning some data and extracting some information into columns for both race and trial data
race_results_combined <- race_results_combined %>%
  select(-Colour,
         -Penalty) %>%
  mutate(Finish = as.numeric(Finish),
         odds = as.numeric(gsub(".*?([0-9.]+).*", "\\1",`Starting Price`)),
         Weight = as.numeric(gsub("kg.*","", Weight)),
         Date_format = as.Date(str_split(info, ",") %>% map_chr(., 1),
                               format('%Y%b%d')),
         State = as.factor(str_split(info, ",") %>% map_chr(., 2)),
         Racecourse = as.factor(str_split(info, ",") %>% map_chr(., 3)),
         Race = as.factor(str_extract(race_info, "^Race \\d+")),
         Day_of_week = as.factor(format(Date_format, '%a')),
         distance = str_extract_all(race_info,"\\(\\d\\d\\d\\d METRES\\)"),
         track_condition = as.factor(as.character(str_extract_all(race_results_combined$character,"Track Condition: .* \\d+ T"))),
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         winning_time = gsub("Time: ","",str_extract_all(character,"Time: \\d+:\\d+.\\d+ "))
         ) %>%
  na.omit() 

#saveRDS(race_results_combined, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/Race_results_last_month_{Sys.Date()}.rds"))



trial_results_combined <- trial_results_combined %>%
  select(-Colour,
         -Penalty) %>%
  mutate(Finish = as.numeric(Finish),
         odds = as.numeric(gsub(".*?([0-9.]+).*", "\\1",`Starting Price`)),
         Weight = as.numeric(gsub("kg.*","", Weight)),
         Date_format = as.Date(str_split(info, ",") %>% map_chr(., 1),
                               format('%Y%b%d')),
         State = as.factor(str_split(info, ",") %>% map_chr(., 2)),
         Racecourse = as.factor(str_split(info, ",") %>% map_chr(., 3)),
         Race = as.factor(str_extract(race_info, "^Race \\d+")),
         Day_of_week = as.factor(format(Date_format, '%a')),
         distance = str_extract_all(race_info,"\\(\\d+ METRES\\)"),
         track_condition = as.factor(as.character(str_extract_all(character,"Track Condition: .* \\d+ T"))),
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         winning_time = gsub("Time: ","",str_extract_all(character,"Time: \\d+:\\d+.\\d+ "))
  )
#saveRDS(trial_results_combined, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/Trial_results_last_month_{Sys.Date()}.rds"))

#both trial and race data contain the same columns, so an analysis can be run on both combined if required

############### Results saved as RDS on local drive to allow combination of race results over longer periods in future ##########
##### End of script run to pull and clean race data into useable format
#### more can be done like converting dates etc.


train_index <- createDataPartition(race_results_combined$info, p=0.85,
                                   times = 1,
                                   list = F)


time_function <- function(dataset){
dataset$winning_time_sec<- as.numeric(gsub(":","",str_extract(dataset$winning_time,":\\d+.\\d+")))

dataset$winning_time_min <- as.numeric(str_extract(dataset$winning_time,"\\d+"))*60
dataset$winning_time_total <- dataset$winning_time_min+dataset$winning_time_sec
dataset$Margin <- gsub("L","",dataset$Margin) 
dataset$Margin_Metres <- as.numeric(dataset$Margin)*2.5
dataset$winner_MperSec <- dataset$distance/dataset$winning_time_total

dataset$time <- dataset$winning_time_total+
  ifelse(is.na(dataset$Margin_Metres/dataset$winner_MperSec),
         0,
         dataset$Margin_Metres/dataset$winner_MperSec)
return(dataset)
}

race_results_combined_time <- time_function(race_results_combined)
trial_results_combined_time <- time_function(trial_results_combined)

race_results_combined_train <- race_results_combined_time[train_index,] 
race_results_combined_test <- race_results_combined_time[-train_index,]


training_ds <- rbind(race_results_combined_train, trial_results_combined_time)


glm1 <- glm(data = training_ds, formula = time ~ distance + track_condition + Racecourse + Weight)


race_results_combined_test$time_pred <- predict(glm1, race_results_combined_test)


ggplot(race_results_combined_test)+
  geom_point(aes(x = time, y = time_pred, colour = as.factor(distance)))+
  geom_abline()

training_ds$win_indicator <- ifelse(training_ds$Finish == 1,1,0)
race_results_combined_test$win_indicator <- ifelse(race_results_combined_test$Finish==1,1,0)

glm_binom <- glm(data = training_ds, formula = win_indicator ~ distance+track_condition+Weight+Day_of_week+Date_format+Racecourse, family = "binomial")

race_results_combined_test$win_pred <- predict(glm_binom, race_results_combined_test, "response")

full_ds <- rbind(trial_results_combined_time, race_results_combined_time)


nested_training_ds <- full_ds %>%
  mutate(ID = paste(info,",",Race)) %>%
  select(Finish,
         Horse,
         Trainer,
         Jockey,
         Bar.,
         Weight,
         odds,
         Racecourse,
         Race,
         distance,
         Day_of_week,
         track_condition,
         time,
         ID) %>%
  group_by(ID)

library(rsample)
library(furrr)



# Sampling function that creates a Monte-Carlo CV set
# and returns the analysis portion.
mc_sample <- function(data, times, prop) {
  data %>% 
    mc_cv(times = times, prop = prop) %>% 
    mutate(analysis = map(splits, ~analysis(.x))) %>%
    select(-c(id, splits))
}

# Set up out workers
plan(multisession, workers = availableCores() - 1)

# Parallel sampling
number_samples <- 1000 
hr_mccv <- future_map(
  1:number_samples,
  ~{ mc_sample(nested_training_ds, times = 1, prop = .1) },
  .options = furrr_options(seed = TRUE)
)

# Switch plans to close workers and release memory
plan(sequential)

# Bind samples together and unnest
hr_mccv <- hr_mccv %>% 
  bind_rows() %>% 
  mutate(sample_id = 1:n()) %>% 
  unnest(cols = analysis) %>% 
  unnest(cols = data)



#function to apply a bet and calculate returns
bet_returns <- function(data, bet = 1) {
  data %>% 
    mutate(
      bet_return = if_else(
        Finish == 1,
        (bet * odds) - bet,
        -bet
      )
    ) %>% 
    group_by(ID) %>% 
    mutate(
      sample_race_index = 1:n(),
      cumulative_return = cumsum(bet_return),
      cumulative_rpr = cumulative_return / sample_race_index 
    ) %>% 
    ungroup()
}


hr_favourite <- hr_mccv %>% 
  drop_na(odds) %>% 
  group_by(sample_id, ID) %>% 
  mutate(odds.rank = order(odds)) %>% 
  slice_min(odds.rank, with_ties = FALSE, n = 1) %>% 
  ungroup()

# Place out bets
hr_favourite <- bet_returns(full_ds, bet = 100)



fav_horse <- training_ds %>%
  mutate(ID = paste(info,",",Race)) %>%
  select(Finish,
         Horse,
         Trainer,
         Jockey,
         Bar.,
         Weight,
         odds,
         Date_format,
         Racecourse,
         Race,
         distance,
         Day_of_week,
         track_condition,
         time,
         ID) %>%
  group_by(ID, Date_format)


betting_function <- function(data, bet = 100, groupby = ID){
  data <- data %>%
    group_by(Date_format, Racecourse) %>%
    mutate(bet = ifelse(odds == min(odds),bet,NA),
           return = ifelse(Finish ==1,(bet*odds)-bet,-bet)) %>%
    ungroup() %>%
    mutate(rolling_return = cumsum(coalesce(return,0)))
    
  return(data)
}




fav_bet <- betting_function(fav_horse, bet = 100)

ggplot(fav_bet)+
  geom_line(aes(x=Date_format, y = rolling_return))





#######

#Neural Network
library(caret)

v1 <- race_results_combined_train %>% 
  filter(time >0) %>%
  mutate(winner = ifelse(Finish==1,1,0))
v2 <- race_results_combined_test %>%
  filter(time >0) %>%
  mutate(winner = ifelse(Finish==1,1,0))

nn <- train(winner ~ Bar. + Weight + odds + Race + Day_of_week + distance + track_condition, data=v1, method = "nnet", linout=TRUE, trace = FALSE, type = "prob")

v2$pred <- predict(nn, v2)

v2_test <- v2 %>%
  filter(pred>0.65) %>%
  arrange(Date_format)%>%
  mutate(bet_result = ifelse(Finish ==1, (1000*odds)-1000,-1000),
         total = cumsum(bet_result))
