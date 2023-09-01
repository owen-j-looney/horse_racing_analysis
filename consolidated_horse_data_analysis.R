setwd("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis")

library(tidyverse)
library(reactable)
library(reshape2)
library(glue)
library(dplyr)
library(caret)
library(lubridate)

df <- list.files(pattern = ".rds") %>%
  map(readRDS)


df1 <- df %>%
  bind_rows %>%
  unique()



# creating time function to approximate a finish time for all horses, not just winning horses
time_function <- function(dataset){
  dataset$winning_time_sec<- as.numeric(gsub(":","",str_extract(dataset$winning_time,":\\d+.\\d+")))
  
  dataset$winning_time_min <- as.numeric(str_extract(dataset$winning_time,"\\d+"))*60
  dataset$winning_time_total <- dataset$winning_time_min+dataset$winning_time_sec
  dataset$Margin <- gsub("L","",dataset$Margin) 
  ## using the assumption that a horse length is roughly 2.5 metres
  dataset$Margin_Metres <- as.numeric(dataset$Margin)*2.5
  dataset$winner_MperSec <- dataset$distance/dataset$winning_time_total
  
  dataset$time <- dataset$winning_time_total+
    ifelse(is.na(dataset$Margin_Metres/dataset$winner_MperSec),
           0,
           dataset$Margin_Metres/dataset$winner_MperSec)
  return(dataset)
}

df2 <- time_function(df1)

df3 <- df2 %>%
  dplyr::select(-winning_time,
         -winning_time_sec,
         -winning_time_min,
         -winning_time_total,
         -winner_MperSec) %>%
  mutate(RaceID = paste(info,",",Race),
         ID = paste(info,",",Race,"Finish-", Finish),
         Margin_Metres = ifelse(is.na(Margin_Metres)==T, 0, Margin_Metres),
         winner = ifelse(Finish==1,1,0),
         trackcondition = as.factor(as.character(str_extract_all(df2$character,"Track Condition: .* Time:"))),
         track_condition = gsub('.{5}$','',trackcondition),
         Weight = ifelse(is.na(Weight),0,Weight),
         Race_time = str_extract(race_info,"\\d+:\\d+[AP]M"),
         Datetime = as.POSIXct(paste(Date_format, " ", Race_time), format = "%Y-%m-%d %I:%M%p"),
         Datetime = case_when(State=="WA" ~ Datetime+(3600*2),
                              State %in% c("SA","NT") ~ Datetime+(3600*0.5),
                              TRUE ~Datetime),
         #Datetime = format(Datetime, format = "%Y-%m-%d %I:%M%p"),
         Race_time = format(Datetime, format = "%I:%M%p")) %>%
  filter(time >0,
         is.na(Finish)!=T,
         track_condition!="character(",
         is.na(distance)==F) %>%
  arrange(Datetime,RaceID, Finish) %>%
  group_by(RaceID) %>%
  mutate(favourite = ifelse(odds == min(odds),1,0),
         fav_winner = ifelse(favourite ==1 & winner ==1,1,0)) %>%
  ungroup() %>%
  data.frame() %>%
  distinct(ID, .keep_all = T) %>%
  mutate(Bar = Bar.,
         Horse = gsub(" {1+}","",Horse),
         Horse = gsub("[^[:alpha:] ]", "", Horse),
         Jockey = gsub(" {1+}","",Jockey),
         Jockey = gsub("[^[:alpha:] ]", "", Jockey),
         Jockey = gsub("*\\(.{1+}\\)", "", Jockey),
         Jockey = gsub("kg$","",Jockey),
         Trainer = gsub(" {1+}","",Trainer),
         Trainer = gsub("[^[:alpha:] ]", "", Trainer),
         track_condition = gsub(" {1+}","",track_condition),
         track_condition = gsub("\\_{1+}","",track_condition),
         trackcondition = gsub(":{1+}","",track_condition))

df4 <- df3 %>%
  filter(!is.na(odds)) %>%
  #mutate(win_prize = as.numeric(gsub(" 2nd ","",
  #                                    gsub(",","",str_split(character, "\\$") %>% map_chr(., 3)))),
  #       prize_pool = as.numeric(gsub(".1st ","",
  #                                    gsub(",","",str_split(character, "\\$") %>% map_chr(., 2))))) %>%
  mutate(Bar = Bar.,
         Horse = gsub(" {1+}","",Horse),
         Horse = gsub("[^[:alpha:] ]", "", Horse),
         Jockey = gsub(" {1+}","",Jockey),
         Jockey = gsub("[^[:alpha:] ]", "", Jockey),
         Jockey = gsub("*\\(.{1+}\\)", "", Jockey),
         Jockey = gsub("kg$","",Jockey),
         Trainer = gsub(" {1+}","",Trainer),
         Trainer = gsub("[^[:alpha:] ]", "", Trainer),
         track_condition = gsub(" {1+}","",track_condition),
         track_condition = gsub("\\_{1+}","",track_condition),
         trackcondition = gsub(":{1+}","",track_condition))


#attempting to create a betting system on favourites where bets grow larger to recoup losses until a winner is picked providing the desired profit


daily_profit_function <- function(race_data, desired_profit_daily = 200, backup_daily_profit = desired_profit_daily){
  fav_betting_system_df <- race_data %>%
    #only need favourites
    filter(favourite == 1) %>%
    #arranging by time to align with real life and multiple tracks on same day, and barrier to remove any horses where there are multiple favourites so only a single fav is chosen for each race
    arrange(Datetime, Bar.) %>%
    #selecting single fav for each race
    distinct(Datetime, .keep_all =T) %>%
    #calculating required bet wo win $200 of that race
    mutate(bet = NA,
           new_bet = NA,
           total_outlay = NA) 
  
  #calculating the bet amount for each individual race, and the total outlay, for races after an initial race has failed. so all bet amounts would be taken off the new_bet once this is complete
  for (i in 1:nrow(fav_betting_system_df)){
    if (i==1){
      fav_betting_system_df$bet[[i]] <- 1
      fav_betting_system_df$new_bet[[i]] <- desired_profit_daily/(fav_betting_system_df$odds[[i]]-1)
      fav_betting_system_df$total_outlay[[i]] <- desired_profit_daily/(fav_betting_system_df$odds[[i]]-1)
    } else if(fav_betting_system_df$Date_format[[i]] != fav_betting_system_df$Date_format[[i-1]]) {
      fav_betting_system_df$bet[[i]] <- 1
      fav_betting_system_df$new_bet[[i]] <- desired_profit_daily/(fav_betting_system_df$odds[[i]]-1)
      fav_betting_system_df$total_outlay[[i]] <- desired_profit_daily/(fav_betting_system_df$odds[[i]]-1)
    } else {
      fav_betting_system_df$bet[[i]] <- ifelse(fav_betting_system_df$bet[[i-1]]==0,
                                               0,
                                               ifelse(fav_betting_system_df$winner[[i-1]]==1,
                                                      0,
                                                      1))
      fav_betting_system_df$new_bet[[i]] <- ifelse(fav_betting_system_df$bet[[i]]==1,(fav_betting_system_df$total_outlay[[i-1]]+backup_daily_profit)/(fav_betting_system_df$odds[[i]]-1),0)
      fav_betting_system_df$total_outlay[[i]] <- ifelse(fav_betting_system_df$bet[[i]]==1, (fav_betting_system_df$total_outlay[[i-1]]+fav_betting_system_df$new_bet[[i]]),0)
    }
  }
  
  return(fav_betting_system_df)
}

graph_daily_bets <- function(profit_data){
  #quick plot of number of bets per day to get a winner
  ggplot(profit_data %>% group_by(Date_format) %>%
           summarise(numbets_day = sum(bet))) +
    geom_line(aes(x=Date_format, y = numbets_day))+
    labs(title = "plot of number of bets per day to get a winner",
         x = "Date",
         y = "Number of daily bets required until a favourite wins")
  
}

graph_daily_profits <- function(profit_data){
  #quick plot of maximum amount required for any single day of betting to get a winner over the previous month
  ggplot(profit_data %>% group_by(Date_format) %>%
           summarise(maxbet = max(total_outlay))) +
    geom_line(aes(x=Date_format, y = maxbet))+
    labs(title = "plot of maximum amount required for any single day of betting to get a winner",
         x = "Date",
         y = "Total money required to 'double down'")
}

graph_total_profits <- function(profit_data){
  ggplot(profit_data %>% 
           #filtering for only races we bet on now
           filter(bet ==1) %>%
           mutate(profits = ifelse(winner ==1,(new_bet*odds)-new_bet,-new_bet),
                  roll_profits = cumsum(profits))) +
    geom_line(aes(x = Datetime, y = roll_profits))+
    labs(title = "accumulative profits through time",
         x = "Time",
         y = "Accumulative profits")
}

profit_system_df_function <- function(profit_data){
  fav_betting_system_df_bets <- profit_data %>%
    #filtering for only races we bet on now
    filter(bet ==1) %>%
    mutate(profits = ifelse(winner ==1,(new_bet*odds)-new_bet,-new_bet),
           roll_profits = cumsum(profits))
  return(fav_betting_system_df_bets)
}


profit_50_daily <- daily_profit_function(df4, 10,10)
graph_daily_bets(profit_50_daily)
graph_daily_profits(profit_50_daily)
graph_total_profits(profit_50_daily)


profit_50_daily_runs_until_win <- profit_50_daily_runs_until_win %>%
  mutate(count_ind = ifelse(winner==1,0,1))
for (i in 1:nrow(profit_50_daily_runs_until_win)){
  if (profit_50_daily_runs_until_win$winner[[i]]==1){
    profit_50_daily_runs_until_win$num_bets[[i]]=0
  } else {
    profit_50_daily_runs_until_win$num_bets[[i]]=profit_50_daily_runs_until_win$count_ind[[i]]+profit_50_daily_runs_until_win$num_bets[[i-1]]
  }
}
ggplot(profit_50_daily_runs_until_win)+geom_line(aes(x=Datetime, y=num_bet))
graph_daily_bets(profit_50_daily_runs_until_win)

t1 <- profit_50_daily %>% filter(bet ==1) %>% group_by(Date_format) %>%
  summarise(Day_of_week,
            bets_per_day = n()) 

reactable(profit_system_df_function(profit_50_daily) %>% dplyr::select(-character,
                                                                -info,
                                                                -race_info))  

################
#########

df3 <- df2 %>%
  dplyr::select(-winning_time,
         -winning_time_sec,
         -winning_time_min,
         -winning_time_total,
         -winner_MperSec) %>%
  mutate(RaceID = paste(info,",",Race),
         ID = paste(info,",",Race,"Finish-", Finish),
         Margin_Metres = ifelse(is.na(Margin_Metres)==T, 0, Margin_Metres),
         winner = ifelse(Finish==1,1,0),
         track_condition = gsub('.{2}$','',track_condition),
         Weight = ifelse(is.na(Weight),0,Weight),
         Race_time = str_extract(race_info,"\\d+:\\d+[AP]M"),
         Datetime = as.POSIXct(paste(Date_format, " ", Race_time), format = "%Y-%m-%d %I:%M%p"),
         track_condition = as.factor(as.character(str_extract_all(df2$character,"Track Condition: .* Time:"))),
         track_condition = gsub("Time:","", track_condition),
         Datetime = ifelse(State=="WA",
                           Datetime+dhours(2),
                           ifelse(State %in% c("SA","NT"),
                                  Datetime+dhours(1.5),
                                  Datetime)),
         Race_time = format(Datetime, format = "%I:%M%p")) %>%
  filter(time >0,
         is.na(Finish)!=T,
         track_condition!="character(",
         is.na(distance)==F) %>%
  arrange(Datetime,RaceID, Finish) %>%
  mutate(Bar = Bar.,
         Horse = gsub(" {1+}","",Horse),
         Horse = gsub("[^[:alpha:] ]", "", Horse),
         Jockey = gsub(" {1+}","",Jockey),
         Jockey = gsub("[^[:alpha:] ]", "", Jockey),
         Jockey = gsub("*\\(.{1+}\\)", "", Jockey),
         Jockey = gsub("kg$","",Jockey),
         Trainer = gsub(" {1+}","",Trainer),
         Trainer = gsub("[^[:alpha:] ]", "", Trainer),
         track_condition = gsub(" {1+}","",track_condition),
         track_condition = gsub("\\_{1+}","",track_condition),
         trackcondition = gsub(":{1+}","",track_condition)
         ) %>%
  filter(!is.na(odds)) %>%
  mutate(win_prize = as.numeric(gsub(" 2nd ","",
                                     gsub(",","",str_split(character, "\\$") %>% map_chr(., 3)))),
         prize_pool = as.numeric(gsub(".1st ","",
                                      gsub(",","",str_split(character, "\\$") %>% map_chr(., 2)))))

  test_model_race <- c(df3$Date_format[[nrow(df3)]])

test_model <- df3 %>% filter(Date_format %in% test_model_race) %>%
  dplyr::select(RaceID,
         Horse,
         Jockey,
         Trainer,
         Finish, 
         Bar,
         Weight,
         distance,
         trackcondition,
         time,
         odds,
         #win_prize,
         #prize_pool
         )

train_model <- df3 %>% 
  filter(#!RaceID %in% test_model_race,
         Horse %in% test_model$Horse,
         #Jockey %in% test_model$Jockey,
         #Trainer %in% test_model$Trainer,
         trackcondition %in% test_model$trackcondition
         ) %>%
  dplyr::select(RaceID,
         Horse,
         Jockey,
         Trainer,
         Finish, 
         Bar,
         Weight,
         distance,
         trackcondition,
         time,
         odds,
         #win_prize,
         #prize_pool
         )

test_model <- test_model %>%
  filter(Horse %in% train_model$Horse,
         #Jockey %in% train_model$Jockey,
         #Trainer %in% train_model$Trainer
         trackcondition %in% train_model$trackcondition
         )

#train_model <- train_model %>% 
#  filter(Horse %in% test_model$Horse,
#         Jockey %in% test_model$Jockey,
#         Trainer %in% test_model$Trainer,
#         trackcondition %in% test_model$trackcondition)
#

library(merTools)

horse_mod <- glmer(time ~ distance + (1|trackcondition)+ (1|distance) + (1|odds)+ (1|Weight) +(0+distance|Horse), data = train_model)

horse_sim <- function(simulation = 50, top_n =1, mod = horse_mod_basic){
  p1 <- predictInterval(mod, newdata = test_model, n.sims = simulation, returnSims = T)
  pred_times <- as.data.frame(attributes(p1)$sim.results)

  #t1 <- simulate(horse_mod, nsim = 1000, newdata = race_tomorrow_combined_test)
  
  
  rf <- test_model %>%
    dplyr::select(RaceID, Horse, Finish)
  
  
  race_tomorrow_combined_times <- cbind(rf, pred_times) %>%
    group_by(RaceID) %>%
    mutate(across(V1:glue("V{simulation}"), ~rank(.x)),
           across(V1:glue("V{simulation}"), ~ifelse(.x<=top_n,1,0))) %>%
    rowwise() %>%
    summarise(RaceID,
              Horse,
              Finish,
              prob = sum(c_across(V1:glue("V{simulation}")))/simulation)
  
  return(race_tomorrow_combined_times)
}

horse_racing_perc <- list()
horse_racing <- list()

mod_accuracy <- function(num_runs=10, mod = horse_mod_basic) {
for (i in 1:num_runs) {
horse_racing_perc[[i]] <- horse_sim(mod, simulation = 10000,top_n = 1) %>%
  mutate(id = glue("prob_{i}"))

horse_racing <- do.call(rbind, horse_racing_perc) %>%
  spread(key = id, value = prob) %>%
  rowwise() %>%
  mutate(prob_avg = mean(c_across(prob_1:glue("prob_{i}")))) %>%
  ungroup() %>%
  mutate(odds = 1/prob_avg) %>%
  group_by(RaceID) %>%
  arrange(RaceID, odds) %>%
  mutate(pred_fav = 1:n()) %>%
  filter(pred_fav ==1) %>%
  mutate(fav_accuracy = ifelse(Finish == 1,1,0)) %>%
  ungroup() %>%
  summarise(count = n(),
            fav_wins = sum(fav_accuracy),
            perc = fav_wins/count)
    
}
return(horse_racing$perc)
}

#set.seed(NULL)

horse_mod_basic <- lmer(time ~ distance + trackcondition + Bar + odds + Weight + (distance|Horse), data = train_model)
mod_accuracy(num_runs = 3, mod = horse_mod)


mod_accuracy(3)
##################
library(caret)

nn_train_df <- df3 %>% 
  filter(Horse %in% race_tomorrow_combined_test$Horse) %>%
  dplyr::select(time,Bar,Weight, trackcondition, distance, Horse, odds, win_prize, prize_pool)
nn_test_df <- race_tomorrow_combined_test %>% 
  filter(Horse %in% nn_train_df$Horse)


nnet_model <- train(time ~ .,
                    data = nn_train_df,
                    method = 'glm',
                    #trControl= train_params,
                    na.action = na.omit,
                    tuneLength = 9
)



p1 <- predict(nnet_model, nn_test_df)

p1<- as.data.frame(p1)

preds <- cbind(nn_test_df, p1)




##################




test_model_1 <- test_model %>% 
  filter(Horse %in% train_model$Horse) 
#%>%
  dplyr::select(-Finish)

nn_train_dummy_vars <- dummyVars(" ~ .", data = train_model %>%
                                   dplyr::select(Horse))

nn_test_dummy_vars <- dummyVars(" ~ .", data = test_model %>%
                                  dplyr::select(Horse))


nn_train_dummy_df <- data.frame(predict(nn_train_dummy_vars, newdata = train_model))

nn_train_df <- cbind(train_model %>% 
                       dplyr:: select(Bar,
                                      Weight,
                                      distance,
                                      odds,
                                      #win_prize,
                                      #prize_pool,
                                      time), nn_train_dummy_df) %>%
  na.omit()
  
nn_test_dummy_df <- data.frame(predict(nn_test_dummy_vars, newdata = test_model))


nn_test_df <- cbind(test_model %>%
                       dplyr:: select(Bar,
                                      Weight,
                                      odds,
                                      #win_prize,
                                      #prize_pool,
                                      distance), nn_test_dummy_df)


library(doParallel)

registerDoParallel(cores = 3)

fitControl <- trainControl(method = "cv",   
                           number = 20) 

#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
fit2 <- train(time ~ ., data = nn_train_df, 
              method = 'glm', 
              trControl = fitControl)

preds <- predict(fit2, nn_test_df)

nn_test_df_preds_acc <- cbind(preds, test_model) %>%
  group_by(RaceID) %>%
  arrange(RaceID,
          preds) %>%
  mutate(pred_finish = 1:n(),
         win_correct = ifelse(pred_finish ==1 & Finish ==1,1,0),
         place_correct = ifelse(pred_finish <4 & Finish <4,1,0)) %>%
  ungroup() %>%
  filter(pred_finish==1) %>%
  summarise(count =n(),
            win_accuracy = sum(win_correct)/count,
            place_accuracy = sum(place_correct)/count)





#fitControl <- trainControl(## 10-fold CV
#  method = "none",
#  number = 1#,
  ## repeated ten times
  #repeats = 1
#  )


############ Getting tomorrows race data to predict on ####
library(rvest)
library(lubridate)
library(glue)
base_url <- 'https://www.racingaustralia.horse/FreeFields/Calendar.aspx'


#getting required urls
#base_url <- "https://www.racingaustralia.horse/FreeFields/Calendar_Results.aspx"

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


#generating a list of urls to run through to get all race data for last month
results <- function(url_list){
  paste0("https://www.racingaustralia.horse",
         read_html(url_list) %>%
           html_nodes("a") %>%       
           # find all links in the page
           html_attr("href") %>%
           unique() %>%
           str_subset("/FreeFields/Form")
  )
}

#getting list of all urls with race data and converting to one list
race_results_urls <- map(state_list_url, results) %>%
  purrr::flatten()

#creating a key so we only keep urls for tomorrows races to predict on
tomorrow_url_key <- paste0(year(Sys.Date()),
                           month(Sys.Date(),label = T, abbr = T),
                           day(Sys.Date()+1))
#creating today url key if this is run in the am
today_url_key <- paste0(year(Sys.Date()),
                           month(Sys.Date(),label = T, abbr = T),
                           ifelse(day(Sys.Date())<10,paste0("0",day(Sys.Date())),day(Sys.Date())))


#separating out trial and race data
trial_results_urls <- race_results_urls[grepl("Trial", race_results_urls)]

racing_results_urls <- race_results_urls[!grepl("Trial", race_results_urls)]

racing_results_urls <- as.list(grep(today_url_key, racing_results_urls, value = T))
  

#creating function to extract all tables from each url 
read_html_tbls <- function(url) {
  html <- read_html(url) %>% 
    html_nodes("table") %>% 
    html_table() 
  

  #close(url)
  return(html)
}

track_info <- c()

for (i in 1:length(racing_results_urls)){
track_info[[i]] <- as.character(read_html(racing_results_urls[[i]]) %>% 
      html_nodes(".race-venue-bottom") %>%
      html_text())
}


#creating blank list for race tables to be appended to
race_tables <- c()

#appending all race tables
race_tables <- map(racing_results_urls,read_html_tbls)

#naming each group of tables with their date and location
names(race_tables) <- sub('.*Key=','',racing_results_urls)

for (n in 1:length(race_tables)){
  #some days the first table is a 4 column table which contains no useful information and these are to be filtered out (on days like public holidays etc. where there are certain specialties applied by the looks of it)
  for (i in 1:length(race_tables[[n]])) {
      #taking useful data from previous tables and converting it into columns in current table
      race_tables[[n]][[i]] <-race_tables[[n]][[i]] %>% 
        mutate(info = glue("{names(race_tables)[[n]]}"))
    }
  }

for (n in 1:length(race_tables)){
  for (i in 1:length(race_tables[[n]])){
    race_tables[[n]][[i]] <- race_tables[[n]][[i]] %>%
      mutate(track_info = track_info[[n]])
  }
}


race_table <- purrr::flatten(race_tables)

#filtering out tables that have non race results or race information
#appending race info table as a character string to the relevant race so it can be used
for (n in 2:length(race_table)){
  #some days the first table is a 4 column table which contains no useful information and these are to be filtered out (on days like public holidays etc. where there are certain specialties applied by the looks of it)    
  if (ncol(race_table[[n]])==3){
      race_table[[n]] <- race_table[[n]]
    } else if (ncol(race_table[[n-1]])==3){
      race_table[[n]] <- race_table[[n]]
      } else {
      race_table[[n]] <- tibble()
      }
}

race_tables_tomorrow <- Filter(function(x) length(x) > 1,race_table)  


for (i in 1:length(race_tables_tomorrow)) {
    #keeping tables that are 11 columns wide - containing race data, all others containing 1 col are race info or simply abandoned races that are not useful.
    if (ncol(race_tables_tomorrow[[i]])==3){
      NA
    } else {
      #taking useful data from previous tables and converting it into columns in current table
      race_tables_tomorrow[[i]] <-race_tables_tomorrow[[i]] %>% 
        mutate(character = as.character(race_tables_tomorrow[[i-1]][[1]]),
               race_info = as.character(names(race_tables_tomorrow[[i-1]][1])))
    }
  }

race_tables_tomoz <- Filter(function(x) length(x) > 3,race_tables_tomorrow) 

library(plyr)

race_tomorrow_combined <- do.call("rbind.fill",
                                  race_tables_tomoz)
detach("package:plyr", unload = TRUE)

race_tomorrow_combined <- race_tomorrow_combined %>%
  mutate(info = gsub('%2C',",", info),
         info = gsub('%20'," ", info),
         Weight = as.numeric(gsub("kg.*","", Weight)),
         #converting Date to an actual date in required format
         Date_format = as.Date(str_split(info, ",") %>% map_chr(., 1),
                               format('%Y%b%d')),
         #splitting the info column into useful data such as state & racecourse
         State = as.factor(str_split(info, ",") %>% map_chr(., 2)),
         Racecourse = as.factor(str_split(info, ",") %>% map_chr(., 3)),
         #getting race number
         Race = as.factor(str_extract(race_info, "^Race \\d+")),
         #converting date to a day of the week - purely as Wednesdays and Saturdays are bigger meets
         Day_of_week = as.factor(format(Date_format, '%a')),
         #getting race/trial distance
         distance = str_extract_all(race_info,"\\(\\d{1,} METRES\\)"),
         #converting distance to number
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         track_info = gsub("\\r","",
                           gsub("\\n", "",
                                gsub("\\t","",track_info))),
         #getting track condition which is critically important to clean properly
         trackcondition = as.factor(substr(str_extract_all(race_tomorrow_combined$track_info,"Track Condition: .* [\\d+, ]{0,}W"),1,24)),
         trackcondition = gsub(" ","",
                                gsub("W","",
                                     gsub(":","",trackcondition))),
         RaceID = paste(info,",",Race),
         Race_time = str_extract(race_info,"\\d+:\\d+[AP]M"),
         Horse = gsub(" {1+}","",Horse),
         Horse = gsub("[^[:alpha:] ]", "", Horse),
         Jockey = gsub(" {1+}","",Jockey),
         Jockey = gsub("[^[:alpha:] ]", "", Jockey),
         Jockey = gsub("*\\(.{1+}\\)", "", Jockey),
         Jockey = gsub("kg$","",Jockey),
         Trainer = gsub(" {1+}","",Trainer),
         Trainer = gsub("[^[:alpha:] ]", "", Trainer),
         trackcondition = gsub(" {1+}","",trackcondition),
         trackcondition = gsub("\\_{1+}","",trackcondition),
         trackcondition = gsub(":{1+}","",trackcondition),
         trackcondition = gsub("TrackConditionHeavy1", "TrackConditionHeavy10",trackcondition),
         win_prize = as.numeric(gsub(" 2nd ","",
                                     gsub(",","",str_split(character, "\\$") %>% map_chr(., 3)))),
         prize_pool = as.numeric(gsub(".1st ","",
                                      gsub(",","",str_split(character, "\\$") %>% map_chr(., 2)))))

race_tomorrow_combined_test <- race_tomorrow_combined %>%
  mutate(trackcondition = gsub("TrackConditionGoode", "TrackConditionGood", trackcondition)) %>%
  filter(trackcondition!= "TrackConditionGood",
         trackcondition!= "TrackConditionSynthet",
         !is.na(trackcondition)) %>%
  mutate(Bar = Barrier,
         win_prize = as.numeric(gsub(" 2nd ","",
                                     gsub(",","",str_split(character, "\\$") %>% map_chr(., 3)))),
         prize_pool = as.numeric(gsub(".1st ","",
                                      gsub(",","",str_split(character, "\\$") %>% map_chr(., 2)))))


#############

#test_model_race <- c(df3$Date_format[[nrow(df3)]])
#
#test_model <- df3 %>% filter(Date_format %in% test_model_race) %>%
#  dplyr::select(RaceID,
#                Horse,
#                Jockey,
#                Trainer,
#                Finish, 
#                Bar,
#                Weight,
#                distance,
#                trackcondition,
#                time)

test_model_test <- runners_with_odds %>%
  dplyr::select(RaceID,
                Horse,
                Jockey,
                Trainer,
                Bar,
                Weight,
                distance,
                trackcondition,
                win_prize,
                odds,
                prize_pool) %>%
  na.omit()

test_model_race <- c(race_tomorrow_combined_test$RaceID)

train_model <- df3 %>% 
  filter(!RaceID %in% test_model_race,
         Horse %in% test_model_test$Horse,
         #Jockey %in% test_model_test$Jockey,
         #Trainer %in% test_model$Trainer,
         #trackcondition %in% test_model_test$trackcondition
  ) %>%
  dplyr::select(RaceID,
                Horse,
                Jockey,
                Trainer,
                Finish, 
                Bar,
                Weight,
                distance,
                trackcondition,
                win_prize,
                prize_pool,
                odds,
                time)

test_model <- test_model_test %>%
  filter(Horse %in% train_model$Horse,
         #Jockey %in% train_model$Jockey,
         #Trainer %in% train_model$Trainer
         #trackcondition %in% train_model$trackcondition
  )



test_model_1 <- test_model %>% 
  filter(Horse %in% train_model$Horse) 
#%>%
#dplyr::select(-Finish)

nn_train_dummy_vars <- dummyVars(" ~ .", data = train_model %>%
                                   dplyr::select(Horse))

nn_test_dummy_vars <- dummyVars(" ~ .", data = test_model %>%
                                  dplyr::select(Horse))


nn_train_dummy_df <- data.frame(predict(nn_train_dummy_vars, newdata = train_model))

nn_train_df <- cbind(train_model %>% 
                       dplyr:: select(Bar,
                                      Weight,
                                      distance,
                                      odds,
                                      prize_pool,
                                      win_prize,
                                      trackcondition,
                                      time), nn_train_dummy_df) %>%
  na.omit()

nn_test_dummy_df <- data.frame(predict(nn_test_dummy_vars, newdata = test_model))


nn_test_df <- cbind(test_model %>%
                      dplyr::select(Bar,
                                     Weight,
                                     odds,
                                     win_prize,
                                    trackcondition,
                                     prize_pool,
                                     distance), nn_test_dummy_df)


library(doParallel)

registerDoParallel(cores = 3)

fitControl <- trainControl(method = "cv",   
                           number = 20) 

#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
fit2 <- train(time ~ ., data = nn_train_df, 
              method = 'glm', 
              trControl = fitControl)

preds <- predict(fit2, nn_test_df)

nn_test_df_preds_glm <- cbind(preds, test_model) %>%
  group_by(RaceID) %>%
  arrange(RaceID,
          preds) %>%
  mutate(pred_finish = 1:n())


race_tomorrow_combined_test_summary <- race_tomorrow_combined_test %>%
  group_by(RaceID) %>%
  summarise(RaceID,
            count= n()) %>%
  unique()

nn_test_df_preds_summary <- nn_test_df_preds %>%
  group_by(RaceID) %>%
  summarise(RaceID,
            count= n()) %>%
  unique()

race_numbers <- full_join(race_tomorrow_combined_test_summary, nn_test_df_preds_summary, by = "RaceID") %>%
  mutate(perc = (count.y/count.x)*100)

race_numbers_bet <- race_numbers %>%
  filter(perc>=85)

bets <- inner_join(race_numbers_bet, nn_test_df_preds) %>%
  filter(pred_finish ==1)


bets_Earth <- inner_join(race_numbers_bet, nn_test_df_preds_Earth) %>%
  filter(pred_finish ==1)
  
  
pred_winners <- nn_test_df_preds %>%
  filter(pred_finish ==1)




##Mixed Model ##

library(lme4)
library(merTools)


#horse_mod <- lmer(time ~ distance + trackcondition +(1+distance|Horse), data = df3)
horse_mod <- glmer(time ~ distance + (1|trackcondition)+ (1|distance) + (1|Weight) + (1|prize_pool) + (1|win_prize) +(0+distance|Horse), data = df4)
hm1 <- lmer(time ~ distance + trackcondition+ (0+Weight) + (0+distance|trackcondition) +(1+distance|Horse), data = df3)

horse_raceing_sim <- function(simulation = 50, top_n =1, mod = horse_mod, df = race_tomorrow_combined_test){
  p1 <- predictInterval(mod, newdata = df, n.sims = simulation, returnSims = T)
  pred_times <- as.data.frame(attributes(p1)$sim.results)
  
  #t1 <- simulate(horse_mod, nsim = 1000, newdata = race_tomorrow_combined_test)
  
  
  rf <- df %>%
    dplyr::select(RaceID, Horse)
  
  
  race_tomorrow_combined_times <- cbind(rf, pred_times) %>%
    group_by(RaceID) %>%
    mutate(across(V1:glue("V{simulation}"), ~rank(.x)),
           across(V1:glue("V{simulation}"), ~ifelse(.x<=top_n,1,0))) %>%
    rowwise() %>%
    summarise(RaceID,
              Horse,
              prob = sum(c_across(V1:glue("V{simulation}")))/simulation)
  
  return(race_tomorrow_combined_times)
}
  
pred_winnings <- horse_raceing_sim(simulation = 10000,top_n = 1, mod = horse_mod) %>%
  mutate(odds = 1/prob) %>%
  arrange(RaceID, odds)
pred_winnings_1 <- horse_raceing_sim(simulation = 10000,top_n = 1, mod = hm1) %>%
  mutate(odds = 1/prob) %>%
  arrange(RaceID, odds)


d1 <- inner_join(pred_winnings, race_tomorrow_combined_test, by = c("RaceID","Horse"))
d1 <- inner_join(pred_winnings_1, d1, by = c("RaceID","Horse"))

d2 <- d1 %>%
  arrange(RaceID, desc(prob)) %>%
  mutate(pred_finish = 1:n()) %>%
  filter(pred_finish ==1)

pred_placings <- horse_raceing_sim(simulation = 10000,top_n = 3, mod = horse_mod) %>%
  mutate(odds = 1/prob) %>%
  arrange(RaceID, odds)
pred_placings_1 <- horse_raceing_sim(simulation = 10000,top_n = 3, mod = hm1) %>%
  mutate(odds = 1/prob) %>%
  arrange(RaceID, odds)


pl1 <- inner_join(pred_placings, race_tomorrow_combined_test, by = c("RaceID","Horse"))
pl1 <- inner_join(pred_placings_1, pl1, by = c("RaceID","Horse"))


pl2 <- pl1 %>%
  arrange(RaceID, desc(prob)) %>%
  mutate(pred_finish = 1:n()) %>%
  filter(pred_finish ==1)

t1 <- data.frame()
pred_winnings <- list()

test_fun <- function(n, model = horse_mod, top_runners =3) {
for (i in 1:n){
  pred_winnings[[i]] <- horse_raceing_sim(mod = model ,simulation = 10000,top_n = top_runners) %>%
    mutate(id = glue("prob_{i}"))
}
  t1 <- do.call("rbind",pred_winnings) %>%
    spread(key = id, value = prob) %>%
    rowwise() %>%
    mutate(avg = mean(c_across(prob_1:glue("prob_{n}"))))
  return(t1)
}
a1<- test_fun(3, top_runners = 1)

a1 <- a1 %>% group_by(RaceID) %>%
  mutate(fav_dif = avg - min(avg),
         fav_rank = 1:n()) %>%
  filter(fav_rank %in% c(1,2))

#######################################

race_tomorrow_combined_pred <- inner_join(race_tomorrow_combined_test, pred_winnings, by = c("RaceID","Horse"))

race_tomorrow_combined_pred <- cbind(race_tomorrow_combined_test, pred_times) %>%
  group_by(RaceID) %>%
  arrange(pred_times) %>%
  mutate(pred_finish = row_number()) #%>%
  filter(pred_finish %in% c(1,2)) %>%
  mutate(time_from_winner = abs(pred_times-min(pred_times)))


race_tomorrow_combined_count <- race_tomorrow_combined %>%
  group_by(RaceID) %>%
  summarise(count = n())

race_tomorrow_combined_test_count <- race_tomorrow_combined_test %>%
  group_by(RaceID) %>%
  summarise(count = n())

#race_preds_fulfilled <- race_tomorrow_combined_count %>%
#  mutate(perc_fulfiled = (race_tomorrow_combined_test_count$count/count)*100)%>%
#  filter(perc_fulfiled>=90)

#race_tomorrow_combined_pred_bet <- race_tomorrow_combined_pred %>%
#  filter(RaceID %in% race_preds_fulfilled$RaceID) %>%
#  group_by(RaceID) %>%
#  filter(pred_finish %in% c(1,2,3)) %>%
#  mutate(time_from_winner = abs(pred_times-min(pred_times)))

##### getting races results for yesterday to compare predictions to results
#getting required urls
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

#generating a list of urls to run through to get all race data for last month
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
  purrr::flatten() 

#yesterday url key
yesterday_url_key <- paste0(year(Sys.Date()),
                            month(Sys.Date(),label = T, abbr = T),
                            ifelse(day(Sys.Date())<=10,paste0("0",day(Sys.Date()-1)),day(Sys.Date()-1)))

#2 days ago key
yesterday_2_url_key <- paste0(year(Sys.Date()),
                            month(Sys.Date(),label = T, abbr = T),
                            ifelse(day(Sys.Date())<=10,paste0("0",day(Sys.Date()-2)),day(Sys.Date()-2)))

#separating out trial and race data
trial_results_urls <- race_results_urls[grepl("Trial", race_results_urls)]

racing_results_urls <- race_results_urls[!grepl("Trial", race_results_urls)]

racing_results_urls <- as.list(grep(yesterday_2_url_key, racing_results_urls, value = T))
#racing_results_urls <- as.list(grep(today_url_key, racing_results_urls, value = T))

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
  #some days the first table is a 4 column table which contains no useful information and these are to be filtered out (on days like public holidays etc. where there are certain specialties applied by the looks of it)
  if (ncol(race_tables_non_abndn[[n]][[1]])==4){
    race_tables_non_abndn[[n]] <- race_tables_non_abndn[[n]][-1]
  } else {
    NULL
  }
  for (i in 1:length(race_tables_non_abndn[[n]])) {
    #keeping tables that are 11 columns wide - containing race data, all others containing 1 col are race info or simply abandoned races that are not useful.
    if (ncol(race_tables_non_abndn[[n]][[i]])!=11){
      NA
    } else {
      #taking useful data from previous tables and converting it into columns in current table
      race_tables_non_abndn[[n]][[i]] <-race_tables_non_abndn[[n]][[i]] %>% 
        mutate(character= as.character(race_tables_non_abndn[[n]][[i-1]]),
               race_info = as.character(names(race_tables_non_abndn[[n]][[i-1]])),
               info = glue("{names(race_tables_non_abndn)[[n]]}"))
    }
  }
}



#filtering out tables that have non trial results or race information
#appending race info table as a character string to the relevant race so it can be used
#running same query as above - did not create a function due to data extraction occuring once and this applying to 2 data sets - copy and paste with changes is just as time efficient as making an iterable function
for (n in 1:length(trial_tables_non_abndn)){
  if (ncol(trial_tables_non_abndn[[n]][[1]])==4){
    trial_tables_non_abndn[[n]] <- trial_tables_non_abndn[[n]][-1]
  } else {
    NULL
  }
  for (i in 1:length(trial_tables_non_abndn[[n]])) {
    if (ncol(trial_tables_non_abndn[[n]][[i]])!=11){
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
race_results_list <- purrr::flatten(race_tables_non_abndn)

trial_results_list <- purrr::flatten(trial_tables_non_abndn)

#keeping only tables that have anough columns to contain actual race data, not simply information regarding each race
filtered_race_results_list <- Filter(function(x) ncol(x) == 14,race_results_list)

filtered_trial_results_list <- Filter(function(x) ncol(x) == 14,trial_results_list)

#converting each list into a single df
race_results_combined <- do.call("rbind", filtered_race_results_list) 

trial_results_combined <- do.call("rbind", filtered_trial_results_list) 

#cleaning some data and extracting some information into columns for both race and trial data
race_results_combined <- race_results_combined %>%
  #removing useless columns
  dplyr::select(-Colour,
         -Penalty) %>%
  mutate(Finish = as.numeric(Finish),
         Horse = gsub(" ","",Horse),
         
         #converting odds & weight to a number
         odds = as.numeric(gsub(".*?([0-9.]+).*", "\\1",`Starting Price`)),
         Weight = as.numeric(gsub("kg.*","", Weight)),
         #converting Date to an actual date in required format
         Date_format = as.Date(str_split(info, ",") %>% map_chr(., 1),
                               format('%Y%b%d')),
         #splitting the info column into useful data such as state & racecourse
         State = as.factor(str_split(info, ",") %>% map_chr(., 2)),
         Racecourse = as.factor(str_split(info, ",") %>% map_chr(., 3)),
         #getting race number
         Race = as.factor(str_extract(race_info, "^Race \\d+")),
         #converting date to a day of the week - purely as Wednesdays and Saturdays are bigger meets
         Day_of_week = as.factor(format(Date_format, '%a')),
         #getting race/trial distance
         distance = str_extract_all(race_info,"\\(\\d\\d\\d\\d METRES\\)"),
         #getting track condition which is critically important to clean properly
         trackcondition = as.factor(as.character(str_extract_all(race_results_combined$character,"Track Condition: .* \\d+ T"))),
         #converting distance to number
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         #applying the winning time from each race to all horses in that race so an approx time for each hores could be calculated in the future
         winning_time = gsub("Time: ","",str_extract_all(character,"Time: \\d+:\\d+.\\d+ ")),
         RaceID = paste(info,",",Race),
         Horse = gsub("[^[:alpha:] ]", "", Horse),
         Jockey = gsub(" {1+}","",Jockey),
         Jockey = gsub("[^[:alpha:] ]", "", Jockey),
         Jockey = gsub("*\\(.{1+}\\)", "", Jockey),
         Jockey = gsub("kg$","",Jockey),
         Trainer = gsub(" {1+}","",Trainer),
         Trainer = gsub("[^[:alpha:] ]", "", Trainer),
         trackcondition = gsub(" {1+}","",trackcondition),
         trackcondition = gsub("\\_{1+}","",trackcondition),
         trackcondition = gsub(":{1+}","",trackcondition),
         trackcondition = gsub("TrackConditionHeavy1", "TrackConditionHeavy10",trackcondition),
         win_prize = as.numeric(gsub(" 2nd ","",
                                     gsub(",","",str_split(character, "\\$") %>% map_chr(., 3)))),
         prize_pool = as.numeric(gsub(".1st ","",
                                      gsub(",","",str_split(character, "\\$") %>% map_chr(., 2))))
  ) # %>%

results_pred_winnings <- inner_join(nn_test_df_preds_glm %>%
                                      dplyr::select(Horse,
                                             odds,
                                             pred_finish), race_results_combined, by = c("RaceID","Horse")) %>%
  na.omit() %>%
  arrange(RaceID,pred_finish) %>%
  filter(pred_finish ==1)

nn_test_df_preds_glm <- cbind(preds, test_model) %>%
  group_by(RaceID) %>%
  arrange(RaceID,
          preds) %>%
  mutate(pred_finish = 1:n())


race_tomorrow_combined_test_summary <- race_tomorrow_combined_test %>%
  group_by(RaceID) %>%
  summarise(RaceID,
            count= n()) %>%
  unique()

nn_test_df_preds_summary <- nn_test_df_preds_glm %>%
  group_by(RaceID) %>%
  summarise(RaceID,
            count= n()) %>%
  unique()

race_numbers <- full_join(race_tomorrow_combined_test_summary, nn_test_df_preds_summary, by = "RaceID") %>%
  mutate(perc = (count.y/count.x)*100) %>%
  filter(perc >=90)


bets_preds <- inner_join(race_numbers, results_pred_winnings, by = c("RaceID"))


bets_results <- inner_join(race_results_combined, bets, by = c("RaceID","Horse"))


#June22_results <- time_function(race_results_combined)
#June22_preds <- race_tomorrow_combined_pred
results <- race_results_combined %>%
  inner_join(bets, by = c("RaceID","Horse")) %>%
  group_by(RaceID) %>%
  filter(!is.na(Finish)) %>%
  arrange(RaceID, Finish,
          RaceID) %>%
  #mutate(pred_finish = 1:n()) %>%
  ungroup() #%>%
  filter(pred_finish ==1) %>%
  summarise(accurate = sum(ifelse(Finish == pred_finish,1,0)),
            count = n(),
            perc = accurate/count)


results_bet <- results %>%
  filter(RaceID %in% race_numbers_bet$RaceID) %>%
  filter(pred_finish==1)

#June23_all_preds <- race_tomorrow_combined_pred
#June23_race_preds <- race_tomorrow_combined_pred_bet
#June23_results <- time_function(race_results_combined)

June23_race_preds_winners <- June23_race_preds %>%
  filter(pred_finish %in% c(1,2)) %>%
  inner_join(June23_results, by = c("info"="info", "Race"="Race", "No"="No.")) %>%
  filter(!is.na(Finish)) %>%
  ungroup() %>%
  mutate(place_win = ifelse(Finish<=3,1,0)) %>%
  #group_by(RaceID) %>% 
  summarise(place_win = sum(place_win),
            Bets = n(),
            perc_place = place_win/Bets)

###
horse_mod <- lmer(time ~ distance + trackcondition + Bar + (1|Bar) +(1|trackcondition) +(1|Weight)+ (1|Jockey) +(1+distance|Horse), data = train_model)
pred_times <- predict(horse_mod, test_model)
test_model_preds <- cbind(test_model, pred_times) %>%
  group_by(RaceID) %>%
  arrange(time) %>%
  mutate(finish = row_number()) %>%
  arrange(pred_times) %>%
  mutate(pred_finish = row_number()) %>%
  filter(pred_finish == 1) %>%
  ungroup() %>%
  summarise(number_correct <- sum(ifelse(finish<2,1,0)),
            total_races = n(),
            perc_correct = number_correct/total_races)

ggplot(data.frame(Effect(c("distance","Horse"),m1)),
       aes(x=distance,y=time,color=Horse,group=Horse))+
  geom_line()



##########################################################


#Neural Network
library(neuralnet)
library(nnet)

Horse_labels <- class.ind(as.factor(train_model$Horse))
Jockey_labels <- class.ind(as.factor(train_model$Jockey))
Trainer_labels <- class.ind(as.factor(train_model$Trainer))
Track_labels <- class.ind(as.factor(train_model$trackcondition))

train_model_numeric <- train_model %>%
  select(-Horse,
         -Jockey,
         -Trainer,
         -trackcondition)

standardiser <- function(x){
  (x-min(x))/(max(x)-min(x))
}

train_model_numeric[,1:ncol(train_model_numeric)] <- lapply(train_model_numeric[,1:ncol(train_model_numeric)], standardiser)
train_model_df <- cbind(Horse_labels,Jockey_labels, Trainer_labels, Track_labels,train_model_numeric)


train_col_list <- paste(c(colnames(train_model_df[,1:ncol(train_model_df)-1])),collapse="+")
library(glue)
f <- glue("time ~ {train_col_list}")
#f <- as.formula(f)

time_net <- neuralnet(f, data = train_model_df, hidden = c(5, 2))

#plot(time_net)

## test model df
Horse_labels <- class.ind(as.factor(test_model$Horse))
Jockey_labels <- class.ind(as.factor(test_model$Jockey))
Trainer_labels <- class.ind(as.factor(test_model$Trainer))
Track_labels <- class.ind(as.factor(test_model$trackcondition))

test_model_numeric <- test_model %>%
  select(-Horse,
         -Jockey,
         -Trainer,
         -trackcondition)
train_model_numeric <- train_model %>%
  select(-Horse,
         -Jockey,
         -Trainer,
         -trackcondition)

standardiser <- function(x){
  (x-min(x))/(max(x)-min(x))
}

test_model_numeric[,c(1:2,ncol(test_model_numeric))] <- lapply(test_model_numeric[,c(1:2,ncol(test_model_numeric))], standardiser)
test_model_numeric$distance <- (test_model_numeric$distance/min(train_model$distance))/(max(train_model$distance)-min(train_model$distance))
test_model_df <- cbind(Horse_labels,Jockey_labels, Trainer_labels, Track_labels,test_model_numeric[,1:3])


test_model_df$time_preds <- compute(time_net, test_model_df)






nn_train_matrix <- model.matrix(~time+Horse+
                                  Finish+
                                  Bar+
                                  Weight+
                                  distance+
                                  trackcondition,
                                data = train_model_data)
colnames(nn_train_matrix)

col_list <- paste(c(colnames(nn_train_matrix[,-c(1,21)])),collapse="+")
col_list <- paste(c("time~",col_list),collapse="")
f <- formula(col_list)

#n <- names(train_model_data)
#f <- as.formula(paste("time~", paste(n[!n %in% "time"], collapse = " + ")))

nn <- neuralnet(f, data=nn_train_matrix,threshold = 0.01,
                learningrate.limit = NULL,
                learningrate.factor = list(minus = 0.5, plus = 1.2),
                algorithm = "rprop+")
nn$result.matrix
plot(nn)

nn_test_matrix <- model.matrix(~time+Horse+
                                  Finish+
                                  Bar+
                                  Weight+
                                  distance+
                                  trackcondition,
                                data = test_model)
colnames(nn_train_matrix)
colnames(nn_test_matrix)

compute(nn,nn_test_matrix[,-2])

test_model$Pred_time <- predict(nn,
                                nn_test_matrix,
                                hidden = c(8,5,3),
                                linear.output = F)

test_model <- test_model %>%
  arrange(Pred_time) %>%
  mutate(Pred_finish = 1:n())


################################

## Assessing how often the favourite wins
fav_win_odds <- df3 %>%
  filter(!is.na(odds)) %>%
  group_by(Date_format) %>%
  #group_by(Day_of_week) %>%
  summarise(num_fav_winners = sum(fav_winner, na.rm = T),
            races = length(unique(RaceID)),
            fav_win_odds = (num_fav_winners/races)*100) %>%
  unique()


##Plotting favourite winning rate per day over the previous month
ggplot(fav_win_odds) +
  geom_line(aes(x=Date_format, y = fav_win_odds))+
  geom_line(aes(x=Date_format, y = mean(fav_win_odds)), colour = "blue")+
  labs(title = "Percentage of favourites that win each day",
       x= "Date",
       y = "percentage of favourites to win")+
  theme_minimal()

mean(fav_win_odds$fav_win_odds)

#########
##Checking how often one of the top 4 horses in odds wins - 
##read about dutch betting and interested to see if it would pay off
df4 <- df3 %>%
  filter(!is.na(odds)) %>%
  arrange(Datetime,
          RaceID,
          odds,
          Bar.) %>%
  group_by(RaceID) %>%
  mutate(Favourite_Number = 1:n(),
         Dutch_bet = ifelse(Favourite_Number<3,1,0),
         Horse_Prob = 1/odds) %>%
  filter(Dutch_bet ==1) %>%
  mutate(Dutch_bet_ind = ifelse(sum(Horse_Prob)>=1,0,1),
         total_bet = 5,
         Bet = (Horse_Prob/sum(Horse_Prob))*total_bet,
         outcome = ifelse(winner ==1, Bet*odds-Bet, -Bet),
         top4_winner = ifelse(sum(winner)==1,1,0),
         race_outcome = sum(outcome)) %>%
  filter(Dutch_bet_ind ==1) %>%
  ungroup() %>%
  mutate(profit = cumsum(outcome)) %>%
  group_by(Date_format) %>%
  mutate(daily_profit = sum(race_outcome))

df5 <- df4 %>%
  group_by(RaceID) %>%
  filter(Favourite_Number==1) %>%
  ungroup() %>%
  mutate(Race_profit = cumsum(race_outcome)) %>%
  summarise(fav_winners = sum(top4_winner),
            count = n(),
            perc_winner = fav_winners/count)

ggplot(df5) +
  #geom_histogram(aes(race_outcome), bins = 100)
  geom_line(aes(x = Datetime, y = Race_profit))


###########
#Checking Micks fav numbers
t1 <- df3 %>%
  filter(!is.na(odds),
   !Day_of_week %in% c('Sat',"Wed"),
         grepl("Heavy",trackcondition)==F,
         !is.na(odds)
  ) %>%
  arrange(Datetime,
          RaceID,
          odds,
          Bar.) %>%
  group_by(RaceID) %>%
  mutate(Favourite_Number = 1:n()) %>%
  filter(Favourite_Number ==1) %>%
  mutate(winner = ifelse(Finish ==1,1,0),
         Prize_pool = as.numeric(gsub(" 2nd ","",
                                     gsub(",","",str_split(character, "\\$") %>% map_chr(., 3))))
         ) %>%
  ungroup() %>%
  filter(Race %in% c('Race 1','Race 2','Race 3'),
         Prize_pool<=15000) %>%
  summarise(races = n(),
            fav_wins = sum(winner),
            perc_fav_win = fav_wins/races)





########

