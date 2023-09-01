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
  purrr::flatten() 

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
  dplyr::select(-Colour,
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
         track_condition = as.factor(as.character(str_extract_all(race_results_combined$character,"Track Condition: .* Time:"))),
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         winning_time = gsub("Time: ","",str_extract_all(character,"Time: \\d+:\\d+.\\d+ "))
  ) %>%
  na.omit() 

saveRDS(race_results_combined, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/Race_results_last_month_{Sys.Date()}.rds"))



trial_results_combined <- trial_results_combined %>%
  dplyr::select(-Colour,
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
         track_condition = as.factor(as.character(str_extract_all(character,"Track Condition: .* Time:"))),
         distance = as.numeric(str_replace_all(distance, c("\\(" = "", "METRES\\)" = ""))),
         winning_time = gsub("Time: ","",str_extract_all(character,"Time: \\d+:\\d+.\\d+ "))
  )
saveRDS(trial_results_combined, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/Trial_results_last_month_{Sys.Date()}.rds"))

#both trial and race data contain the same columns, so an analysis can be run on both combined if required

############### Results saved as RDS on local drive to allow combination of race results over longer periods in future ##########
##### End of script run to pull and clean race data into useable format
#### more can be done like converting dates etc.

setwd("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/sectional_positions")

library(tidyverse)
library(reactable)
library(reshape2)
library(glue)
library(dplyr)
library(caret)
library(lubridate)

df_sec <- list.files(pattern = ".rds") %>%
  map(readRDS)


df1_sec <- df_sec %>%
  bind_rows %>%
  unique()



library(rvest)
library(lubridate)
library(glue)
library(furrr)

future::plan(multisession, workers = 4)

base_url <- 'https://www.racingzone.com.au/results/'


#getting required urls
#base_url <- "https://www.racingaustralia.horse/FreeFields/Calendar_Results.aspx"

date_list <- c(as.character(seq.Date(max(df1_sec$Date_format)+1,Sys.Date(), by = 1)))

#date_list <- c(as.character(seq.Date(as.Date('2022-10-21'),as.Date('2022-11-21'), by = 1)))
#creating blank list so i can get a url for race results for each state
date_list_url <- c()

#generating urls for each states racing results over the last month
for (i in 1:length(date_list)) {
  date_list_url[[i]] <- paste0(base_url,date_list[i],"/")
}

#generating a list of urls to run through to get all race data for last month
results <- function(url_list){
  paste0('https://www.racingzone.com.au/',
         read_html(url_list) %>%
           html_nodes("div.race-loader")%>%
           html_nodes("a") %>%     
           # find all links in the page
           html_attr("href") %>%
           unique() #%>%
           #str_subset("/results/.+/\\d+-[:alpha:]+.")
  )
}

#getting list of all urls with race data and converting to one list
sec_results_url <- future_map(date_list_url, results,.progress = T) 

sec_results_urls <- sec_results_url %>%
  purrr::flatten()

sec_results_urls <- sec_results_urls[!sec_results_urls %in% c("https://www.racingzone.com.au/results/","https://www.racingzone.com.au" )]

Date_table <- c()

for (i in 1:length(sec_results_url)){
  for (n in 1:length(sec_results_url[[i]])){
    Date_table[[i]] <- list(rep(date_list[i], each = n))
  }
}

Date_tables <- Date_table %>%
  purrr::flatten() %>%
  purrr::flatten() 

#creating function to extract all tables from each url 
read_html_tbls <- function(url) {
  html <- read_html(url) %>% 
    html_nodes("table") %>% 
    html_table()
  
  
  #close(url)
  return(html)
}


#creating blank list for race tables to be appended to
race_secs <- c()

#appending all race tables
race_secs <- future_map(sec_results_urls,read_html_tbls,.progress = T)

race_sectionals <- c() 

for (i in 1:length(race_secs)){
  race_sectionals[[i]] <- as.data.frame(race_secs[[i]][1])
  if (ncol(as.data.frame(race_secs[[i]][1]))==6){
    race_sectionals[[i]] <- cbind(as.data.frame(race_secs[[i]][1]),
                                  data.frame(matrix(nrow = nrow(as.data.frame(race_secs[[i]][1])), ncol = 6)))
  }
  colnames(race_sectionals[[i]]) <- c("Finish",
                                      "Margin",
                                      "Colours",
                                      "Name",
                                      "SP",
                                      "Bar",
                                      "Weight",
                                      "Settle_pos",
                                      "pos_1200m",
                                      "pos_800m",
                                      "pos_400m",
                                      "Stewards_Comments")
}

#generating list of dates for each row in race sectionals
Dates <- c()

for (i in 1:length(race_sectionals)){
  Dates[[i]] <- data.frame(c(rep(Date_tables[[i]], nrow(race_sectionals[[i]]))))
}

c1 <- do.call(rbind,race_sectionals)
c2 <- do.call(rbind, Dates) 
names(c2) <- "Date"

c3 <- cbind(c1,c2)

c4 <- c3 %>%
  filter(!is.na(pos_400m)) %>%
  mutate(trainer = str_split(Name,"-") %>% map_chr(., 2),
         Horses= str_split(Name,"\\d[:alpha:]") %>% map_chr(.,1),
         Horse= str_to_upper(gsub(" ","",Horses)),
         Horse = gsub("\n","",Horse),
         Horse = gsub("'","",Horse),
         Horse = gsub("\\d+","",Horse),
         Date_format = as.Date(Date),
         Settle_pos = gsub("-",NA,Settle_pos)) %>%
  select(Horse, Date_format, Bar, Settle_pos, pos_1200m, pos_800m, pos_400m)

saveRDS(c4, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/sectional_positions/sectional_positions_from_",min(date_list),"_to_", max(date_list),'.rds'))