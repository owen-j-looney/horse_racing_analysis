library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(curl)
library(glue)
library(ggplot2)

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

#naming each group of tables with their date and location
names(race_tables) <- sub('.*Key=','',racing_results_urls)

#removing abandoned races with no data
race_tables_non_abndn <- Filter(function(x) length(x) > 1,race_tables)


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
      as.character(race_tables_non_abndn[[n]][[i]])
      } else {
        race_tables_non_abndn[[n]][[i]] <- race_tables_non_abndn[[n]][[i]] %>%
          mutate(character = as.character(race_tables_non_abndn[[n]][[i-1]]),
                 info = glue("{names(race_tables_non_abndn)[[n]]},race{i}"))
      }
  }
}


race_results_list <- flatten(race_tables_non_abndn)

filtered_race_results_list <- Filter(function(x) ncol(x) == 13,race_results_list)

race_results_combined <- do.call("rbind", filtered_race_results_list) 

race_results_combined <- race_results_combined %>%
  select(-Colour,
         -Penalty) %>%
  mutate(odds = as.numeric(gsub(".*?([0-9.]+).*", "\\1",`Starting Price`)),
         Date_format = as.Date(str_split(info, ",") %>% map_chr(., 1),
                               format('%Y%b%d')),
         State = str_split(info, ",") %>% map_chr(., 2),
         Racecourse = str_split(info, ",") %>% map_chr(., 3),
         Race = str_split(info, ",") %>% map_chr(., 4),
         Day_of_week = format(Date_format, '%a')) %>%
  na.omit() 

saveRDS(race_results_combined, glue("C:/Users/owenl/Documents/Owen/R_git/Horse_racing_analysis/Race_results_last_month_{Sys.Date()}.rds"))
############### Results saved as RDS on local drive to allow combination of race results over longer periods in future ##########
##### End of script run to pull and clean race data into useable format
#### more can be done like converting dates etc.

