library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

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

race_results_urls <- map(state_list_url, results) %>%
  flatten()

test1 <- read_html(race_results_urls[[1]]) %>%
  html_table()
  html_nodes("table") %>%
  xml_find_all(xpath = '//*[@id="page-content/table"]')
  #html_nodes(xpath = '//*[@id="page-content"]') %>%
  #html_elements(".race-title") %>%
  html_table()
  html_elements(xpath = '/html/body/div[1]/div/div[4]/div/div/div/table[1]') %>%
  html_table(xpath = "/html/body/div[1]/div/div[4]/div/div/div/table[1]")
  html_children() %>%
  html_attrs()
  html_name()
  html_nodes('table') 
  html_table()
