# 
rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

update_archive_path <- 'data/update_archive'
media_url <- "http://www.publichealth.lacounty.gov/media/Coronavirus/"
update_files <- dir(update_archive_path, pattern = 'update.*.html', full.names = T)
update_table_file <- 'data/update_table.csv'

update_urls <- 
  read_html(media_url) %>% 
  html_nodes('div.card-body') %>% 
  html_nodes('p.card-text') %>%
  html_node('a') %>% 
  html_attr('href') 
  
update_dates <- 
  read_html(media_url) %>% 
  html_nodes('div.card-body') %>% 
  html_nodes('p.card-text') %>%
  html_node('a') %>% 
  html_node('strong') %>% 
  html_text() %>% 
  str_trim() 

update_table <- 
  data.frame( date = update_dates, url = update_urls) %>% 
  filter( complete.cases(. )) %>% 
  mutate( date = mdy( date )) %>% 
  filter( date > "2020-03-16") %>% 
  #filter( date <= "2020-03-28") %>%
  mutate( url = as.character( url )) %>% 
  mutate( update_file_name = paste0( 'update-', date, '-', str_extract(url, 'prid=\\d+'), '.html'))

for( i in 1:nrow( update_table)){ 
  
  if( !file.exists(file.path(update_archive_path, update_table$update_file_name[i])) ){ 
    
    download_html(url = update_table$url[i], 
                  file = file.path(update_archive_path, update_table$update_file_name[i]))
    
  }else{ 
    print(sprintf("Already Downloaded %s", 
                  file.path(update_archive_path, update_table$update_file_name[i] ) ))
  }
}
