# 
rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

media_url <- "http://www.publichealth.lacounty.gov/media/Coronavirus/"
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
  filter( date <= "2020-03-28") %>%
  mutate( url = as.character( url ))

if (file.exists(update_table_file)){ 
  old_table <- read_csv( update_table_file )
  new_updates <- dplyr::setdiff(old_table, update_table)
  
  bind_rows(new_updates, 
            old_table) %>% 
    write_csv(update_table_file)
}else{ 
  update_table %>% 
    write_csv(update_table_file)
}

for( i in 1:nrow(update_table)){ 
  update_fname <- paste0( 'data/update_archive/', 'update', '-', update_table$date[i], '.html')
  
  if( !file.exists(update_fname) ){
    download_html(update_table$url[i], update_fname) 
  }
}



