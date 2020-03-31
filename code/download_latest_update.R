rm(list = ls()) 
library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

todays_date <- lubridate::today()
base_URL <- "http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm"
latest_download <- max( dir('data/update_archive/', pattern = 'update-.*.html') %>% lubridate::ymd(.)) 

# Get time stamp for update ------------ : 
latest_update <- 
  read_html(base_URL) %>% 
  html_nodes('table') %>% 
  html_nodes('caption') %>% 
  html_text() %>% 
  str_extract(. , "(?<=As of).*") %>% 
  str_trim(.) %>% 
  str_c(. , '/20')  %>% 
  str_split(. , ' ' )
  
latest_update <- 
  paste( latest_update[[1]][[2]], latest_update[[1]][[1]], sep = ' ' ) %>% 
  lubridate::mdy_h(.) %>% 
  force_tz(., tzone = 'America/Los_Angeles')

# ------------------------------------------------- # 

# check whether update is later than latest on system

if( as_date( latest_update ) >  latest_download) { 
  update_filename <- paste0( 'data/update_archive/update-', as_date(latest_update), '.html')
  print(sprintf("downloading latest update from %s", latest_update))
  download_html(base_URL, update_filename)
}else{
  print(sprintf("latest update: %s; latest download: %s", as_date(latest_update), latest_download))
  print('Up to date. Nothing downloaded.')
}

