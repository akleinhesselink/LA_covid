rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

community_names <- read_csv('data/community_names.csv')
updates <- dir( 'data/update_archive', '*.html', full.names = T)

temp <- list()
cases_dat <- list() 

for( i in 1:length(updates)) { 
  
  cur_date <- ymd( str_extract( updates[i], '2020[0-9 -]+') )
  
  temp[[i]] <- 
    read_html(updates[i]) %>% 
    html_nodes('li') %>% 
    html_text() 
    
  cases_dat[[i]] <- 
    data.frame( text = 
                  temp[[i]][ temp[[i]] %>% 
                               str_trim(.) %>% 
                               str_detect( . , '^[A-Z]+.*\\d+$') ] ) %>% 
    mutate( text = str_trim( text )) %>% 
    mutate( community = str_extract( text, "^[A-Za-z -\\.]+" ), 
            cases = str_extract(text, '\\d+$')) %>%
    separate( community, c('city', 'neighborhood'), sep = '[-]+ ') %>% 
    mutate( date = cur_date) %>% 
    mutate( city = str_trim(city) , neighborhood = str_trim(neighborhood )) 
  
}

cases_dat  <- 
  do.call( bind_rows, cases_dat ) %>% 
  mutate(neighborhood = ifelse( is.na(neighborhood) | neighborhood == "", city, neighborhood)) %>% 
  select(date,  city, neighborhood, cases)  %>% 
  mutate(neighborhood = str_remove( neighborhood, '\\*')) %>% 
  mutate(neighborhood = str_remove( neighborhood, '^[Cc]ity of ')) %>% 
  filter( complete.cases( . ) )  %>% 
  distinct() %>% 
  group_by( date, city, neighborhood) %>%
  mutate( cases = as.numeric( str_trim(cases))) %>% 
  filter( cases == max(cases)) %>% 
  filter( !str_detect( neighborhood, 'ospital|death|[iI]nvesti'))  %>% 
  ungroup() %>% 
  mutate( neighborhood = str_remove(neighborhood, ' --$'))

latest_update <- max(cases_dat$date)

date_seq <- seq.Date(ymd('2020-03-10'), latest_update, by = 1)

case_grid <- expand.grid( date = date_seq  ,  neighborhood = community_names$name)

neighborhood_data <- 
  case_grid %>% 
  left_join(cases_dat) %>% 
  arrange( neighborhood, date )  %>% 
  write_csv(paste0('data/cases-by-neighborhood-', latest_update, '.csv'))


