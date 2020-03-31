rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

# This processes the data between 2020-03-17 and 2020-03-28.  
# After 2020-03-29 LA Co. Public Health changed output style. 
# Use New scraper for those

last_date <- '2020-03-28'
updates <- dir( 'data/update_archive', '*.html', full.names = T)

cases_dat <- list() 

posting_dates <- lapply( updates, function(x) ymd( str_extract( x, '2020[0-9 -]+' )))

updates <- 
  updates[ unlist( lapply( posting_dates, function(x)  x <= last_date )) ]  # filter out updates after 2020-03-28

# updates 1 use below: 

cases_dat[[1]] <- 
  read_html(updates[1]) %>% 
  html_nodes( xpath ="//body/table[2]/tr[2]/td/ul[4]/li" ) %>% 
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '--') 

# updates 2-4: 
for( i in 2:4 ) { 
  cases_dat[[i]] <- 
    read_html(updates[i]) %>% 
    html_nodes( xpath ="//body/table[2]/tr[2]/td/ul[3]/li" ) %>% 
    html_text() %>% 
    data.frame( cases = . )  %>% 
    filter( !str_detect(cases , 'Investigat')) %>% 
    separate(cases, c('community', 'cases'), sep = '--') 
}

# update 5 
cases_dat[[5]] <- 
  read_html(updates[5]) %>% 
  html_nodes( xpath ="//body/table[2]/tr[2]/td/li" ) %>% 
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '\\t') 

# updates 6 - 7 current use below: 
for( i in 6:7 ) { 
  cases_dat[[i]] <- 
    read_html(updates[i]) %>%
    html_nodes(xpath ="//body//table[2]//td//ul[6]//li") %>%
    html_text() %>% 
    data.frame( cases = . ) %>% 
    filter( !str_detect(cases , 'Investigat')) %>% 
    separate(cases, c('community', 'cases'), sep = '\\t')
}

# update 8  ------------ # 
cases_dat[[8]] <- 
  read_html(updates[8]) %>%
  html_nodes(xpath ="//body//table[2]//td//ul[5]//li") %>%
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '\\t')

# updates 9 - current --------- # 


for(i in 9:length(updates)){ 
    cases_dat[[i]] <- 
      read_html(updates[i]) %>%
      html_nodes(xpath ="//body//table[2]//td//ul[6]//li") %>%
      html_text() %>% 
      data.frame( cases = . ) %>% 
      filter( !str_detect(cases , 'Investigat')) %>% 
      separate(cases, c('community', 'cases'), sep = '\\t') 
}

LA_LBC_PASADENA_cases <- list()
LA_LBC_PASADENA_cases[[1]] <- 
  read_html(updates[1]) %>%
  html_nodes(xpath ="//body//table[2]//td//ul[3]//li")  %>% 
  html_text()  %>% 
  data.frame( cases = . ) %>%
  separate(cases, c('community', 'cases'), sep = '--') %>% 
  mutate_all( .fun = function(x) str_trim(str_to_upper(x))) %>% 
  mutate( cases = as.numeric(str_extract( cases, '\\d+')))

for( i in 2:length(updates)){ 
  LA_LBC_PASADENA_cases[[i]] <- 
    read_html(updates[i]) %>%
    html_nodes(xpath ="//body//table[2]//td//ul[2]//li") %>%
    html_text() %>%
    data.frame( cases = . ) %>%
    separate(cases, c('community', 'cases'), sep = '--') %>% 
    mutate_all( .fun = function(x) str_trim(str_to_upper(x))) %>% 
    mutate( cases = as.numeric(str_extract( cases, '\\d+')))
}  



for( i in 1:length(cases_dat)){ 
  
  cases_dat[[i]]$date <- posting_dates[[i]]
  LA_LBC_PASADENA_cases[[i]]$date <- posting_dates[[i]]
  
  cases_dat[[i]] <- 
    cases_dat[[i]] %>% 
    transmute_all( .funs = str_trim ) %>% 
    mutate( community = str_to_upper(community)) %>% 
    mutate( cases = as.numeric(str_extract(cases, "\\d+"))) %>% 
    mutate( community = str_remove(community, '\\*')) %>% 
    separate( community, c('region', 'community'), sep = ' - ', fill = 'left') %>% 
    mutate( community = str_remove( community, '^CITY OF ')) 
  
}


do.call( bind_rows, cases_dat  ) %>% 
  mutate( community = ifelse( str_detect(community, '<'), 'OTHER', community)) %>% 
  arrange( community, date ) %>% 
  filter( !community == '')  %>% 
  filter( !community %in% c('LONG BEACH', 'PASADENA', 'OTHER')) %>% 
  mutate( date = ymd(date)) %>% 
  bind_rows( 
    do.call(bind_rows, LA_LBC_PASADENA_cases) %>% 
      mutate( region = community)) %>% 
  write_csv(paste0( 'data/temp/raw-data-scraped-', last_date, '.csv'))

