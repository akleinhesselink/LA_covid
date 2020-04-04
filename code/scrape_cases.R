rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

updates <- dir( 'data/update_archive', '*.html', full.names = T)

cases_dat <- list() 

# Format inconsistent for first 8 updates, 
# Bespoke scraping -------------------------------- # 

# filter out other updates: 
skip <- c('2269')

updates <- updates[ -which( str_extract(updates, '(?<=prid=)\\d+') %in% skip) ] 

# update 1 and 2: 
for ( i in 1:2 ) { 
  cases_dat[[i]] <- 
    read_html(updates[i]) %>% 
    html_nodes( xpath ="//body/table[2]/tr[2]/td/ul[4]/li" ) %>% 
    html_text() %>% 
    data.frame( cases = . ) %>% 
    filter( !str_detect(cases , 'Investigat')) %>% 
    separate(cases, c('community', 'cases'), sep = '--') 
}

# updates 3-5: 
for( i in 3:5 ) { 
  cases_dat[[i]] <- 
    read_html(updates[i]) %>% 
    html_nodes( xpath ="//body/table[2]/tr[2]/td/ul[3]/li" ) %>% 
    html_text() %>% 
    data.frame( cases = . )  %>% 
    filter( !str_detect(cases , 'Investigat')) %>% 
    separate(cases, c('community', 'cases'), sep = '--') 
}

# update 6 
cases_dat[[6]] <- 
  read_html(updates[6]) %>% 
  html_nodes( xpath ="//body/table[2]/tr[2]/td/li" ) %>% 
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '\\t') 

# updates 7 - 8 current use below: 
for( i in 7:8 ) { 
  cases_dat[[i]] <- 
    read_html(updates[i]) %>%
    html_nodes(xpath ="//body//table[2]//td//ul[6]//li") %>%
    html_text() %>% 
    data.frame( cases = . ) %>% 
    filter( !str_detect(cases , 'Investigat')) %>% 
    separate(cases, c('community', 'cases'), sep = '\\t')
}

# update 9  
cases_dat[[9]] <- 
  read_html(updates[9]) %>%
  html_nodes(xpath ="//body//table[2]//td//ul[5]//li") %>%
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '\\t')

# updates 10 - most recent --------- # 
# These appear to be released consistently so far. 

for(i in 10:19){ 
    cases_dat[[i]] <- 
      read_html(updates[i]) %>%
      html_nodes(xpath ="//body//table[2]//td//ul[6]//li") %>%
      html_text() %>% 
      data.frame( cases = . ) %>% 
      filter( !str_detect(cases , 'Investigat')) %>% 
      separate(cases, c('community', 'cases'), sep = '\\t', extra = 'drop') 
}

#update 20 on 4-04-20
cases_dat[[20]] <- 
  read_html(updates[20]) %>% 
  html_nodes(xpath = "//body//table[2]//td//ul[7]//li") %>%   
  html_text() %>% 
  data.frame( cases = . ) %>% 
  filter( !str_detect(cases , 'Investigat')) %>% 
  separate(cases, c('community', 'cases'), sep = '\\t', extra = 'drop') 


# Process Long Beach, Pasadena and LA County Separately --- # 
LA_LBC_PASADENA_cases <- list()

for( i in 1:2){ 
  LA_LBC_PASADENA_cases[[i]] <- 
    read_html(updates[i]) %>%
    html_nodes(xpath ="//body//table[2]//td//ul[3]//li")  %>% 
    html_text()  %>% 
    data.frame( cases = . ) %>%
    separate(cases, c('community', 'cases'), sep = '--') %>% 
    mutate_all( .fun = function(x) str_squish(str_trim(str_to_upper(x)))) %>% 
    mutate( cases = as.numeric(str_extract( cases, '\\d+')))
}

for( i in 3:length(updates)){ 
  LA_LBC_PASADENA_cases[[i]] <- 
    read_html(updates[i]) %>%
    html_nodes(xpath ="//body//table[2]//td//ul[2]//li") %>%
    html_text() %>%
    data.frame( cases = . ) %>%
    separate(cases, c('community', 'cases'), sep = '--') %>% 
    mutate_all( .fun = function(x) str_squish(str_trim(str_to_upper(x)))) %>% 
    mutate( cases = as.numeric(str_extract( cases, '\\d+')))
}  

# ---------- Get dates ---------------------- # 
release_date <- NA
for( i in 1:length(updates)){ 
  
  release_date[i] <- 
    updates[i] %>% 
    read_html() %>% 
    html_node(xpath = "//html/body/table[1]") %>% 
    html_text() %>% 
    str_squish(.) %>% 
    str_extract(., '(?<=For Immediate Release).*(?=For more)') %>% 
    lubridate::mdy( . ) %>% 
    as.character()
}

# ---------- Combine ---------------------- # 

for( i in 1:length(cases_dat)){ 
  
  cases_dat[[i]]$date <- release_date[i]
  LA_LBC_PASADENA_cases[[i]]$date <- release_date[i]
  
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
  bind_rows( 
    do.call(bind_rows, LA_LBC_PASADENA_cases) %>% 
      mutate( region = community)) %>%
  mutate( date = ymd(date)) %>% 
  write_csv(paste0( 'data/temp/raw-data-scraped-', max(release_date), '.csv'))

