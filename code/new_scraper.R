rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

todays_date <- lubridate::today()
update_filename <- paste0( 'data/update_archive/update-', todays_date, '.html')

base_URL <- "http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm"

output_file <- paste0( 'data/temp/raw-data-scraped-', todays_date, '.csv')

if( !file.exists( update_filename)){ 
  download_html(base_URL, update_filename)
}

# Update records # 
update_table <- read_csv('data/update_table.csv')

update_table %>% 
  bind_rows(data.frame( date = todays_date, url = base_URL) ) %>% 
  distinct() %>% 
  write_csv('data/update_table.csv')
# ----------- # 

cases <- 
  read_html(update_filename ) %>% 
  html_table() %>% .[[1]]

cases <- 
  cases %>% 
  mutate( blank_rows = Locations == '' & `Total Cases` == "" ) %>% 
  mutate( section = cumsum( blank_rows ) ) %>% 
  filter( !blank_rows ) %>% 
  split( .$section)
  
LA_LBC_PASADENA_cases <- 
  cases[[1]] %>% 
  filter( !str_detect(Locations, "Laboratory"))  %>% 
  mutate_all( .fun = function(x) str_trim(str_to_upper(x))) %>% 
  mutate( cases = as.numeric(str_extract( `Total Cases`, '\\d+'))) %>% 
  mutate( Locations = str_remove_all(Locations, '\\*')) %>%
  mutate( Locations = str_squish( str_trim( str_remove_all(Locations, '-')))) %>% 
  mutate( community = Locations ) %>% 
  mutate( region = community) %>% 
  mutate( date = todays_date)

cases_dat <-   
  cases[[5]] %>% 
  filter( !str_detect(Locations , '[Ii]nvestigation')) %>%
  filter( !str_detect(Locations, "CITY/COMMUNITY")) %>% 
  transmute_all( .funs = str_trim ) %>% 
  mutate( community = str_to_upper(Locations)) %>% 
  mutate( cases = as.numeric(str_extract(`Total Cases`, "\\d+"))) %>%
  mutate( community = str_remove_all(community, '\\*')) %>% 
  separate( community, c('region', 'community'), sep = ' - ', fill = 'left') %>% 
  mutate( community = str_remove( community, '^CITY OF ')) %>% 
  mutate( date = todays_date) %>% 
  filter( !community %in% c('PASADENA', 'LONG BEACH', 'LOS ANGELES')) ### REMOVE ROWS FOR THESE: data reported in previous table

cases_dat %>% 
  select( date, region, community, cases ) %>% 
  bind_rows(LA_LBC_PASADENA_cases %>% select( date, region, community, cases)) %>%
  write_csv(output_file)

