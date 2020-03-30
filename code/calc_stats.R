
rm(list = ls() )
library(tidyverse) 
library(lubridate)
library(sf)
library(zoo)

cases <- read_csv('data/temp/cases-update-2020-03-28.csv')
load('data/temp/BASA_shapes.rda')


la_county <- 
  cases %>% 
  filter( community %in% c('LOS ANGELES COUNTY (EXCL. LB AND PAS)', 'PASADENA', 'LONG BEACH')) %>% 
  group_by( date ) %>%
  summarise( total_cases = sum(cases) ) %>% 
  mutate( region = 'LOS ANGELES COUNTY')

basic_stats <- 
  expand.grid( date = seq.Date( min(cases$date), max(cases$date), by = 1 ), OBJECTID = BASA$OBJECTID ) %>% 
  data.frame()  %>% 
  left_join( BASA %>% st_drop_geometry() %>% select(OBJECTID, region, community, POPULATION) ) %>%  
  left_join( cases )  %>% 
  mutate( cases = replace_na(cases, 0)) %>% 
  mutate( cases_per_1k = (cases/POPULATION)*1000 ) 

most_recent_update <- max( cases$date  )

basic_stats <- 
  basic_stats %>% 
  mutate( label = ifelse( region == "UNINCORPORATED", paste( community, "Unincorporated", sep = '--\n'), community) ) %>% 
  mutate( label = str_to_title(label)) %>% 
  rename( "Cases" = cases, 
          `Cases per thousand` = cases_per_1k, 
          "Population" = POPULATION) 

map_data <- 
  BASA %>% 
  left_join(
  basic_stats %>% 
    group_by( OBJECTID, region, community )  %>% 
    filter( date == most_recent_update) )


save(la_county, basic_stats, map_data, file = 'app/data/case_data.rda')

