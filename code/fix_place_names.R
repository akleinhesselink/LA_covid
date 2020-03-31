rm(list = ls())

library(tidyverse) 
library(lubridate)
library(sf)

name_replacements <- c('PARK LABREA' = "PARK LA BREA", 
                       "^PICO$" = "PICO-UNION", 
                       "^TEMPLE$" = "TEMPLE-BEAUDRY", 
                       "^ATHENS$" = "ATHENS-WESTMONT", 
                       "^FLORENCE$" = "FLORENCE-FIRESTONE")

load('data/temp/BASA_shapes.rda')

scraped_data <- dir('data/temp', pattern = 'raw-data-scraped-.*.csv', full.names = T)

most_recent_scrape <- scraped_data[ which.max( file.mtime(scraped_data) ) ] 

cases_dat <- 
  read_csv(most_recent_scrape) %>% 
  mutate( community = str_replace_all(community, pattern = name_replacements))

BASA_names <- 
  BASA %>% 
  st_drop_geometry() %>% 
  select(region, community) 

communities_with_blank_regions <- 
  cases_dat %>% 
  distinct(region, community) %>% 
  group_by( community) %>% 
  filter( 1 < n_distinct(region)) %>% 
  filter( any(is.na(region)))  %>% 
  distinct(community )

distinct_community_names <- 
  BASA_names %>%
  distinct(region, community) %>% 
  group_by( community ) %>% 
  filter( n_distinct(region) == 1 ) 

fix1 <- 
  distinct_community_names %>% 
  left_join(communities_with_blank_regions, by = 'community') %>% 
  distinct() %>% 
  rename( 'region_fix' = region )

cases_dat <- 
  cases_dat %>%
  left_join(fix1, by = 'community')  %>% 
  mutate( region = ifelse( is.na(region), region_fix, region)) 

missing_region <- 
  cases_dat %>% 
  filter( is.na(region)) %>% 
  distinct(community)

fix2 <- 
  missing_region %>% 
  left_join(BASA_names, by = 'community') %>% 
  distinct(region, community) %>% 
  filter( region != 'UNINCORPORATED') %>% 
  rename( 'region_fix2' = region)

cases_dat <- 
  cases_dat %>% 
  left_join(fix2, by = 'community') %>% 
  mutate( region = ifelse( is.na(region), region_fix2, region)) %>% 
  filter( ! is.na(region) ) %>% 
  select( region, community, cases, date ) %>% 
  arrange( region, community, date ) 

latest_date <- max( cases_dat$date )

write_csv( cases_dat, 
           path = paste0( 'data/temp/processed-cases-update-', latest_date, '.csv'))

