rm(list = ls() )
library(tidyverse) 
library(lubridate)

name_replacements <- c('PARK LABREA' = "PARK LA BREA", 
                       "^PICO$" = "PICO-UNION", 
                       "^TEMPLE$" = "TEMPLE-BEAUDRY", 
                       "^ATHENS$" = "ATHENS-WESTMONT", 
                       "^FLORENCE$" = "FLORENCE-FIRESTONE")

cases_dat <- 
  read_csv('data/temp/first_scrape.csv') %>% 
  mutate( community = str_replace_all(community, pattern = name_replacements))

# Process BASA Shapefile -------------------------------- #   
BASA <- read_sf( 'data/BOS_Countywide_Statistical_Areas')

BASA <- 
  BASA %>% 
  separate(LABEL, c('region', 'community'), sep = ' - ', fill = 'left', remove = F) %>% 
  mutate_at( .vars = c('region', 'community'), .fun = function(x) str_trim( str_to_upper (x))) %>% 
  mutate( region = ifelse(str_detect(community, '^CITY OF '), community, region)) %>% 
  mutate_at( .vars = c('region', 'community'), .fun = function(x) str_remove(x, '^CITY OF '))

save( BASA , file = 'data/BASA_shapes.rda')
# -------------------------------------------------------- # 

BASA_names <- 
  BASA %>% 
  st_drop_geometry() %>% 
  select(CITY_TYPE, LCITY, LABEL, region, community) %>% 
  distinct() 

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
           path = paste0( 'data/temp/cases-update-', latest_date, '.csv'))

