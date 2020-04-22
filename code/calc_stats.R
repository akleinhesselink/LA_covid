rm(list = ls() )
library(tidyverse) 
library(lubridate)
library(sf)

processed_data_files <- 
  dir('data/temp', pattern = 'processed-cases-update-2.*.csv$', 
      full.names = T)

latest_update <- processed_data_files[ which.max( file.mtime(processed_data_files) ) ] 

load('data/temp/BASA_shapes.rda')

cases <- read_csv(latest_update)

most_recent_update <- max( cases$date  )

la_county <- 
  cases  %>% 
  filter( community %in% c('LOS ANGELES COUNTY (EXCL. LB AND PAS)', 'PASADENA', 'LONG BEACH')) %>% 
  left_join(BASA %>% st_drop_geometry() %>% select( OBJECTID, region, community, POPULATION )) %>% 
  group_by( date ) %>%
  summarise( cases = sum(cases) ) %>% 
  mutate( region = 'LOS ANGELES COUNTY', 
          POPULATION = sum(BASA$POPULATION) ) 

# manually change the total for 2020-04-07 to match 
# LA Co. Public Health total.  There itemized total 
# adding up Long Beach, Pasadena and Los Angeles Co. 
# does not match the total they report in this update. 

la_county <- 
  la_county %>% 
  ungroup() %>% 
  mutate( cases = ifelse( date == '2020-04-07', 6910, cases)) %>% 
  arrange( date ) %>% 
  mutate( new_cases = cases - lag(cases),
          cases_per_1k = (cases/POPULATION)*1000) %>% 
  mutate( new_cases = ifelse(new_cases < 0 , NA, new_cases)) %>% 
  rename( `Total cases` = cases, 
          `New cases` = new_cases,
          `Cases per thousand` = cases_per_1k, 
          "Population" = POPULATION) 

basic_stats <- 
  expand.grid( date = seq.Date( min(cases$date), max(cases$date), by = 1 ), OBJECTID = BASA$OBJECTID ) %>% 
  data.frame()  %>% 
  left_join( BASA %>% st_drop_geometry() %>% select(OBJECTID, region, community, POPULATION) ) %>%  
  left_join( cases )  %>% 
  mutate( cases_per_1k = (cases/POPULATION)*1000 ) 

basic_stats <- 
  basic_stats %>% 
  mutate( label = ifelse( region == "UNINCORPORATED", paste( community, "Unincorporated", sep = ' --\n'), community) ) %>% 
  mutate( label = str_to_title(label)) %>% 
  group_by( label) %>% 
  arrange(label, date) %>% 
  mutate( new_cases = cases - lag(cases)) %>%
  mutate( new_cases = ifelse(new_cases < 0 , NA, new_cases)) %>% 
  rename( `Total cases` = cases, 
          `New cases` = new_cases, 
          `Cases per thousand` = cases_per_1k, 
          "Population" = POPULATION) 

map_data <- 
  BASA %>% 
  left_join(
  basic_stats %>% 
    group_by( OBJECTID, region, community )  %>% 
    filter( date == most_recent_update) ) 

# basic_stats <-
#   basic_stats %>%
#   filter( !is.na(Cases)) # filter out rows with NA's for timeseries figures

# Transform to long-format data frames 

la_county <- 
  la_county %>% 
  select( - Population, - `Cases per thousand`) %>% 
  arrange( date ) %>% 
  mutate( `Growth rate` = log(`Total cases`) - log(lag(`Total cases`))) %>% 
  gather( variable, value , c(`Total cases`, `New cases`, `Growth rate`)) %>% 
  distinct()

basic_stats <- 
  basic_stats %>% 
  select( - Population, -`Cases per thousand` ) %>% 
  group_by(OBJECTID) %>% 
  arrange( date ) %>% 
  mutate( `Growth rate` = log(`Total cases`) - log(lag(`Total cases`))) %>% 
  mutate( `Growth rate` = ifelse( is.na(`Growth rate`) | `Growth rate` < 0 | `Total cases` < 20, NA, `Growth rate`)) %>% 
  gather( variable, value , c(`Total cases`, `New cases`, `Growth rate`)) %>% 
  left_join(la_county %>% select(-region), by = c('date', 'variable')) %>% 
  rename(value = value.x, 
         countywide = value.y) %>% 
  distinct()

la_county <- 
  la_county %>% 
  mutate( plot_note = '') %>% 
  mutate( plot_note = ifelse (date >= '2020-04-20', "*Includes case backlog", plot_note))

basic_stats <- 
  basic_stats %>% 
  mutate( plot_note = '') 
  
save(la_county, basic_stats, map_data, file = 'app/data/case_data.rda')
save(la_county, basic_stats, file = 'data/temp/case_data_copy.rda')
