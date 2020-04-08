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

la_county <- 
  cases  %>% 
  filter( community %in% c('LOS ANGELES COUNTY (EXCL. LB AND PAS)', 'PASADENA', 'LONG BEACH')) %>% 
  group_by( date ) %>%
  summarise( total_cases = sum(cases) ) %>% 
  mutate( region = 'LOS ANGELES COUNTY')

# manually change the total for 2020-04-07 to match 
# LA Co. Public Health total.  There itemized total 
# adding up Long Beach, Pasadena and Los Angeles Co. 
# does not match the total they report in this update. 

la_county <- 
  la_county %>% 
  mutate( total_cases = ifelse( date == '2020-04-07', 6910, total_cases))

basic_stats <- 
  expand.grid( date = seq.Date( min(cases$date), max(cases$date), by = 1 ), OBJECTID = BASA$OBJECTID ) %>% 
  data.frame()  %>% 
  left_join( BASA %>% st_drop_geometry() %>% select(OBJECTID, region, community, POPULATION) ) %>%  
  left_join( cases )  %>% 
  mutate( cases_per_1k = (cases/POPULATION)*1000 ) 

most_recent_update <- max( cases$date  )

basic_stats <- 
  basic_stats %>% 
  mutate( label = ifelse( region == "UNINCORPORATED", paste( community, "Unincorporated", sep = ' --\n'), community) ) %>% 
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

# basic_stats <-
#   basic_stats %>%
#   filter( !is.na(Cases)) # filter out rows with NA's for timeseries figures

save(la_county, basic_stats, map_data, file = 'app/data/case_data.rda')

