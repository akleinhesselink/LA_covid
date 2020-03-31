rm(list = ls() )
library(tidyverse) 
library(lubridate)
library(sf)

# Process BASA Shapefile -------------------------------- #   
BASA <- 
  read_sf( 'data/BOS_Countywide_Statistical_Areas') %>% 
  st_transform("+proj=longlat +datum=WGS84")

BASA <- 
  BASA %>% 
  separate(LABEL, c('region', 'community'), sep = ' - ', fill = 'left', remove = F) %>% 
  mutate_at( .vars = c('region', 'community'), .fun = function(x) str_trim( str_to_upper (x))) %>% 
  mutate( region = ifelse(str_detect(community, '^CITY OF '), community, region)) %>% 
  mutate_at( .vars = c('region', 'community'), .fun = function(x) str_remove(x, '^CITY OF ')) %>% 
  select( OBJECTID, region, community, POPULATION)

save(BASA , file = 'data/temp/BASA_shapes.rda')
# -------------------------------------------------------- # 
