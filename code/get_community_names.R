library(sp)
library(sf)

neighborhoods <- read_sf('data/la-county-neighborhoods-current')

neighborhoods %>% 
  select( name, display_na) %>% 
  as.data.frame() %>% 
  select( name ) %>% 
  write_csv('data/community_names.csv')