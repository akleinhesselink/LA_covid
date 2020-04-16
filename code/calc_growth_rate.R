rm(list = ls() )
library(tidyverse)

load('data/temp/case_data_copy.rda')

growth_per_community <- 
  basic_stats %>% 
  group_by( label) %>% 
  select( - countywide) %>% 
  spread( variable, value) %>% 
  select( - `New cases`) %>% 
  mutate( growth_rate = log(`Total cases`) - log(lag(`Total cases`))) 

growth_per_community %>% View

la_county_growth_rate <- 
  la_county %>% 
  spread( variable, value ) %>% 
  select( - `New cases`) %>% 
  arrange( date ) %>% 
  mutate( growth_rate = log(`Total cases`) - log(lag(`Total cases`)))


la_county_growth_rate %>% 
  ggplot( aes( x = date, y = growth_rate))  + 
  geom_line( color = 'red')  +
  geom_line(data = growth_per_community %>% filter( `Total cases` > 25), aes( x = date, y = growth_rate, group = label), alpha = 0.1)


growth_per_community %>% 
  ggplot( aes( x = date, y = growth_rate, group = label)) + 
  geom_line() 


growth_per_comm_wide <- 
  growth_per_community %>% 
  filter( `Total cases` > 50 ) %>% 
  ungroup() %>% 
  group_by( OBJECTID) %>% 
  filter( sum(!is.na(`growth_rate`)) > 8) %>% 
  select( date, OBJECTID, growth_rate) %>% 
  spread( OBJECTID, growth_rate) 



