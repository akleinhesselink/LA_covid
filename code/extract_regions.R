rm(list = ls())

library(tabulizer)
library(tidyverse)

community_names <- tabulizer::extract_tables('data/ncovid19_regions_communities.pdf')

comm_names <- c( community_names[[1]][, 2], 
                 community_names[[2]][, 3], 
                 community_names[[3]][, 3], 
                 community_names[[4]][, 3] )

pops <- c( community_names[[1]][, 4], 
           community_names[[2]][, 6], 
           community_names[[3]][, 6], 
           community_names[[4]][, 6] )


data.frame( community = comm_names, population = pops ) %>% 
  filter( !community == '') %>% 
  mutate( community = as.character(community)) %>% 
  mutate( community = ifelse( is.na(lead(population)) | lead(population) == '' , paste( community, lead(community) ), community )) %>% 
  filter( population != '') %>% 
  mutate( population = str_extract( population, '\\d+') ) %>% 
  filter( !is.na(population)) %>% 
  mutate( community = str_remove( community, '--.*$')) %>% 
  mutate( community = str_trim( str_to_upper(community))) %>% 
  mutate( population = as.numeric(population)) %>% 
  write_csv('data/county_populations.csv')
