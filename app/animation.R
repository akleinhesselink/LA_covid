rm(list = ls() )
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)
library(viridis)
library(shiny)
library(plotly)
library(gganimate)
library(gapminder)
library(transformr)


##############
##############

cases <- 
  read_csv('cases-by-neighborhood-2020-03-27.csv') %>%
  filter(date>='2020-03-17')

neighborhood_spatial <- 
  read_sf('la-county-neighborhoods-current/') %>%
  select(name) %>%
  st_transform("+proj=longlat +datum=WGS84")


combined_df <- 
  neighborhood_spatial %>%
  left_join(cases, by=c('name'='neighborhood'))


###################
####################

p<- combined_df %>%
  ggplot() +
  geom_sf(aes(fill=cases, group=date), color="gray90") +
  scale_fill_viridis(name="cases", 
                     limits = c(min(combined_df$cases),max(combined_df$cases)), 
                     na.value="gray99")+
  transition_states(date)
animate(p, renderer = ffmpeg_renderer())

###################
####################

combined_df_sp <- as_Spatial(combined_df)
combined_df_map <- fortify(combined_df_sp, region = "name")

combined_df_map %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(colour="black") +
  coord_fixed(1.3)  +
  guides(fill=FALSE) 

  
  
  scale_fill_viridis(name="cases", 
                     limits = c(min(combined_df$cases),max(combined_df$cases)), 
                     na.value="gray99")

