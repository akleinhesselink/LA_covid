rm(list = ls() )
library(tidyverse)
library(leaflet)
library(sf)

cases <- 
  read_csv('data/temp/cases-update-2020-03-28.csv') 

cases$community %>% unique()

cases %>% 
  filter( community != 'LOS ANGELES COUNTY (EXCL. LB AND PAS)') %>% 
  group_by( date ) %>% 
  summarise( sum(cases, na.rm = T))

cases %>% 
  filter( community == 'LOS ANGELES COUNTY (EXCL. LB AND PAS)') %>% 
  group_by( date ) %>% 
  summarise( sum(cases, na.rm = T))

cases$cases[ cases$cases == 0 ] <- NA

load( 'data/BASA_shapes.rda')

# FIX KAGEL/LOPEZ Canyon 
BASA <- 
  BASA %>% mutate( region = ifelse( region == 'UNINCORPORATE', 'UNINCORPORATED', region))
# 

# Remove LONG BEACH UNINCORPORATED 
BASA <- 
  BASA %>% filter( ! (region == 'UNINCORPORATED' & community == 'LONG BEACH'))
#

cases <- 
  cases %>% 
  left_join(BASA %>% st_drop_geometry %>% distinct(region, community, POPULATION)) %>% 
  group_by( region, community ) %>% 
  arrange( region, community, date) %>% 
  mutate( case_growth = log(cases) - log(lag(cases))) %>% 
  mutate( cases_per_10k = (cases/POPULATION)*10000 )

BASA_map <- 
  BASA %>% 
  left_join(cases) %>% 
  filter( date == max(date)) 
  
################leaflet################
library(leaflet)
library(RColorBrewer)
library(viridis)

BASA_map <- 
  BASA_map %>% 
  st_transform(4326)

pal <- colorNumeric(palette = "YlOrRd", domain = BASA_map$cases_per_10k)

labels <- 
  sprintf("<strong>%s:</strong><br/>%g cases",
          BASA_map$community, BASA_map$cases_per_10k) %>% 
  lapply(htmltools::HTML)

BASA_map %>%
  leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(stroke = FALSE, fillOpacity = 0.8, 
              fillColor = ~pal(cases_per_10k),
              highlight = highlightOptions( color = "Cyan",fillOpacity = 0.8,bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 
  addLegend(position = "bottomright", pal = pal, values = BASA_map$cases_per_10k, title = "Number of cases", opacity = 0.8) 
  



  
