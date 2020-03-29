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

county_pops <- read_csv('data/county_populations.csv')

load( 'data/BASA_shapes.rda')

# FIX KAGEL/LOPEZ Canyon 
BASA <- 
  BASA %>% mutate( region = ifelse( region == 'UNINCORPORATE', 'UNINCORPORATED', region))
# 

# Remove LONG BEACH UNINCORPORATED 
BASA <- 
  BASA %>% filter( ! (region == 'UNINCORPORATED' & community == 'LONG BEACH'))
#

BASA <- 
  BASA %>% 
  left_join(cases %>% spread( date, cases ))

################leaflet################
library(leaflet)
library(RColorBrewer)
library(viridis)

BASA <- 
  BASA %>% 
  st_transform(4326)

pal <- colorNumeric(palette = "YlOrRd", domain = BASA$`2020-03-28`)

labels <- 
  sprintf("<strong>%s:</strong><br/>%g cases",
          BASA$community, BASA$`2020-03-28`) %>% 
  lapply(htmltools::HTML)

BASA %>%
  leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(stroke = FALSE, fillOpacity = 0.8, 
              fillColor = ~pal(`2020-03-28`),
              highlight = highlightOptions( color = "Cyan",fillOpacity = 0.8,bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 
  addLegend(position = "bottomright", pal = pal, values = BASA$`2020-03-28`, title = "Number of cases", opacity = 0.8) 
  




  
