rm(list = ls() )
library(tidyverse)
library(leaflet)
library(sf)

cases <- 
  read_csv('data/cases-by-neighborhood-2020-03-27.csv') %>%
  mutate(cases = if_else(is.na(cases), 0, cases))

neighborhood_spatial <- 
  read_sf('data/la-county-neighborhoods-current/') %>%
  select(name) %>%
  st_transform("+proj=longlat +datum=WGS84")


combined_df <- 
  neighborhood_spatial %>%
  left_join(cases, by=c('name'='neighborhood'))

combined_df %>%
  filter(date=="2020-03-27") %>%
  ggplot() + 
  geom_sf(aes(fill=cases),color='grey') +
  scale_fill_viridis_c("cases",option = "plasma",na.value=NA)+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  coord_sf(datum=NA) 



################leaflet################
library(leaflet)
library(RColorBrewer)
library(viridis)

filter_df<- combined_df %>% filter(date=="2020-03-27")

pal <- colorNumeric(palette = "YlOrRd", domain =filter_df$cases)

labels <- sprintf(
  "<strong>%s:</strong><br/>%g cases",
  filter_df$name, filter_df$cases
) %>% lapply(htmltools::HTML)

filter_df%>%
  leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(stroke = FALSE,
              fillColor = ~pal(cases),
              highlight = highlightOptions( color = "Cyan",fillOpacity = 0.8,bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 
  addLegend(position = "bottomright", pal = pal, values = filter_df$cases, title = "Number of cases", opacity = 0.8) 
  




  
