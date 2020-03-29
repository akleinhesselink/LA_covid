library(sf)
s<-read_sf("~/Downloads/EGIS_BG10FIPxx_CSA_20170118/EGIS_BG10FIPxx_CSA_20170118.shp")

list<- s%>%
  st_set_geometry(NULL) %>%
  select(LCITY,BASA) %>%
  group_by(LCITY,BASA) %>%
  summarise()


unique(s$LCITY)
unique(s$BASA)
