# load AUS POA data SHP

poa_shp_file <- "Data/POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp"

here::i_am("Code/POA_level code.R")

library(tidyverse)
library(sf)

aus_poa <- st_read(here::here(poa_shp_file))

vic_poa <- aus_poa %>% 
  mutate(POA_CODE21=as.numeric(POA_CODE21)) %>% 
  filter(between(POA_CODE21, 3000, 3999))

ggplot(vic_poa) + 
  geom_sf(col="black", fill="yellow")

library(geojsonio)
topojson_write(vic_poa, geometry="polygon", file="vic_poa.topojson")
