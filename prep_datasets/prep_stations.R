library(tidyverse)
library(sf)

read_csv(file.path(ref_path, 'CARB/stations_2023-12-28.txt'), 
         col_names = c('longitude', 'latitude', 'site_name', 'site_id')) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_write(file.path(ref_path, 'CARB', 'stations_2023-12-28.geojson'))




