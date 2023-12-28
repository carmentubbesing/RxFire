# make map for jeremy to see pfirs compliance

library(tidyverse)
library(sf)
library(fs)
library(mapview)

# PFIRS
pfirs <- st_read(file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson'))
pfirs <- pfirs %>% 
  st_transform(4326)

districts <- dir_ls(file.path(ref_path, 'CARB'), glob = '*AirDistrict.shp', recurse = T) %>% 
  st_read %>% st_transform(use_crs) %>% select(NAME) %>% st_transform(4326)


# viirs
viirs <- st_read(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs.geojson'))
viirs <- viirs %>% 
  filter(!category %in% c('Crop', 'Developed', 'Developed-artifactual', 'Wildfire')) %>% 
  mutate(category = factor(category, levels = c(
    "Spatial overlap", 'Spatiotemporal overlap',
    'Unaccounted:\nUS EPA assumed Rx fire (Jan-Apr)',
    'Unaccounted:\nUS EPA assumed Wildfire (May-Dec)'))
  )

# do some summarizing by air district -------------------------------------

# viirs vs pfirs--to see compliance? for jeremy.
viirs_districts <- st_intersection(
  select(viirs, category, tf_id, pfirs_id, year, datetime), 
  districts)
pfirs_districts <- st_intersection(st_transform(pfirs, 4326), districts)


districts_pfirs_viirs <- full_join(
  st_drop_geometry(viirs_districts) %>% 
    group_by(NAME) %>% 
    summarise(n_viirs = n(), viirs_acres = n()*34.7),
  st_drop_geometry(pfirs_districts) %>%
    group_by(NAME) %>% 
    summarise(n_pfirs = n(), pfirs_acres = sum(acres_burned))
) %>% 
  mutate(pfirsN_viirs_ratio = signif(n_pfirs / n_viirs, 2),
         pfirsAcres_viirs_ratio = signif(pfirs_acres / viirs_acres, 2)) 

left_join(districts, districts_pfirs_viirs) %>% 
  mapview(zcol = 'pfirsN_viirs_ratio')
left_join(districts, districts_pfirs_viirs) %>% 
  mapview(zcol = 'pfirsAcres_viirs_ratio')
