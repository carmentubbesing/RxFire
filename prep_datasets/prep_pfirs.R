library(fs)
library(tidyverse)
library(sf)

pfirs <- readxl::read_xlsx(file.path(ref_path, 'PFIRS/PFIRS 2017-2022 pulled 2023.xlsx')) %>% 
  janitor::clean_names()
pfirs_filt <- pfirs %>% 
  filter(year(burn_date) %in% c(2021, 2022), 
         !is.na(burn_date), !is.na(acres_burned)) %>% 
  select(-total_tons) %>% 
  mutate(burn_date = as.Date(burn_date)) %>% 
  # remove exact duplicates
  distinct(burn_date, burn_unit, acres_burned, .keep_all = T) %>% 
  # make it spatial
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  # project to a local crs where units are in meters (will buffer by different radii)
  st_transform(st_crs(CA)) %>% 
  # crop to CA
  st_intersection(st_geometry(CA)) %>% 
  # add ID to each row
  mutate(pfirs_id = 1:nrow(.), .before = burn_date)
st_write(obj = pfirs_filt, file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson'), 
         delete_dsn = T)