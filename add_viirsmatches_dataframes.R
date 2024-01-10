# get viirs and tf with viirs match column 
library(tidyverse)
library(sf)
library(fs)

# load data
tf <- st_read('outputs_spatial/vector/taskforce_with_ids_2021_2022.geojson') %>% 
  filter(st_geometry_type(.) != 'POINT')

pfirs <- st_read(file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson'))


# download the matches
tf_match <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_matches.csv'))
pfirs_match <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_pfirs_matches.csv'))

# priortize spatiotemporal
pfirs_match <- pfirs_match %>% 
  group_by(pfirs_id) %>% 
  arrange(desc(match == 'spatiotemporal')) %>% 
  slice(1) %>%
  ungroup
tf_match <- tf_match %>% 
  group_by(tf_id) %>% 
  arrange(desc(match == 'spatiotemporal')) %>% 
  slice(1) %>%
  ungroup


# join the matches for pfirs and tf
pfirs_wmatch <- st_drop_geometry(pfirs) %>% 
  left_join(pfirs_match) %>% 
  rename(viirs_match = match)
tf_wmatch <- st_drop_geometry(tf) %>% 
  left_join(tf_match) %>% 
  rename(viirs_match = match)

# export
expath <- file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/datasets/pfirs_viirsmatches.csv')
write_csv(pfirs_wmatch, expath)
expath <- file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/datasets/tf_viirsmatches.csv')
write_csv(tf_wmatch, expath)


# get summaries
count(pfirs_wmatch, viirs_match) %>% 
  mutate(perc = round(n/sum(n)*100, 2))
count(tf_wmatch, viirs_match) %>% 
  mutate(perc = round(n/sum(n)*100, 2))
