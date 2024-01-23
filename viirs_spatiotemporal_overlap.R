

library(fs)
library(sf)
library(tidyverse)

source('functions.R')

# load data ---------------------------------------------------------------

# CA shapefile
CA <- file.path(ref_path, 'CA boundary/ca-state-boundary/CA_State_TIGER2016.shp')
CA <- st_read(CA)

# task force
path <- file.path(ref_path, 'task force/tf_2021_2022.geojson')
tf_2021_2022 <- st_read(path)
use_crs <- st_crs(tf_2021_2022)

# pfirs
pfirs_filt <- st_read(file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson'))
pfirs_filt <- pfirs_filt %>% 
  st_transform(use_crs) %>% 
  st_buffer(acres_to_radius(.$acres_burned))

# import VIIRS
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_extracted.geojson')
viirs <- st_read(viirs_path)
viirs <- viirs[st_transform(CA, 4326),] #removes non-CA points. 

# make viirs time PST
viirs <- viirs %>% 
  mutate(datetime = paste(acq_date, acq_time) %>% 
           ymd_hm %>% with_tz(tz = 'America/Los_Angeles'),
         acq_date = as.Date(acq_date),
         .before = acq_date
  ) %>% 
  mutate(viirs_id = 1:nrow(.), viirs_row = NULL, .before = brightness) 

# get non-wildfire viirs, clean more
viirs_nonwf <- viirs %>% 
  filter(category != 'wildfire') %>% 
  st_transform(use_crs) %>% 
  st_buffer(375/2)


# there are a few viirs points in wf that weren't removed. figure that out.
viirs %>% 
  filter(viirs_id %in% c(236273, 449416, 236345))


# find spatiotemporal overlap with VIIRS ----------------------------------

# match viirs with task force
tf_match <- f_spatiotemporal_match(viirs_nonwf, tf_2021_2022, 
                                   'activity_start', 'activity_end')

# match viirs with pfirs
pfirs_match <- f_spatiotemporal_match(viirs_nonwf, pfirs_filt, 'burn_date')

# combine matches, join back with rest of viirs
priortize_spatiotemporal <- function(df){
  df %>% 
    group_by(viirs_id) %>%
    arrange(desc(match == "spatiotemporal")) %>%
    slice(1) %>%
    ungroup()
}

tf_match <- select(tf_match, viirs_id, match, tf_id)
pfirs_match <- select(pfirs_match, viirs_id, match, pfirs_id)
viirs_matches <- full_join(priortize_spatiotemporal(tf_match), 
          priortize_spatiotemporal(pfirs_match)) %>% 
  group_by(viirs_id) %>% 
  mutate(across(c(tf_id, pfirs_id), ~ first(.x, na_rm = T))) %>% 
  priortize_spatiotemporal()



# export things -----------------------------------------------------------

# the matches
write_csv(viirs_matches, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs_matches.csv'))
write_csv(tf_match, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_matches.csv'))
write_csv(pfirs_match, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_pfirs_matches.csv'))

# # read these in
# viirs_matches <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs_matches.csv'))
# tf_match <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_matches.csv'))
# pfirs_match <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_pfirs_matches.csv'))


# update categories
viirs_categories <- st_drop_geometry(viirs) %>% 
  select(viirs_id, fire_name, CDL, power_source, landfill_nm, solar, camping, density_2021, density_2022, datetime, category)
category_tf <- viirs_categories %>% 
  left_join(priortize_spatiotemporal(tf_match)) %>% 
  update_category %>%
  rename(category_tf = category)
category_pfirs <- viirs_categories %>% 
  left_join(priortize_spatiotemporal(pfirs_match)) %>% 
  update_category %>%
  rename(category_pfirs = category)
category <- viirs_categories %>% 
  left_join(priortize_spatiotemporal(viirs_matches)) %>% 
  update_category %>%
  rename(category = category)

res <- viirs %>% 
  select(-category) %>% 
  left_join(category_tf) %>%
  left_join(category_pfirs) %>%
  left_join(category) %>% 
  relocate(category_tf, category_pfirs, .after = category) %>% 
  mutate(year = year(datetime))


st_write(res, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs.geojson'), delete_dsn = T)



res$category %>% unique
as_tibble(res) %>% 
  count(category_tf)
as_tibble(res) %>% 
  count(category)

