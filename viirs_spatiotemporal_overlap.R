

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


# find spatiotemporal overlap with VIIRS ----------------------------------

# match viirs with task force
tf_match <- f_spatiotemporal_match(viirs_nonwf, tf_2021_2022, 
                                   'datetime', 'activity_start', 'activity_end')
# 35 sec
tf_match$viirs_id <- viirs_nonwf$viirs_id[tf_match$x_row]
tf_match$tf_id <- tf_2021_2022$tf_id[tf_match$y_row]
tf_match$x_row <- NULL
tf_match$y_row <- NULL


# match viirs with pfirs
pfirs_match <- f_spatiotemporal_match(viirs_nonwf, pfirs_filt, 
                                      'datetime', 'burn_date')
pfirs_match$viirs_id <- viirs_nonwf$viirs_id[pfirs_match$x_row]
pfirs_match$pfirs_id <- pfirs_filt$pfirs_id[pfirs_match$y_row]
pfirs_match$x_row <- NULL
pfirs_match$y_row <- NULL

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
  priortize_spatiotemporal


# export things -----------------------------------------------------------

# the matches
write_csv(viirs_matches, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs_matches.csv'))
write_csv(tf_match, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_matches.csv'))
write_csv(pfirs_match, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_pfirs_matches.csv'))

# viirs with matches
viirs <- viirs %>% left_join(viirs_matches) %>% 
  mutate(year = year(datetime), 
         category = case_when(
           !is.na(fire_name) ~ 'Wildfire',
           match == 'spatial' ~ 'Spatial overlap',
           match == 'spatiotemporal' ~ 'Spatiotemporal overlap',
           CDL == "crop" ~ 'Crop',
           CDL == "developed" ~ "Developed",
           !is.na(power_source)|!is.na(solar)|!is.na(camping) ~ 'Developed-artifactual', 
           density > 40 ~ 'Developed-artifactual',
           T & month(datetime) >= 5 ~ 'Unaccounted:\nUS EPA assumed Wildfire (May-Dec)',
           T & month(datetime) < 5 ~ 'Unaccounted:\nUS EPA assumed Rx fire (Jan-Apr)'
         )) 

st_write(viirs, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs.geojson'), delete_dsn = T)



