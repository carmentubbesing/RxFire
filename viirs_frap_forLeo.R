# see if viirs overlaps these Rx fires

# load libraries
library(tidyverse)
library(sf)
library(fs)

# laod frap
path <- dir_ls(file.path(ref_path, 'FRAP'), glob = '*.geojson', recurse = T)
frap <- st_read(path)
frap_filt <- frap %>% 
  filter(TREATMENT_NAME %in% c('2021 Fuel Reduction', 'Lake Assist', 'LIGGETT ANNUAL BURN',
                               'BEASORE ROAD', 'FUNKY TS', '3N01 HT'),
         YEAR_ == '2021') %>% 
  mutate(START_DATE = as.Date(START_DATE), END_DATE = as.Date(END_DATE))
frap_filt <- st_transform(frap_filt, 4326)
frap_filt <- st_make_valid(frap_filt)

# load viirs
viirs_2021 <- dir_ls(file.path(ref_path, 'viirs/CA_2021_2022'), glob = '*.csv', recurse = T) %>% 
  map(read_csv)
viirs_2021 <- map2_df(viirs_2021, c('J1V', 'SV', 'SV'),
                 ~ select(.x, -version) %>% 
                   mutate(satellite = .y))
viirs_2021 <- filter(viirs_2021, year(acq_date) == 2021) 

viirs_2020 <- dir_ls(file.path(ref_path, 'viirs/CA_2020'), glob = '*.csv', recurse = T) %>% 
  read_csv
viirs <- bind_rows(viirs_2020, viirs_2021)

# make into sf object
viirs <- st_as_sf(viirs, coords = c('longitude', 'latitude'), crs = 4326) %>% 
  mutate(viirs_id = 1:nrow(.), .before = brightness) %>% 
  # convert date and time columns to create datetime object 
  mutate(datetime = paste(acq_date, acq_time) %>% 
         ymd_hm() %>% with_tz(tzone = 'America/Los_Angeles')) %>% 
  filter(confidence != 'l')

# get intersections
intersections <- st_intersects(frap_filt$geometry, viirs$geometry)
f_join_with_frap <- function(i){
  viirs[intersections[[i]],] %>% 
    mutate(fire_id = i, .after = viirs_id) %>% 
    left_join(st_drop_geometry(frap_filt[i,])) %>% 
    mutate(temp_match = acq_date >= START_DATE & acq_date <= END_DATE, .after = fire_id)
}
int_df <- map_df(1:6, f_join_with_frap) 
int_df %>% 
  st_drop_geometry() %>%
  count(TREATMENT_NAME, temp_match) 
write_csv(int_df, 'tmp_outputs/viirs_frap_forLeo.csv')

# plot it
f_plot <- function(i){
  int_df %>% 
    filter(fire_id == i) %>% 
    ggplot() +
    geom_sf(data = frap_filt[i,]) +
    geom_sf(aes(color = temp_match)) +
    labs(title = paste("Fire:", frap_filt$TREATMENT_NAME[i]))
}
pdf('tmp_outputs/viirs_frap_forLeo.pdf')
map(1:6, f_plot)
dev.off()
