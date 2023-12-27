# see if viirs overlaps these Rx fires

# load libraries
library(tidyverse)
library(sf)
library(fs)
library(mapview)

theme_set(theme_classic())


# functions ---------------------------------------------------------------

source('functions.R')

# load things -------------------------------------------------------------


# laod frap
path <- dir_ls(file.path(ref_path, 'FRAP'), glob = '*.geojson', recurse = T)
frap <- st_read(path)
frap_filt <- frap %>% 
  filter(TREATMENT_NAME %in% c('2021 Fuel Reduction', 'Lake Assist', 'LIGGETT ANNUAL BURN',
                               'BEASORE ROAD', 'FUNKY TS', '3N01 HT'),
         YEAR_ == '2021') %>% 
  mutate(START_DATE = as.Date(START_DATE), END_DATE = as.Date(END_DATE), fire_id = 1:nrow(.))
# buffer by 375/2 first, them other stuff
frap_filt_buff <- st_buffer(frap_filt, dist = 375/2) %>% st_transform(4326) %>% st_make_valid()
frap_filt <- st_transform(frap_filt, 4326) %>% st_make_valid()

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

intersections <- st_intersects(frap_filt_buff$geometry, viirs$geometry)
f_join_with_frap <- function(i){
  viirs[intersections[[i]],] %>% 
    mutate(fire_id = i, .after = viirs_id) %>% 
    left_join(st_drop_geometry(frap_filt[i,])) %>% 
    mutate(temp_match = acq_date >= START_DATE & acq_date <= END_DATE, .after = fire_id)
}
int_df <- map_df(1:6, f_join_with_frap) 
int_df <- as_tibble(int_df) %>% mutate(TREATMENT_NAME = pretty_names(TREATMENT_NAME))
int_df %>% 
  st_drop_geometry() %>%
  count(TREATMENT_NAME, temp_match) 
write_csv(int_df, 'tmp_outputs/viirs_frap_forLeo.csv')

# add this too
int_df <- int_df %>% 
  mutate(days_since_start = as.numeric(acq_date - START_DATE)) #%>% 
  # mutate(days_since_start = case_when(
  #   days_since_start > 180 ~ 180,
  #   days_since_start < -180 ~ -180,
  #   TRUE ~ days_since_start)) 

# plot it
f_plot <- function(i){
  int_df %>% 
    filter(fire_id == i) %>% 
    st_as_sf(sf_column_name = 'geometry') %>% 
    ggplot() +
    geom_sf(data = frap_filt[i,]) +
    geom_sf(aes(color = temp_match)) +
    labs(title = paste("Fire:", frap_filt$TREATMENT_NAME[i]))
}
pdf('tmp_outputs/viirs_frap_forLeo.pdf')
map(1:6, f_plot)
dev.off()

f_plot(6)

# map time ----------------------------------------------------------------


# make a mapview map
prep_mapview <- function(fire_id){
  frap_d <- frap_filt[fire_id,]
  bb <- bbox_buffered(frap_d) 
  
  filt <- st_intersects(viirs, bb) %>% map_vec(length)
  viirs_filt <- viirs[filt>0,]
  
  # merge with info from int_df
  int_df <- int_df %>% 
    select(viirs_id, fire_id, temp_match) %>% 
    filter(fire_id == fire_id)
  viirs_filt <- viirs_filt %>% 
    left_join(int_df) %>%
    mutate(match = case_when(
      temp_match == T ~ 'spatiotemporal', 
      temp_match == F ~ 'spatial', 
      is.na(temp_match) ~ 'no match'), temp_match = NULL) %>% 
    mutate(days_since_start = as.numeric(acq_date - frap_d$START_DATE)) #%>% 
    # mutate(days_since_start = case_when(
    #   days_since_start > 180 ~ 180,
    #   days_since_start < -180 ~ -180,
    #   TRUE ~ days_since_start)) 
  
  return(list(frap = frap_d, viirs = viirs_filt))
}


f_map <- function(data_list, fire_name){
  ggplot() +
    geom_sf(data = data_list$frap, fill = 'yellowgreen') +
    geom_sf(data = data_list$viirs, size = 4, aes(color = days_since_start, shape = match == 'spatiotemporal')) +
    scale_shape_manual(values = c(4, 19)) +
    harrypotter::scale_color_hp(option = 'ronweasley2', limits = c(-300, 300), na.value = 'black') +
    labs(title = fire_name, color = 'Days since start', shape = 'Spatiotemporal overlap') +
    theme_bw() 
}

mapview_dat <- map(1:6, prep_mapview, .progress = T)
fire_names <- frap_filt$TREATMENT_NAME


f_map(mapview_dat[[6]], fire_names[6])

pdf('tmp_outputs/Leos_trt_mapped.pdf')
map2(mapview_dat, fire_names, f_map)  
dev.off()  

# remake anny's figure ----------------------------------------------------

frap_filt %>% 
  mutate(TREATMENT_NAME = pretty_names(TREATMENT_NAME)) %>% 
  ggplot(aes(y = TREATMENT_NAME)) +
  geom_vline(xintercept = as.Date(c('2020-01-01','2021-01-01', '2022-01-01')), color = 'grey10', lty = 2) +
  geom_linerange(aes(xmin = START_DATE, xmax = END_DATE), lwd = 7, color = 'grey10') +
  geom_point(data = int_df, size = 5, alpha = .7,
             aes(x = acq_date, shape = temp_match, color = days_since_start)) +
  harrypotter::scale_color_hp(option = 'ronweasley2', limits = c(-300, 300), na.value = 'black') +
  labs( color = 'Days since start', shape = 'Spatiotemporal overlap', x = "Date", y = "Treatment") +
  scale_shape_manual(values = c(4, 19)) +
  #scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-01-01")), expand = c(.01,.01),
    breaks = seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "1 month"),
    labels = function(x) ifelse(month(x) == 1, scales::date_format("%b %Y")(x), scales::date_format("%b")(x))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave('tmp_outputs/Leos_trt_time.png', width = 10, height = 5, units = 'in')
