
# install.packages("arcpullr")

# library(arcpullr)
# 
# url <- 'https://gsal.sig-gis.com/server/rest/services/Hosted/ITS_Dashboard_Feature_Layer/FeatureServer'
# layer_info <- arcpullr::get_layer_info(url)
# layers <- layer_info$layers
# 
# urls <- paste(url, layers$id, sep = '/')
# layers <- map(urls, get_spatial_layer)
# 
# 

library(fs)
library(sf)
library(tidyverse)

# load task force data ----------------------------------------------------


# import task force data
path <- dir_ls(file.path(ref_path, 'task force/Treatment_Tracking_Fire 20230926.gdb'), 
               glob = '*.gdb', recurse = T)
lyrs <- st_layers(path)

tf_points <- st_read(path, lyrs$name[1]) %>% 
  mutate(tf_id = 1:nrow(.), .before = everything()) %>% 
  mutate(across(.cols = c(ACTIVITY_END, ACTIVITY_START, PROJECT_START, PROJECT_END,
                          TREATMENT_START, TREATMENT_END), ~ as.Date(.x)))
tf_polys <- st_read(path, lyrs$name[2]) %>% 
  mutate(tf_id = (1:nrow(.)) + max(tf_points$tf_id), 
         .before = everything()) %>% 
  mutate(across(.cols = c(ACTIVITY_END, ACTIVITY_START, PROJECT_START, PROJECT_END,
                          TREATMENT_START, TREATMENT_END), ~ as.Date(.x)))
# st_drop_geometry(tf_points) %>% write_csv(file.path(ref_path, 'task force/full_tf_points.csv'))
# st_drop_geometry(tf_polys) %>% write_csv(file.path(ref_path, 'task force/full_tf_polygons.csv'))


# clean it up and only keep Rx fire activity ------------------------------


# lines: can ignore the lines dataset
# unique(tf_list[[1]]$activity_description) 

# * points =============================== 
# points: filter activity_cat == 'BENEFICIAL_FIRE'

# columns to keep
tf_points_clean <- tf_points %>% 
  select(tf_id, PROJECT_NAME, ACTIVITY_DESCRIPTION, ACTIVITY_CAT, BROAD_VEGETATION_TYPE,
         ACTIVITY_STATUS, ACTIVITY_QUANTITY, ACTIVITY_UOM, ACTIVITY_START, ACTIVITY_END, 
         PRIMARY_OWNERSHIP_GROUP, ORG_ADMIN_t, Source) %>% 
  janitor::clean_names() 

# contains only fire, no treatments with EA (used by CRNA to measure # of piles)
tf_points_clean <- tf_points_clean %>% 
  filter(activity_cat == 'BENEFICIAL_FIRE', activity_uom == 'AC')

# assign new geometries--either circle polygon or point
radii <- acres_to_radius(tf_points_clean$activity_quantity) %>% 
  replace_na(0)
newshape <- st_buffer(tf_points_clean$Shape, radii)
newshape[st_is_empty(newshape)] <- tf_points_clean$Shape[st_is_empty(newshape)] 
st_geometry(tf_points_clean) <- newshape


# * polygons ========================================
# polygons: filter activity_cat == 'BENEFICIAL_FIRE'
tf_polys_clean <- tf_polys %>% 
  select(tf_id, PROJECT_NAME, ACTIVITY_DESCRIPTION, ACTIVITY_CAT, BROAD_VEGETATION_TYPE,
         ACTIVITY_STATUS, ACTIVITY_QUANTITY, ACTIVITY_UOM, ACTIVITY_START, ACTIVITY_END, 
         PRIMARY_OWNERSHIP_GROUP, ORG_ADMIN_t, Source) %>% 
  janitor::clean_names() %>% 
  filter(activity_cat == 'BENEFICIAL_FIRE')


# * join together ====================================
tf_joined <- bind_rows(tf_points_clean, tf_polys_clean) %>% 
  rename(record_acres = activity_quantity) %>% 
  mutate(gis_acres = units::set_units(st_area(.), acres) %>% signif(2) %>% as.numeric, 
         activity_uom = NULL,
         .after = record_acres)

# compare to VIIRS, just use 2021 and 2022. 
# if no start day, assume it was the day before the end date. 
tf_2021_2022 <- tf_joined %>% 
  # fix data where the start and stop dates got mixed up
  mutate(duration = as_date(activity_end) - as_date(activity_start) %>% 
           as.numeric() %>% replace_na(0),
         activity_start = if_else(duration < 0, activity_end, activity_start),
         activity_end = if_else(duration < 0, activity_start, activity_end)) %>% 
  # assume if no start time, it's the same day as end date
  mutate(activity_start = coalesce(activity_start, activity_end), 
         duration = NULL) %>% 
  filter(year(activity_end) %in% c(2021, 2022))

# export and reimport to fix mutlisurface geometries
path <- file.path(ref_path, 'task force/tf_2021_2022.geojson')
#st_write(tf_2021_2022, path, delete_dsn = T)


tf_2021_2022 <- st_read(path)

