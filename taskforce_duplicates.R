# search for duplicates in the task force data


library(fs)
library(sf)
library(tidyverse)
library(mapview)

bbox_buffered <- function(spatial_df, buffer = 1000){
  u <- st_crs(spatial_df, parameters = T)$units_gdal
  if(u != 'metre') stop("your spatial data frame needs to be in meters")
  (st_bbox(spatial_df) + c(-buffer, -buffer, buffer, buffer)) %>% 
    st_make_grid(n = 1)
}

theme_set(theme_bw())

# load stuff --------------------------------------------------------------


# import the task force data you were working with to make the ppt
path <- file.path(ref_path, 'task force/tf_2021_2022.geojson')
tf_2021_2022 <- st_read(path)

# also viirs points
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce.geojson')
viirs <- st_read(viirs_path)



# find duplicates in task force data --------------------------------------


# look for treatments where the geometries simply intersect
l <- tf_2021_2022$geometry %>% st_intersects()  
intersects_id <- map_vec(l, ~ paste(.x, collapse = ',')) %>% 
    match(., unique(.))
  
# look for treatments where the geometries are the same and dates
l <- tf_2021_2022$geometry %>% st_equals()  
geom_id <- map_vec(l, ~ paste(.x, collapse = ',')) %>% 
  match(., unique(.))

# find rows that have same geometries. 2544 out of 5082 same geometries. 
tf_with_ids <- mutate(tf_2021_2022, intersects_id = intersects_id, geom_id = geom_id) %>% 
  as_tibble() %>% 
  group_by(activity_start, activity_end, record_acres, geom_id) %>% 
  mutate(field_id = cur_group_id()) %>% 
  arrange(geom_id, field_id) %>% 
  ungroup

 
# basic facts
tf_with_ids %>% 
  group_by(geom_id) %>% 
  summarise(same_geom = n(), 
            same_fields = sum(table(field_id)[table(field_id)>1])
            )  %>% 
  summarise(n_activities = sum(same_geom), 
            n_same_geom = sum(same_geom[same_geom > 1]), 
            n_same_geom_fields = sum(same_fields)) %>% 
  mutate(across(contains('same'), ~ .x/n_activities*100, .names = "perc_{.col}")) %>% 
  print(width = Inf)



# visualize things --------------------------------------------------------


# map out ones where there are multiple copies of the geometry, including exact copies
# overlay viirs points on top?
print(tf_with_ids, width = Inf)
tf_with_ids %>% 
  group_by(geom_id) %>% 
  filter(any(duplicated(field_id))) %>% #, length(unique(source)) > 1) %>% 
  filter(source == 'PFIRS') %>% 
  select(-geometry) %>% arrange(intersects_id ) %>% view# print(width = Inf, n = 50) %>% view

# export duplicates
tf_with_ids %>% 
  group_by(geom_id) %>% 
  filter(any(duplicated(field_id))) %>% 
  st_drop_geometry() %>% select(-geometry, -intersects_id) %>% ungroup %>%
  write_csv('tmp_outputs/taskforce_duplicates.csv')

# check out a specific clump of intersecting polygons
tf <- tf_with_ids %>% 
  st_as_sf(sf_column_name = 'geometry') %>% 
  filter(geom_id == 2913) %>% 
  select(tf_id, activity_description, broad_vegetation_type, record_acres, gis_acres, 
         activity_start, activity_end, org_admin_t)
bb <- bbox_buffered(tf, 0)
tf_filt <- st_as_sf(tf_with_ids, sf_column_name = 'geometry') %>% st_intersection(bb) %>% 
  select(tf_id, geom_id, activity_description, broad_vegetation_type, record_acres, gis_acres, 
         activity_start, activity_end, org_admin_t)
dates_of_geoms <- tf_filt %>% 
  st_drop_geometry() %>% 
  mutate(dates = case_when(
    activity_start == activity_end ~ glue::glue("{format(as.Date(activity_end), '%m/%d/%y')}"), 
    activity_start != activity_end ~ glue::glue("{format(as.Date(activity_start), '%m/%d')}-{format(as.Date(activity_end), '%m/%d/%y')}")
  )) %>% 
  group_by(geom_id) %>% 
  summarise(dates = paste(dates, collapse = ', '),
            record_acres = paste(record_acres, collapse = ', ')) 
print(dates_of_geoms, n = Inf)
tf_filt <- tf_filt %>% 
  distinct(geom_id, activity_description, gis_acres, org_admin_t, geometry) %>% 
  left_join(dates_of_geoms)

viirs_filt <- viirs %>% 
  st_intersection(bbox_buffered(tf, 0) %>% st_transform(st_crs(viirs))) %>% 
  select(datetime, category, tf_id, record_acres) %>% 
  filter(category != 'Wildfire') %>% 
  mutate(category = case_when(
    grepl('Rx', category) ~ 'Unaccounted: US EPA assumed Rx fire',
    grepl('Wildfire', category) ~ 'Unaccounted: US EPA assumed wildfire',
    T ~ category
  ))


#mapview(tf, zcol = 'source', alpha.regions = .1, col.regions= sample(wesanderson::wes_palette('Zissou1')), legend = F ) +
  mapview(tf_filt, zcol = 'org_admin_t', alpha.regions = .5, col.regions= sample(c("#E2D200", "#DD8D29")), legend = F ) +
  mapview(viirs_filt, zcol = 'category', layer.name = 'VIIRS', 
          col.regions = c( '#5BBCD6', '#FF0000', 'grey30', 'grey30'))
print(st_drop_geometry(tf), width = Inf, n = Inf) 
print(st_drop_geometry(tf_filt), width = Inf, n = Inf)
