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
  mutate(across(contains('same'), ~ .x/n_activities*100, .names = "perc_{.col}"))



# visualize things --------------------------------------------------------


# map out ones where there are multiple copies of the geometry, including exact copies
# overlay viirs points on top?
print(tf_with_ids, width = Inf)
tf_with_ids %>% 
  group_by(intersects_id) %>% 
  filter(any(duplicated(field_id))) %>% #, length(unique(source)) > 1) %>% 
  filter(source != 'PFIRS') %>% 
  select(-geometry) %>% arrange(intersects_id ) %>%  print(width = Inf, n = 50) %>% view

# check out a specific clump of intersecting polygons
tf <- tf_with_ids %>% 
  st_as_sf(sf_column_name = 'geometry') %>% 
  filter(intersects_id == 1113) 
bb <- bbox_buffered(tf, 2000)
tf_filt <- st_as_sf(tf_with_ids, sf_column_name = 'geometry') %>% st_intersection(bb)
viirs_filt <- viirs %>% 
  st_intersection(bbox_buffered(tf, 2000) %>% st_transform(st_crs(viirs))) %>% 
  select(datetime, category, record_acres) %>% 
  filter(category != 'Wildfire')

#mapview(tf, zcol = 'source', alpha.regions = .1, col.regions= sample(wesanderson::wes_palette('Zissou1')), legend = F ) +
  mapview(tf_filt, zcol = 'source', alpha.regions = .5, col.regions= sample(wesanderson::wes_palette('Zissou1')), legend = F ) +
  mapview(viirs_filt, zcol = 'category', layer.name = 'VIIRS')
print(st_drop_geometry(tf), width = Inf, n = Inf) 
print(st_drop_geometry(tf_filt), width = Inf, n = Inf)


st_as_sf(tf_with_ids, sf_column_name = 'geometry') %>% 
  filter(intersects_id == 1113) %>% 
  print( width = Inf, n = Inf)

st_drop_geometry(tf_filt) %>% 
  group_by(geom_id) %>% 
  summarise(
    n = n(), 
    n_same_fields = sum(duplicated(field_id) | duplicated(field_id, fromLast = T)),
    date_min = as_date(min(activity_start)), date_max = as_date(max(activity_end))
  ) 

# look at the records over time 
st_drop_geometry(tf) %>% 
  group_by(field_id) %>% mutate(n_copies = n() > 1) %>% ungroup %>% 
  ggplot(aes(activity_start, as.factor(geom_id))) +
  geom_point(aes(size = record_acres, color = n_copies), alpha = .5) +
  labs(x = 'Date', y = 'Geometry ID', color = '>1 exact copy', size = 'Acres') +
  scale_color_manual(values = c('lightblue3', 'red1'))

# find activities where polygons overlap and diff sources. export and view in arcpro
  tf_with_ids %>% 
    group_by(intersects_id) %>% 
    filter(n()>1) %>% 
    mutate(n_sources = length(unique(source))) %>% 
    filter(n_sources > 1) %>% 
    st_as_sf(sf_column_name = 'geometry') %>% 
    st_write('outputs_spatial/vector/taskforce_duplicates.shp', delete_layer = T)


 tf_with_ids %>% 
   filter(st_geometry_type(geometry) != 'POINT') %>% 
   st_as_sf(sf_column_name = 'geometry') %>% 
   st_write('outputs_spatial/vector/taskforce_duplicates.shp', delete_layer = T)
 