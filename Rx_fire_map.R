# make map for anny

library(tidyverse)
library(sf)
library(fs)
library(mapview)

source('functions.R')

# prep datasets -----------------------------------------------------------

# task force--previously dealt with in 'taskforce_duplicates.R'
tf <- st_read('outputs_spatial/vector/taskforce_with_ids_2021_2022.geojson') %>% 
  filter(st_geometry_type(.) != 'POINT')

# use this crs for the rest
use_crs <- st_crs(tf)

# NEI 
nei_rx <- read_csv(file.path(ref_path, 'US EPA/2022FireLoc_California.csv')) %>% 
  select(date, id, event_name, latitude, longitude, county, area, sources, scc_description) %>% 
  mutate(Rx = grepl('prescribed', scc_description), 
         FS = ifelse(grepl('flaming', scc_description), 'F', 'S') 
  ) %>% 
  filter(Rx == T) %>% 
  distinct(date, id, event_name, latitude, longitude, .keep_all = T) %>% 
  mutate(geom_id = as.integer(as.factor(paste(latitude, longitude, sep = ','))),
         .after = id) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = F)
# convert date string to date, change format
nei_rx$date <- as.Date(trimws(nei_rx$date),  format = "%Y%m%d") %>% 
  format('%m/%d/%y')
nei_rx <- st_transform(nei_rx, use_crs) %>% 
  st_buffer(acres_to_radius(nei_rx$area))

# PFIRS
pfirs <- readxl::read_xlsx(file.path(ref_path, 'PFIRS/PFIRS 2017-2022 pulled 2023.xlsx')) %>% 
  janitor::clean_names() %>% 
  filter(year(burn_date) %in% 2021:2022) %>%
  distinct() %>% 
  mutate(pfirs_id = 1:nrow(.), .before = burn_date) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_transform(use_crs) %>% 
  st_buffer(acres_to_radius(.$acres_burned))


# CARB things: air districts boundaries and monitoring sites
districts <- dir_ls(file.path(ref_path, 'CARB'), glob = '*AirDistrict.shp', recurse = T) %>% 
  st_read %>% st_transform(use_crs) %>% select(NAME)
stations <- dir_ls(file.path(ref_path, 'CARB'), glob = '*stations.shp', recurse = T) %>% 
  st_read()%>% st_transform(use_crs) %>% select(AIRSSITEID, AIRDISTRIC)




# group records by geometry -----------------------------------------------

#NEI
nei_collapsed <- nei_rx %>% 
  as_tibble %>% 
  group_by(geom_id) %>% 
  mutate(lat_lon = paste(latitude, longitude, sep = ', ')) %>% 
  summarize(dates = paste(date, collapse = ', '),
            event_name = paste(unique(event_name), collapse = ', '),
            acres_values = paste(signif(area, 2), collapse = ', '), 
            sum_acres = signif(sum(area), 2), 
            sources = paste(unique(sources), collapse = ', '),
            coordinates = paste(unique(lat_lon), sep = ', ')
              ) 
# merge back with geometry, make sf
nei_collapsed <- bind_cols(nei_collapsed, 
                             select(nei_rx, geometry)[match(nei_collapsed$geom_id, nei_rx$geom_id),]  
) %>% 
  st_as_sf(sf_column_name = 'geometry') %>% arrange(-st_area(.))



# PFIRS: look for treatments where the geometries are the same and dates
l <- pfirs$geometry %>% st_equals()  
pfirs <- pfirs %>% mutate(geom_id = map_vec(l, ~ paste(.x, collapse = ',')) %>% 
                   match(., unique(.)), 
                 .after = pfirs_id)
# export pfirs bc anny will prob want
# write_csv(as_tibble(pfirs), 'outputs_spatial/vector/pfirs_withids_2017_2023.csv')
pfirs_collapsed <- pfirs %>% 
  as_tibble %>% 
  mutate(date = glue::glue("{format(as.Date(burn_date), '%m/%d/%y')}")) %>% 
  group_by(geom_id) %>%
  summarise(n_records = n(),
            agency = paste(unique(agency), collapse = ', '), 
            burn_unit = paste(unique(burn_unit), collapse = ', '),
            dates = paste(date, collapse = ', '),
            acres_values = paste(signif(acres_burned, 2), collapse = ', '),
            sum_acres = signif(sum(acres_burned), 2))
# merge back with geometry, make sf
pfirs_collapsed <- bind_cols(pfirs_collapsed, 
          select(pfirs, geometry)[match(pfirs_collapsed$geom_id, pfirs$geom_id),]  
          ) %>% 
  st_as_sf(sf_column_name = 'geometry') %>% arrange(-st_area(.))


# TASK FORCE: group by geom_id, collapse dates, record_acres, and tf_ids
tf_collapsed <- tf %>% 
  st_drop_geometry() %>% 
  mutate(dates = case_when(
    activity_start == activity_end ~ glue::glue("{format(as.Date(activity_end), '%m/%d/%y')}"), 
    activity_start != activity_end ~ glue::glue("{format(as.Date(activity_start), '%m/%d')}-{format(as.Date(activity_end), '%m/%d/%y')}")
  )) %>% 
  group_by(geom_id) %>% 
  summarise(tf_ids = paste(tf_id, collapse = ', '),
            organization = unique(org_admin_t), 
            project_name = paste(unique(project_name), collapse = ','),
            dates = paste(dates, collapse = ', '),
            acres_values = paste(signif(record_acres, 2), collapse = ', '),
            sum_acres = signif(sum(record_acres), 2))
# merge back with geometry, make sf
tf_collapsed <- bind_cols(tf_collapsed, 
                             select(tf, geometry)[match(tf_collapsed$geom_id, tf$geom_id),]  
) %>% st_as_sf(sf_column_name = 'geometry') %>% arrange(-st_area(.))
# separate by pfirs
tf_pfirs <- tf_collapsed %>% filter(organization == 'CARB')
tf_nopfirs <- tf_collapsed %>% filter(organization != 'CARB')



# import viirs  -----------------------------------------------------------

# import viirs that's already been categorized as spatially/temporally overlapping



# map things --------------------------------------------------------------
library(leaflet)

# station icon
station_icon <- makeIcon(
  iconUrl = "images/sensor.png",
  iconWidth = 20, iconHeight = 20
)

district <- filter(districts, NAME == 'Mendocino')

layers <- list(tf_pfirs, tf_nopfirs, nei_collapsed, pfirs_collapsed, stations)
layer_names <- c('tf_pfirs', 'tf_nopfirs', 'nei_rx', 'pfirs', 'stations')
names(layers) <- layer_names
layers_filt <- map(layers, ~ .x[district,]) %>% 
  map(~ st_transform(.x, 4326)) 



# map it!
leaflet() %>% 
  # base maps
  addProviderTiles(providers$CartoDB.Positron, group = 'street') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'satellite') %>%
  addProviderTiles(providers$OpenTopoMap, group = 'topo') %>%
  # add markers
  addMarkers(data = layers_filt$stations, group = 'stations', icon = station_icon,
             popup = ~ paste("<b>siteID: </b> ", AIRSSITEID, "<br>",
                             "<b>district: </b> ", AIRDISTRIC, "<br>"),
             clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T)) %>%
  # add polygons
  addPolygons(data = layers_filt$nei_rx, group = 'NEI', color = 'orange', fillOpacity = .2,
              popup = ~ paste("<b>dataset:</b> ", "NEI", "<br>",
                              "<b>id:</b> ", geom_id, "<br>",
                              '<b>event name:</b> ', event_name, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sources:</b> ', sources, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers_filt$pfirs, group = 'PFIRS', color = 'blue',
              popup = ~ paste("<b>dataset:</b> ", "PFIRS", "<br>",
                              "<b>geom_ids:</b> ", geom_id, "<br>",
                              "<b>n_records:</b> ", n_records, "<br>",
                              "<b>agency:</b> ", agency, "<br>",
                              '<b>burn unit:</b> ', burn_unit, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sum acres:</b> ', sum_acres, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers_filt$tf_nopfirs, group = 'Task Force (non-PFIRS)', color = 'purple',
              popup = ~ paste("<b>dataset:</b> ", "Task Force (non-PFIRS)", "<br>",
                              "<b>tf_ids:</b> ", tf_ids, "<br>",
                              "<b>org:</b> ", organization, "<br>",
                              '<b>project name:</b> ', project_name, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sum acres:</b> ', sum_acres, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers_filt$tf_pfirs, group = 'Task Force (PFIRS)', color = 'hotpink',
            popup = ~ paste("<b>dataset:</b> ", "Task Force (PFIRS)", "<br>",
                            "<b>tf_ids:</b> ", tf_ids, "<br>",
                            "<b>org:</b> ", organization, "<br>",
                            '<b>project name:</b> ', project_name, "<br>",
                            '<b>dates:</b> ', dates, "<br>",
                            '<b>acres:</b> ', acres_values, "<br>",
                            '<b>sum acres:</b> ', sum_acres, "<br>"),
            highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                bringToFront = F)) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("street", "satellite", "topo"),
    overlayGroups = c('PFIRS', "Task Force (non-PFIRS)", "Task Force (PFIRS)", 'NEI'),
    options = layersControlOptions(collapsed = F)
  )



