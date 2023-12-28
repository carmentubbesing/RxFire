# make map for anny

library(tidyverse)
library(sf)
library(fs)
library(mapview)
library(terra)

source('functions.R')

# prep datasets -----------------------------------------------------------

# task force--previously dealt with in 'taskforce_duplicates.R'
tf <- st_read('outputs_spatial/vector/taskforce_with_ids_2021_2022.geojson') %>% 
  filter(st_geometry_type(.) != 'POINT')

# use this crs for the rest
use_crs <- st_crs(tf)

# NEI 
nei <- read_csv(file.path(ref_path, 'US EPA/2022FireLoc_California.csv')) %>% 
  select(date, id, event_name, latitude, longitude, county, area, sources, scc_description) %>% 
  mutate(Rx = grepl('prescribed', scc_description), 
         FS = ifelse(grepl('flaming', scc_description), 'F', 'S') 
  ) %>% 
  # filter(Rx == T) %>% 
  distinct(date, id, event_name, latitude, longitude, .keep_all = T) %>% 
  mutate(geom_id = as.integer(as.factor(paste(latitude, longitude, sep = ','))),
         .after = id) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = F)
# convert date string to date, change format
nei$date <- as.Date(trimws(nei$date),  format = "%Y%m%d") %>% 
  format('%m/%d/%y')
nei <- st_transform(nei, use_crs) %>% 
  st_buffer(acres_to_radius(nei$area)) 

# PFIRS
pfirs <- st_read(file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson'))
pfirs <- pfirs %>% 
  st_transform(use_crs) %>% 
  st_buffer(acres_to_radius(.$acres_burned))

# CARB things: air districts boundaries and monitoring sites
districts <- dir_ls(file.path(ref_path, 'CARB'), glob = '*AirDistrict.shp', recurse = T) %>% 
  st_read %>% st_transform(use_crs) %>% select(NAME)
stations <- st_read(file.path(ref_path, 'CARB', 'stations_2023-12-28.geojson'))


# viirs
viirs <- st_read(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs.geojson'))
viirs <- viirs %>% 
  filter(!category %in% c('Crop', 'Developed', 'Wildfire')) %>% 
  mutate(category = factor(category, levels = c(
    "Spatial overlap", 'Spatiotemporal overlap', 'Developed-artifactual',
    'Unaccounted:\nUS EPA assumed Rx fire (Jan-Apr)',
    'Unaccounted:\nUS EPA assumed Wildfire (May-Dec)'))
    )


# add a raster of land use/land cover?
rasts <- terra::rast('outputs_spatial/raster/extract_viirs.tif')
#rasts_crop <- crop(rasts, districts[districts$NAME == 'Glenn',] %>% st_transform(st_crs(rasts)))


# group records by geometry -----------------------------------------------

#NEI
nei_collapsed <- nei %>% 
  as_tibble %>% 
  group_by(geom_id) %>% 
  mutate(lat_lon = paste(latitude, longitude, sep = ', ')) %>% 
  summarize(dates = paste(date, collapse = ', '),
            event_name = paste(unique(event_name), collapse = ', '),
            acres_values = paste(signif(area, 2), collapse = ', '), 
            sum_acres = signif(sum(area), 2), 
            sources = paste(unique(sources), collapse = ', '),
            rx = any(Rx),
            coordinates = paste(unique(lat_lon), sep = ', ')
              ) 
# merge back with geometry, make sf
nei_collapsed <- bind_cols(nei_collapsed, 
                             select(nei, geometry)[match(nei_collapsed$geom_id, nei$geom_id),]  
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
  summarise(pfirs_ids = paste(unique(pfirs_id), collapse = ', '), 
            n_records = n(),
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



# map things --------------------------------------------------------------
library(leaflet)

# station icon
station_icon <- makeIcon(
  iconUrl = "images/sensor.png",
  iconWidth = 20, iconHeight = 20
)

# put layers into a list, transform 
layers <- list(tf_pfirs, tf_nopfirs, nei_collapsed, pfirs_collapsed, stations)
layer_names <- c('tf_pfirs', 'tf_nopfirs', 'nei', 'pfirs', 'stations')
names(layers) <- layer_names
layers <- map(layers, ~ st_transform(.x, 4326), .progress = T)
layers <- map(layers, st_make_valid, .progress = T)
districts <- st_transform(districts, 4326)
#layers$nei <- st_transform(nei_collapsed, 4326)

# filter by district for testing
district <- filter(districts, NAME == 'Mendocino')
layers_filt <- map(layers, ~ .x[district,])
viirs_filt <- viirs[district,]
viirs_filt$category %>% unique


# color palettes
harrypotter::hp_palettes
plot_pal(harrypotter::harrypotter(4, option = 'ronweasley2'))
viirs_pal <- colorFactor(c( '#5BBCD6', '#FF0000', '#2ECC71', 'grey30', 'grey30'), viirs$category)
layers_pal <- colorFactor(harrypotter::harrypotter(4, option = 'ronweasley2'), layer_names[1:4])
layer_labels <- c("NEI (2022 only)", "PFIRS", "Task Force (non-PFIRS)", "Task Force (PFIRS)")
raster_labels <- levels(rasts$CDL)[[1]][,2]
rast_pal <- colorFactor(c('green4', '#A569BD', '#F7DC6F', '#D6EAF8'), values(rasts$CDL))


# map it!
map_polygons <- leaflet() %>% 
  # base maps
  addProviderTiles(providers$CartoDB.Positron, group = 'street') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'satellite') %>%
  addProviderTiles(providers$OpenTopoMap, group = 'topo') %>%
  # add markers
  addMarkers(data = layers$stations, group = 'stations', icon = station_icon,
             popup = ~ paste("<b>siteID: </b> ", site_id, "<br>",
                             "<b>site name: </b> ", site_name, "<br>"),
             clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T)) %>%
  # add polygons
  addPolygons(data = layers$nei, group = 'NEI', 
              color = layers_pal('nei'), fillOpacity = .2,
              popup = ~ paste("<b>dataset:</b> ", "NEI", "<br>",
                              "<b>id:</b> ", geom_id, "<br>",
                              "<b>Rx fire:</b> ", rx, "<br>",
                              '<b>event name:</b> ', event_name, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sources:</b> ', sources, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers$pfirs, group = 'PFIRS', 
              color = layers_pal('pfirs'),
              popup = ~ paste("<b>dataset:</b> ", "PFIRS", "<br>",
                              "<b>pfirs_ids:</b> ", pfirs_ids, "<br>",
                              "<b>n_records:</b> ", n_records, "<br>",
                              "<b>agency:</b> ", agency, "<br>",
                              '<b>burn unit:</b> ', burn_unit, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sum acres:</b> ', sum_acres, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers$tf_nopfirs, group = 'Task Force (non-PFIRS)', 
              color = layers_pal('tf_nopfirs'),
              popup = ~ paste("<b>dataset:</b> ", "Task Force (non-PFIRS)", "<br>",
                              "<b>tf_ids:</b> ", tf_ids, "<br>",
                              "<b>org:</b> ", organization, "<br>",
                              '<b>project name:</b> ', project_name, "<br>",
                              '<b>dates:</b> ', dates, "<br>",
                              '<b>acres:</b> ', acres_values, "<br>",
                              '<b>sum acres:</b> ', sum_acres, "<br>"),
              highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                  bringToFront = F)) %>% 
  addPolygons(data = layers$tf_pfirs, group = 'Task Force (PFIRS)',
            color = layers_pal('tf_pfirs'),
            popup = ~ paste("<b>dataset:</b> ", "Task Force (PFIRS)", "<br>",
                            "<b>tf_ids:</b> ", tf_ids, "<br>",
                            "<b>org:</b> ", organization, "<br>",
                            '<b>project name:</b> ', project_name, "<br>",
                            '<b>dates:</b> ', dates, "<br>",
                            '<b>acres:</b> ', acres_values, "<br>",
                            '<b>sum acres:</b> ', sum_acres, "<br>"),
            highlightOptions = highlightOptions(color = "turquoise", weight = 4, 
                                                bringToFront = F)) %>% 
  # add legend
  addLegend(pal = layers_pal, values=  layer_names[1:4], 
            labFormat = function(type, cuts) paste0(layer_labels),
            opacity = 1,
            title = 'Rx fire layers', position = 'bottomleft') 

# add district boundaries
map_polygons <- map_polygons %>% 
  addPolygons(data = districts, fill = NA, label = ~ paste('Air District: ', NAME),
              highlightOptions = highlightOptions(color = "white", weight = 4, 
                                                  bringToFront = F)) 
  
# add land cover raster 
map_raster <- map_polygons %>% 
  addRasterImage(rasts$CDL, opacity = .5, colors = rast_pal, group = 'Land Type') %>% 
  addLegend(pal = rast_pal, values =values(rasts$CDL), opacity = .9,
            labFormat = function(type, cuts) paste0(raster_labels),
            title = 'land type', position = 'bottomleft') %>% 
  hideGroup('Land Type')

# add viirs to it
map_all <- map_raster %>% 
  addCircleMarkers(data = viirs, group = 'VIIRS', 
             color = ~ viirs_pal(category), stroke = F, 
             radius = 5, fillOpacity = .7, 
             popup = ~ paste("<b>dataset:</b> ", "VIIRS", "<br>",
                             "<b>viirs_id:</b> ", viirs_id, "<br>",
                             "<b>tf_id:</b> ", tf_id, "<br>",
                             "<b>pfirs_id:</b> ", pfirs_id, "<br>",
                             "<b>date:</b> ", acq_date, "<br>",
                             "<b>category:</b> ", category, "<br>"),
             #highlightOptions = highlightOptions(color = "turquoise", weight = 4, bringToFront = F)
             ) %>% 
  addLegend(pal = viirs_pal, values = viirs$category, opacity = .9,
            title = 'VIIRS', position = 'bottomleft') %>%
  addLayersControl(
    baseGroups = c("street", "satellite", "topo"), 
    overlayGroups = c('Land Type', 'stations', 'PFIRS', "Task Force (non-PFIRS)", "Task Force (PFIRS)", 'NEI', 'VIIRS'),
    options = layersControlOptions(collapsed = T)
  ) %>% 
  # add scale bar
  addScaleBar(position = 'bottomright', options = scaleBarOptions(
    maxWidth = 200, metric = T, imperial = T, updateWhenIdle = T
  ))
map_all



# export things -----------------------------------------------------------
# save map
expath_map <- file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/mapleaflet_all_data_2021_2022.html')
mapshot(map_all, expath_map)

# save datasets
nei %>% st_drop_geometry() %>% write_csv(file.path(ref_path, 'US EPA/nei_2022_cleaned.csv'))
# pfirs already saved
# tf already saved

