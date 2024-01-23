# filter VIIRS data to only include Rx fire data
# excludes wildfire, developed, crop, stationary fires

library(tidyverse)
library(sf)
library(fs)
library(terra)



# functions ---------------------------------------------------------------

# extract data from vector layers
extract_polys <- function(polygons, points) {
  # polygons = vector_layers$solar
  # points = viirs[1:5000,"point_id"]
  
  intersects <- st_intersects(points, polygons, sparse = T)
  if ( length(unlist(intersects)) == 0)
    return(warning("No intersections"))
  else{
    intersections_list <- map(intersects, unlist)
    
    # warning if there's more than 1 polygon intersecting a point
    if(any(map_vec(intersections_list, length) > 1)){
      warning("Point(s) overlapped with more than 1 polygon. By default, choosing first one.")}
    # make sure points with no intersection become NA
    polygonIDs <- map_vec(intersections_list, 
                          ~ ifelse(sum(.x) == 0, NA, .x[1]) )
    newcols <- st_drop_geometry(polygons)[polygonIDs, ]
    return(as_tibble(newcols))
  }
}



# load things -------------------------------------------------------------



# import wfs
frap <- dir_ls(file.path(ref_path, 'frap'), glob = '*.gdb', recurse= T)
st_layers(frap)
wf <- st_read(frap, layer = 'firep22_1')

# layers to help exclude data
vector_layers <- read_rds('outputs_spatial/vector/extract_viirs.rds')
rasts <- rast('outputs_spatial/raster/extract_viirs.tif')

# import VIIRS
viirs_paths <- dir_ls(file.path(ref_path, 'viirs/CA_2021_2022'), glob = '*.csv', recurse = T)[1:3] 
viirs <- map(viirs_paths, read_csv)
viirs <- map2_df(viirs, c('J1V', 'SV', 'SV'),
     ~ select(.x, -version) %>% 
       mutate(satellite = .y)) 
# make into sf object
viirs <- st_as_sf(viirs, coords = c('longitude', 'latitude'), crs = 4326)


# extract attributes at VIIRS points --------------------------------------

# raster data
extracted_rasters <- terra::extract(x = rasts, st_transform(viirs, st_crs(rasts)))
extracted_rasters$ID <- NULL

# vector data
vector_names <- list(c("county"),
                     c("camping"),
                     c("power_source", 'power_capacity'),
                     c("solar"), 
                     c('landfill_nm', 'landfill_id'))
tictoc::tic()
extracted_vectors <- map(vector_layers, ~ extract_polys(.x, st_transform(viirs, st_crs(rasts)))) %>% 
  map2(., vector_names, ~ set_names(.x, .y)) %>% 
  bind_cols
tictoc::toc() # 43 sec



# stationary detects ------------------------------------------------------

# rid VIIRS detects that are likely picking up stationary, artifactual fires
# create a density raster and remove points > 20 points annually (McClure et al. 2023, same method as NEI)

# split viirs by year, calculate density for each point, join back. 
viirs_density <- group_split(viirs, year(acq_date)) %>% 
  map(~ rasterize(st_transform(.x, st_crs(rasts)), rasts$SRA,
                  fun = 'length', background = 0)) %>% 
  map(~ terra::extract(x = .x, st_transform(viirs, st_crs(rasts))))
density_df <- tibble(density_2021 = viirs_density[[1]]$V1_length,
       density_2022 = viirs_density[[2]]$V1_length)


bind_together <- function(l){
  nrows <- map_vec(l, nrow)
  same_nrows <- all(nrows)
  if(same_nrows){
    bind_cols(l)
  }else{
    print("why doesn't all of your data have the same number of rows?")
  }
}
extracted_data <- bind_together(list(extracted_rasters, 
                                     extracted_vectors, 
                                     density_df))


# was VIIRS from wildfire? ------------------------------------------------


# wildfire data
wf <- select(wf, FIRE_NAME, ALARM_DATE, CONT_DATE) %>% 
  filter(year(CONT_DATE) %in% c(2021, 2022)) %>% 
  janitor::clean_names()
wf_buffer <- st_buffer(wf, 5000) # buffer it to deal with plume driven detects

# make this list one intersection per row
int_list <- st_intersects(st_transform(viirs, st_crs(wf_buffer)), wf_buffer)
int_df <- tibble(viirs_row = rep(1:nrow(viirs), times = map_vec(int_list, length)), 
       wf_row = unlist(int_list))

f_wf <- function(viirs_row, wf_row){
  #print(viirs_row)
  # determine if they spatially AND temporally overlap
  SI_wf <- wf_buffer[wf_row,] # spatially intersecting wf
  viirs_date <- as.Date(viirs$acq_date[viirs_row])
  temporal <- (as.Date(SI_wf$alarm_date) <= viirs_date) & (as.Date(SI_wf$cont_date) >= viirs_date)
  if(temporal) tibble(viirs_row = viirs_row, st_drop_geometry(SI_wf))
}

tictoc::tic()
wf_res <- map2_df(int_df$viirs_row, int_df$wf_row, f_wf, .progress = T)
tictoc::toc() #590


# i don't care about the name of the fire. just if there was a spatio-temporal match
# make records for all VIIRS detects too. 
wf_res_complete <- wf_res %>% 
  filter(!duplicated(viirs_row)) %>% 
  complete(viirs_row = 1:nrow(viirs))



# merge everything together with VIIRS ------------------------------------

viirs_everything <- bind_cols(viirs, wf_res_complete, extracted_data)

# categorizing: wildfire, developed (including artifactual), crop, everything else
viirs_everything <- viirs_everything %>% 
  mutate(category = case_when(
    !is.na(fire_name) ~ 'wildfire',
    CDL == "crop" ~ 'crop',
    CDL == "developed" ~ "developed",
    !is.na(power_source)|!is.na(solar)|!is.na(camping) ~ 'developed', 
    density_2021 | density_2022 > 20 ~ 'stationary',
    T  ~ 'other'
  )) 

expath <- file.path(ref_path, 'viirs/CA_2021_2022/viirs_extracted.geojson')
#if(file_exists(expath)) fs::file_delete(expath)
  
st_write(viirs_everything, expath, delete_dsn = T)



# trash -------------------------------------------------------------------

# 
# viirs_everything[449416,] #now 461084
# viirs[449416,]
# 
# wf_res_complete[461084,]
# 
# viirs %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   filter(brightness == 351.86, acq_time == 2148) %>% 
#   print(width = Inf)
# nrow(viirs)
# 
# 
# wf %>% filter(fire_name == 'ROBLAR')
# which(wf$fire_name == 'ROBLAR')
# wf[655,]
# keep <- int_df %>% filter(wf_row == 655)
# viirs[keep$viirs_row,] %>% print(width = Inf)
# 
# 
# mapview::mapview(viirs[keep$viirs_row,] )
# viirs %>% filter(acq_time == 2148, brightness == 351.86)
# which(viirs$acq_time == 2148 & viirs$brightness == 351.86)
# 
# 
# viirs_everything %>% filter(acq_time == 2148, brightness == 351.86) %>% 
#   print(width = Inf)
