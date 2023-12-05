# compare VIIRS to Chen's outputs of allfires

# packages
library(tidyverse)
library(sf)
library(fs)
library(mapview)


# functions
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


# ca coordinates? 
# xmin       ymin       xmax       ymax 
# -124.48202   32.52884 -114.13122   42.00951 

# import VIIRS Suomi
viirs_dir <- file.path(ref_path, 'VIIRS/CA_2020/DL_FIRE_SV-C2_389155/fire_archive_SV-C2_389155.csv')
viirs_csv <- read_csv(viirs_dir)
viirs <- st_as_sf(viirs_csv, coords = c("longitude", "latitude"), crs = 4326)


# import Chen
chen_path <- "C:/Users/lrosenth/OneDrive - California Air Resources Board/Desktop/Chen_paper"
chen <- file.path(chen_path, "tmp/allfires2020.geojson") %>% st_read
chen$ted_year %>% table # just 2020
chen$tst_year %>% table # just 2020
chen <- st_make_valid(chen)

# import other things, like counties
vector_layers <- read_rds('outputs_spatial/vector/extract_viirs.rds')
fires <- read_rds('outputs_spatial/vector/fire_records_2020.rds')
counties <- vector_layers$counties
counties <- st_transform(counties, st_crs(chen))
# mapview(counties, zcol = "NAME")

# filter to just sierra nevada counties
sierraNevada_co <- c(
  "Alpine", "El Dorado", "Nevada", "Placer", "Sierra", "Yuba", "Amador",
  "Calaveras", "Tuolumne", "Mariposa", "Madera", "Mono", "Fresno", "Tulare", 
  "Inyo", "Plumas", "Butte") 
tahoe_co <- c("Nevada", "Placer", "Yuba", "Plumas", "Butte")
counties_filt <- filter(counties, NAME %in% tahoe_co)
chen_filt <- chen[counties_filt,]

# wildfire
wf_filt <- st_transform(fires$frap_wf, st_crs(counties_filt))[counties_filt,] %>% 
  filter(year(ALARM_DATE) == 2020)

# filter viirs by intersections with chen's polygons and counties
fireIDs <- extract_polys(select(chen_filt, fireID), viirs)
viirs_filt_chen <- viirs %>% 
  mutate(fireID = fireIDs$value, 
         yday = yday(date(acq_date))) %>% 
  filter(!is.na(fireID)) %>% 
  group_by(fireID) %>% 
  mutate(day = yday - min(yday) + 1) %>% 
  ungroup
fireNames <- extract_polys(select(wf_filt, FIRE_NAME), viirs)
viirs_filt_frap <- viirs %>% 
  mutate(fireNames = fireNames$value, 
         yday = yday(date(acq_date))) %>% 
  filter(!is.na(fireNames)) %>% 
  group_by(fireNames) %>% 
  mutate(day = yday - min(yday) + 1) %>% 
  ungroup

viirs_filt_county <- viirs[counties_filt,]




# visualize
# mapviewOptions(basemaps.color.shuffle = F) # want positron by default
# mapviewOptions()
mapview(wf_filt, legend = F, col.region = "red4") +
mapview(viirs_filt_chen, cex = 2.5, zcol = "day", legend = F) +
  mapview(viirs_filt_frap, cex = 2.5, zcol = "day", legend = F) +
 mapview(chen_filt, col.region = NA, alpha.region = 0, lwd = 2, color = "blue") 

# which viirs points are not identified in Chen's fire perimeters?
int_list <- st_intersects(viirs, chen)
int <- map_vec(int_list, length)
viirs <- mutate(viirs, int_chen = int>0)
# 95% of viirs picked up by Chen
as_tibble(viirs) %>% count(int_chen) %>% mutate(perc = n/sum(n))
as_tibble(viirs) %>% 
  count(int_chen, type) %>% 
  group_by(int_chen) %>% 
  mutate(perc = 100*n/sum(n))

# how many of the fire perimeters have no viirs points? 
int_list <- st_intersects(chen, viirs)
int <- map_vec(int_list, length)
chen <- mutate(chen, int_viirs = int>0)
# 100% of Chen's polygons have VIIRS points in them 
as_tibble(chen) %>% count(int_viirs) %>% mutate(perc = n/sum(n))
