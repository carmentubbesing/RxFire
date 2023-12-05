

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

# functions ---------------------------------------------------------------


# convert acres to buffer radius
acres_to_radius <- function(x){
  require(units)
  area <- set_units(x, acre)
  area_m2 <- set_units(area, m^2)
  
  # get r
  r <- as.numeric(sqrt(area_m2/pi))
  return(r)
}

f_spatiotemporal_match <- function(x, y, x_date, y_start, y_end){
  
  # make sure crs of x and y are the same
  test <- st_crs(x) == st_crs(y)
  if(!test) stop("x and y need to have the same crs")
  
  # make this list one intersection per row
  int_list <- st_intersects(x, y)
  int_df <- tibble(x_row = rep(1:nrow(x), times = map_vec(int_list, length)), 
                   y_row = unlist(int_list))
  
  f_overlap <- function(x_row, y_row){
    # determine if they spatially AND temporally overlap
    spatial <- y[y_row,] # spatially intersecting
    x_date <- as_date(pull(x, x_date)[x_row])
    y_start <- as_date(pull(spatial, y_start))
    y_end <- as_date(pull(spatial, y_end))
    temporal <- (y_start <= x_date) & (y_end >= x_date)
    if(is.na(temporal)) stop("y contains dates that are NA. fix that.")
    if(temporal){
      tibble(x_row = x_row, y_row = y_row, match = 'spatiotemporal', st_drop_geometry(spatial))
    }else{
      tibble(x_row = x_row, y_row = y_row, match = 'spatial', st_drop_geometry(spatial))
    }
  }
  
  tictoc::tic()
  res <- map2_df(int_df$x_row, int_df$y_row, f_overlap, .progress = T)
  tictoc::toc() #590
  
  return(res)
}


# load task force data ----------------------------------------------------




# import task force data
path <- dir_ls(file.path(ref_path, 'task force/Treatment_Tracking_Fire 20230926.gdb'), 
               glob = '*.gdb', recurse = T)
lyrs <- st_layers(path)

tf_points <- st_read(path, lyrs$name[1])
tf_polys <- st_read(path, lyrs$name[2])


# clean it up and only keep Rx fire activity ------------------------------


# lines: can ignore the lines dataset
# unique(tf_list[[1]]$activity_description) 

# * points =============================== 
# points: filter activity_cat == 'BENEFICIAL_FIRE'

# columns to keep
tf_points_clean <- tf_points %>% 
  select(ACTIVITY_DESCRIPTION, ACTIVITY_CAT, BROAD_VEGETATION_TYPE,
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
  select(ACTIVITY_DESCRIPTION, ACTIVITY_CAT, BROAD_VEGETATION_TYPE,
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



# find spatiotemporal overlap with VIIRS ----------------------------------


# import VIIRS
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_extracted.geojson')
viirs <- st_read(viirs_path)

# make viirs time PST
viirs <- viirs %>% 
  mutate(datetime = paste(acq_date, acq_time) %>% 
           ymd_hm %>% with_tz(tz = 'America/Los_Angeles'),
         .before = acq_date
  )

# add in an ID column
viirs$ID <- 1:nrow(viirs)

# check for a spatio-temporal match with TF data. buffer the viirs points, transform.
tictoc::tic()
viirs_nonwf <- viirs %>% 
  filter(category != 'wildfire') %>% 
  st_transform(st_crs(tf_2021_2022)) %>% 
  st_buffer(375/2) 
tictoc::toc() # 9 sec
  
tf_match <- f_spatiotemporal_match(viirs_nonwf, tf_2021_2022, 
                                   'datetime', 'activity_start', 'activity_end')
# 35 sec
tf_match$x_row <- viirs_nonwf$ID[tf_match$x_row]

# make a complete data frame again
# join back with viirs_nonwf
viirs <- left_join(viirs, 
                  # get one row per viirs point
                  tf_match %>% 
                    distinct(x_row, match, record_acres) %>% 
                    group_by(x_row) %>% 
                    arrange(desc(match == "spatiotemporal")) %>%
                    slice(1) %>%
                    ungroup() %>% 
                    rename(ID = x_row))

# from VIIRS perspective... -----------------------------------------------



# repeat what I did in the script to prep viirs, but now with data on TF polygons
viirs <- viirs %>% 
  mutate(acres_per_detect = if_else(record_acres > 34.74906 | is.na(record_acres), 34.74906, record_acres)) %>% 
  mutate(year = year(datetime), 
         category = case_when(
           !is.na(fire_name) ~ 'Wildfire',
           match == 'spatial' ~ 'TF: spatial overlap',
           match == 'spatiotemporal' ~ 'TF: spatiotemporal overlap',
           CDL == "crop" ~ 'Crop',
           CDL == "developed" ~ "Developed",
           !is.na(power_source)|!is.na(solar)|!is.na(camping) ~ 'Developed', 
           density > 40 ~ 'Developed',
           T & month(datetime) >= 5 ~ 'Unaccounted:\nWildfire (May-Dec)',
           T & month(datetime) < 5 ~ 'Unaccounted:\nRx fire (Jan-Apr)'
         )) 
st_write(viirs, file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce.geojson'))


viirs_breakdown <- viirs %>% as_tibble() %>%  
  group_by(category) %>% 
  summarise(n = n(),
            acres = n*34.74906,
            acres_cor = sum(acres_per_detect, na.rm = T)) %>% 
  mutate(n_percent = n/sum(n)*100, .after = n,
         area_percent = acres_cor/sum(acres_cor)*100)

#viirs_breakdown

# from TF perspective... --------------------------------------------------

tf_breakdown <- tf_match %>% 
  st_drop_geometry() %>% 
  arrange(desc(match)) %>% 
  filter(!duplicated(y_row)) %>% 
  full_join(tf_2021_2022 %>% mutate(y_row = 1:nrow(.))) %>% 
  mutate(match = replace_na(match, 'none'), 
         year = year(activity_end)) %>% 
  filter(activity_status == 'COMPLETE') %>% 
  group_by(match) %>% 
  summarise(n_activities = n(), 
            recorded_acres = sum(record_acres, na.rm = T),
            gis_acres = sum(gis_acres, na.rm = T)) %>% 
  #group_by(year) %>% 
  mutate(across(n_activities:gis_acres, ~ signif(.x/sum(.x)*100, 2), .names = "p_{.col}")) 
tf_breakdown




# treemap -----------------------------------------------------------------
library(treemapify)
library(glue)

pal <- wesanderson::wes_palette('Darjeeling1', 5)
plot(1:length(pal), col = pal, pch = 16, cex = 10)
viirs_breakdown$pal <- pal[c(2, 3, 5, 5, NA, NA, 1)]
viirs_breakdown$pal[4] <- colorspace::darken('#5BBCD6', .5)


# from VIIRS perspective
viirs_breakdown %>% 
  ggplot(aes(area = acres_cor , fill= category, 
             label = glue("{category}"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 12) +
  scale_fill_manual(values = viirs_breakdown$pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_VIIRS_all.png', width = 6, height = 3.5)

# from VIIRS perspective, removing wildfire, crop, dev
viirs_breakdown2 <- viirs_breakdown %>% 
  filter(!category %in% c("Crop", 'Developed', 'Wildfire')) %>% 
  mutate(area_percent = signif(acres_cor/sum(acres_cor)*100, 2))
ggplot(viirs_breakdown2, aes(area = acres_cor , fill= category, 
             label = glue("{category}"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 12) +
  scale_fill_manual(values = viirs_breakdown2$pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_VIIRS_subset.png', width = 6, height = 3.5)



# from polygon perspective
tf_pal <- c(NA, '#5BBCD6', '#075D6D')
tf_breakdown %>% 
  ggplot(aes(area = p_n_activities , fill= match, 
             label = glue("{str_to_title(match)}\n{round(p_n_activities)}%"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 8) +
  scale_fill_manual(values = tf_pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_TF_nactivities.png', width = 6, height = 3.5)


tf_breakdown %>% 
  ggplot(aes(area = recorded_acres , fill= match, 
             label = glue("{str_to_title(match)}\n{round(p_recorded_acres)}%"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 8) +
  scale_fill_manual(values = tf_pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_TF_recordedacres.png', width = 6, height = 3.5)


# unifying them together? 
viirs_tf_unified <- tibble(category = c('both_VIIRS_TF', 'TF_only', 'VIIRS_only'), 
       acres_375m2 = c(with(viirs_breakdown2, acres_cor[grep('spatiotemporal', category)]),
                       filter(tf_breakdown, match %in% c('none', 'spatial')) %>% pull(recorded_acres) %>% sum, 
                       filter(viirs_breakdown2, !category %in% grep('temp', category, value = T)) %>% 
                         pull(acres_cor) %>% sum
                       ),
       acres_polygons = c(tf_breakdown$recorded_acres[3], sum(tf_breakdown$recorded_acres[1:2]), acres_375m2[3])
       ) 
acres_viirs_375 <- sum(viirs_tf_unified$acres_375m2[c(1,3)])
acres_viirs_poly <- sum(viirs_tf_unified$acres_polygons[c(1,3)])
acres_TF_375 <- sum(viirs_tf_unified$acres_375m2[c(1,2)])
acres_TF_poly <- sum(viirs_tf_unified$acres_polygons[c(1,2)])
viirs_tf_unified <- viirs_tf_unified %>% 
  mutate(p_viirs_375 = acres_375m2/acres_viirs_375,
         p_viirs_poly = acres_polygons/acres_viirs_poly,
         p_TF_375 = acres_375m2/acres_TF_375,
         p_TF_poly = acres_polygons/acres_TF_poly) 
viirs_tf_unified$p_TF_375[3] <- NA
viirs_tf_unified$p_TF_poly[3] <- NA
viirs_tf_unified$p_viirs_375[2] <- NA
viirs_tf_unified$p_viirs_poly[2] <- NA



# attempts to make a venn diagram -----------------------------------------

library(VennDiagram)

# Generate 3 sets of 200 words
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = 'venn_diagramm.png',
  output=TRUE
)

TF_only <- 246
VIIRS_only <- 892
both <- 29


TF <- c(paste0(rep('TF', TF_only), 1:TF_only), 
        paste0(rep('both', both), 1:both) )
VIIRS <- c(paste0(rep('VIIRS', VIIRS_only), 1:VIIRS_only), 
           paste0(rep('both', both), 1:both))
venn.diagram(
  x = list(TF, VIIRS),
  category.names = c('TF', 'VIIRS'),
  filename = '#14_venn_diagramm.png',
  output=T, 
  
  # circles customize
  lwd = 2, 
  fill = c('#FF0000', '#5BBCD6'),
  col = c('#FF0000', '#5BBCD6'), 
  
  # change size of names
  cex = 0, 
  cat.cex = 0
)

