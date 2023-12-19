# viirs vs. pfirs. 
# see how pfirs detection changes as a function of buffer size

library(tidyverse)
library(sf)
library(mapview)


# functions ---------------------------------------------------------------

# helper function to calculate percentages
get_perc <- function(x) round(x/sum(x)*100, 2)


# convert acres to buffer radius
acres_to_radius <- function(x){
  require(units)
  area <- set_units(x, acre)
  area_m2 <- set_units(area, m^2)
  
  # get r
  r <- as.numeric(sqrt(area_m2/pi))
  return(r)
}

f_spatiotemporal_match <- function(x, y, x_date, y_start, y_end = NULL){
  
  # make sure crs of x and y are the same
  test <- st_crs(x) == st_crs(y)
  if(!test) stop("x and y need to have the same crs")
  
  # make this list one intersection per row
  int_list <- st_intersects(x, y)
  int_df <- tibble(x_row = rep(1:nrow(x), times = map_vec(int_list, length)), 
                   y_row = unlist(int_list))
  
  f_overlap <- function(x_row, y_row){
    # determine if they spatially AND temporally overlap
    
    # spatially intersecting
    spatial <- y[y_row,] 
    
    # temporal intersecting
    x_date <- as_date(pull(x, x_date)[x_row])
    y_start <- as_date(pull(spatial, y_start))
    if(!is.null(y_end)){
      y_end <- as_date(pull(spatial, y_end))
      temporal <- (y_start <= x_date) & (y_end >= x_date)
    }else{
      temporal <- y_start == x_date
    }
    
    # assign matching categories
    if(is.na(temporal)) stop("y contains dates that are NA. fix that.")
    if(temporal){
      tibble(x_row = x_row, y_row = y_row, match = 'spatiotemporal', st_drop_geometry(spatial))
    }else{
      tibble(x_row = x_row, y_row = y_row, match = 'spatial', st_drop_geometry(spatial))
    }
  }
  
  tictoc::tic()
  res <- map2_df(int_df$x_row, int_df$y_row, f_overlap, .progress = T)
  tictoc::toc() 
  
  return(res)
}

# load data ---------------------------------------------------------------

# CA shapefile
CA <- file.path(ref_path, 'CA boundary/ca-state-boundary/CA_State_TIGER2016.shp')
CA <- st_read(CA)

# PFIRS
pfirs <- readxl::read_xlsx(file.path(ref_path, 'PFIRS/PFIRS 2017-2022 pulled 2023.xlsx')) %>% 
  janitor::clean_names()
pfirs_filt <- pfirs %>% 
  filter(year(burn_date) %in% c(2021, 2022), 
         !is.na(burn_date), !is.na(acres_burned)) %>% 
  select(-total_tons) %>% 
  mutate(burn_date = as.Date(burn_date)) %>% 
  # remove exact duplicates
  distinct(burn_date, burn_unit, acres_burned, .keep_all = T) %>% 
  # make it spatial
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  # project to a local crs where units are in meters (will buffer by different radii)
  st_transform(st_crs(CA)) %>% 
  # crop to CA
  st_intersection(st_geometry(CA)) %>% 
  # add ID to each row
  mutate(pfirs_id = 1:nrow(.))

# import VIIRS
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_extracted.geojson')
viirs <- st_read(viirs_path)

# make viirs time PST
viirs <- viirs %>% 
  mutate(datetime = paste(acq_date, acq_time) %>% 
           ymd_hm %>% with_tz(tz = 'America/Los_Angeles'),
         acq_date = as.Date(acq_date),
         .before = acq_date
  )
# get non-wildfire viirs, clean more
viirs_nonwf <- viirs %>% 
  filter(category != 'wildfire') %>% 
  mutate(viirs_id = 1:nrow(.), viirs_row = NULL) %>%
  st_transform(st_crs(pfirs_filt))



# spatial and temporal overlap --------------------------------------------


# write a function so you can do this for different buffer sizes
calculate_overlap <- function(pfirs_filt, viirs_nonwf, buffer_amount, return_dfs = F){
  
  # buffer pfirs
  viirs_res <- 375/2
  pfirs_buffered <- pfirs_filt %>% 
    st_buffer(buffer_amount + viirs_res)
  
  
  # check for a spatio-temporal match with pfirs.
  match_df <- f_spatiotemporal_match(viirs_nonwf, pfirs_buffered, 
                                     'datetime', 'burn_date') %>% 
    mutate(viirs_id = viirs_nonwf$viirs_id[x_row],
           pfirs_id = pfirs_filt$pfirs_id[y_row], 
           .before = match,
           x_row = NULL, y_row = NULL) 
  
  # join back with viirs_nonwf
  viirs_match <- left_join(viirs_nonwf, 
                           # get one row per viirs point
                           match_df %>% 
                             distinct(viirs_id, pfirs_id, match, acres_burned ) %>% 
                             group_by(viirs_id) %>% 
                             arrange(desc(match == "spatiotemporal")) %>%
                             slice(1) %>%
                             ungroup())
  
  # join back with PFIRS
  pfirs_match <- left_join(mutate(pfirs_buffered, buffer_radius = buffer_amount), 
                           # get one row per pfirs point
                           match_df %>% 
                             distinct(viirs_id, pfirs_id, match, acres_burned ) %>% 
                             group_by(pfirs_id) %>% 
                             arrange(desc(match == "spatiotemporal")) %>%
                             slice(1) %>%
                             ungroup()
  )
  
  # calculate overlap summaries
  
  # from VIIRS perspective
  viirs_summary <- viirs_match %>% 
    st_drop_geometry() %>% 
    # update the categories
    mutate(category = case_when(
      match == 'spatiotemporal' ~ 'spatiotemporal',
      match == 'spatial' ~ 'spatial',
      category == 'other' ~ 'no_match',
      TRUE ~ 'developed_crop'
    )) %>% 
    # filter out development and crop
    filter(category %in% c('no_match', 'spatial', 'spatiotemporal')) %>% 
    count(category) %>% 
    mutate(acresV = n*34.7)
  
  # from PFIRS perspective
  pfirs_summary <- pfirs_match %>% 
    st_drop_geometry() %>% 
    mutate(match = replace_na(match, 'no_match')) %>% 
    group_by(match) %>% 
    summarise(n = n(),
              acresP = sum(acres_burned)
    ) %>% 
    rename(category = match)
  
  # merge summaries together. 
  # acresV = estimates acreage by multiplying # viirs points by 34.7 acres
  # acresP = acres burned according to records in PFIRS
  merged_summary <- bind_rows(
    viirs_summary %>% mutate(source = 'VIIRS', .before = category),
    pfirs_summary %>% mutate(source = 'PFIRS', .before = category)
  ) %>% 
    # refactor category
    mutate(category = factor(category, levels = c('spatiotemporal', 'spatial', 'no_match'))) %>% 
    arrange(source, category)
  # make acresV = acresP when category == spatiotemporal
  merged_summary$acresV[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'PFIRS'] <- 
    merged_summary$acresV[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'VIIRS']
  merged_summary$acresP[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'VIIRS'] <- 
    merged_summary$acresP[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'PFIRS']
  # get percentages
  merged_summary <- merged_summary %>% 
    mutate(acresV = coalesce(acresV, acresP), 
           acresP = coalesce(acresP, acresV) 
    ) %>% 
    group_by(source) %>% 
    mutate(across(c(n, acresV, acresP), ~ get_perc(.x), .names = '{.col}_perc')) %>% 
    ungroup() 
  
  if(return_dfs){
    return(list(merged_summary, viirs_match = viirs_match, pfirs_match = pfirs_match))
  }else{
    return(merged_summary)
  }
}

# run this assuming buffer ~ recorded acres
buffer_Amount <- acres_to_radius(pfirs_filt$acres_burned)
res_record_acres <- calculate_overlap(pfirs_filt, viirs_nonwf, buffer_Amount, T)
mapview(res_record_acres$pfirs_match %>% 
          filter(!is.na(match)) %>% 
          select(pfirs_id, burn_date, agency, match, acres_burned, burn_type), 
        zcol = 'match') +
mapview(res_record_acres$viirs_match %>% 
          filter(!is.na(match)) %>% 
          select(viirs_id, acq_date, match, acres_burned),
        zcol = 'match')
# get table to make a venn diagram
df_VD <- res_record_acres[[1]] %>% 
  mutate(category = ifelse(category == 'spatiotemporal', 'spatiotemporal', 'none')) %>% 
  group_by(source, category) %>% 
  summarise(acresV = sum(acresV), acresP = sum(acresP)) %>% 
  mutate(across(c(acresV, acresP), ~ get_perc(.x), .names = '{.col}_perc'), 
         mean_acres = round((acresV  + acresP )/2/1000))
df_VD



# repeat, but now looking at a range of buffer radii
buffer_range <- c(0, 100, 187.5, 500, 1000, 2000, 4000)
res_buffer_range <- map(buffer_range, 
                        ~ calculate_overlap(pfirs_filt, viirs_nonwf, .x, F)) 
res_buffer_range_df <- map2_df(res_buffer_range, buffer_range, 
                               ~ mutate(.x, buffer = .y, .after = category))



# visualize ---------------------------------------------------------------

theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
d_recorded_acres <- filter(res_record_acres[[1]], category == 'spatiotemporal') %>% 
  mutate(max_acres_perc = pmax(acresV_perc, acresP_perc),
         mean_acres_perc = (acresV_perc + acresP_perc)/2) 

p_acres <- res_buffer_range_df %>% 
  filter(category == 'spatiotemporal') %>% 
  mutate(max_acres_perc = pmax(acresV_perc, acresP_perc),
         mean_acres_perc = (acresV_perc + acresP_perc)/2) %>% # print(width = Inf)
  ggplot(aes(buffer, mean_acres_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Estimated acreage (%)', title = 'Spatiotemporal overlap') + 
  geom_hline(data = d_recorded_acres, aes(yintercept = mean_acres_perc), lty = 2) +
  labs(caption = '**Dotted line represents overlap when PFIRS polygon area = recorded acres burned**')
p_n <- res_buffer_range_df %>% 
  filter(category == 'spatiotemporal') %>% 
  ggplot(aes(buffer, n_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Activities/detections (%)', title = 'Spatiotemporal overlap') + 
  geom_hline(data = d_recorded_acres, aes(yintercept = n_perc), lty = 2) +
  labs(caption = '**Dotted line represents overlap when PFIRS polygon area = recorded acres burned**')
ggsave(plot = p_acres, 'tmp_outputs/pfirs_vs_viirs_buffers_acres.png', width = 10, height = 5)
ggsave(plot = p_n, 'tmp_outputs/pfirs_vs_viirs_buffers_n.png', width = 10, height = 5)

# make a venn diagram
PFIRS_only <- df_VD$mean_acres[df_VD$source == 'PFIRS' & df_VD$category == 'none']
VIIRS_only <- df_VD$mean_acres[df_VD$source == 'VIIRS' & df_VD$category == 'none']
both <- df_VD$mean_acres[df_VD$source == 'PFIRS' & df_VD$category == 'spatiotemporal']
p_VD <- c(paste0(rep('PFIRS', PFIRS_only), 1:PFIRS_only), 
          paste0(rep('both', both), 1:both) )
v_VD <- c(paste0(rep('VIIRS', VIIRS_only), 1:VIIRS_only), 
          paste0(rep('both', both), 1:both))
VennDiagram::venn.diagram(
  x = list(p_VD, v_VD),
  category.names = c('PFIRS', 'VIIRS'),
  filename = 'tmp_outputs/venn_diagramm_pfirs_vs_viirs.png',
  output=T, 
  
  # circles customize
  lwd = 2, 
  fill = c('#FF0000', '#5BBCD6'),
  col = c('#FF0000', '#5BBCD6'), 
  
  # change size of names
  cex = 0, 
  cat.cex = 0
)

