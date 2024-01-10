# viirs vs. pfirs. 
# see how pfirs detection changes as a function of buffer size

library(tidyverse)
library(sf)
library(mapview)
library(future)
library(furrr)

source('functions.R')

theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

# load data ---------------------------------------------------------------

# CA shapefile
CA <- file.path(ref_path, 'CA boundary/ca-state-boundary/CA_State_TIGER2016.shp')
CA <- st_read(CA)

# task force
path <- file.path(ref_path, 'task force/tf_2021_2022.geojson')
tf_2021_2022 <- st_read(path)
use_crs <- st_crs(tf_2021_2022)

# PFIRS
pfirs <- st_read(file.path(ref_path, 'PFIRS/PFIRS_cleaned_2021_2022.geojson')) %>% 
  st_transform(use_crs)

# import VIIRS
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_extracted.geojson')
viirs <- st_read(viirs_path)
viirs <- viirs[st_transform(CA, 4326),]

# make viirs time PST
viirs <- viirs %>% 
  mutate(datetime = paste(acq_date, acq_time) %>% 
           ymd_hm %>% with_tz(tz = 'America/Los_Angeles'),
         acq_date = as.Date(acq_date),
         .before = acq_date
  ) %>% 
  mutate(viirs_id = 1:nrow(.), viirs_row = NULL, .before = brightness) 

# get non-wildfire viirs, clean more
viirs_nonwf <- viirs %>% 
  filter(category != 'wildfire') %>% 
  st_transform(use_crs) %>% 
  st_buffer(375/2)


# spatial and temporal overlap: Task Force --------------------------------

# just do this with no buffer. 
res_tf_nobuffer <- calculate_overlap_buffer(tf_2021_2022, viirs_nonwf, 0,
                         acres_field = 'record_acres', 
                         agency_start = 'activity_start', 
                         agency_end = 'activity_end', 
                         return_dfs = T)

# repeat, but now for task force
buffer_range <- c(0, 100, 187.5, 500, 1000, 2000, 4000)
res_buffer_range_tf <- map(
  buffer_range, 
  ~ calculate_overlap_buffer(tf_2021_2022, viirs_nonwf, .x,
                             acres_field = 'record_acres', 
                             agency_start = 'activity_start', 
                             agency_end = 'activity_end', 
                             return_dfs = F),
  .progress = T)

res_buffer_range_tf_df <- map2_df(res_buffer_range_tf, buffer_range, 
                                     ~ mutate(.x, buffer = .y, .after = category)) %>% 
  mutate(source = ifelse(source=='Agency', 'Task Force', source))


# spatial and temporal overlap: PFIRS -------------------------------------

# run this assuming buffer ~ recorded acres
buffer_Amount <- acres_to_radius(pfirs$acres_burned)
res_record_acres_PFIRS <- calculate_overlap_buffer(pfirs, viirs_nonwf, buffer_Amount,
                                                   'acres_burned', 'burn_date', return_dfs = T)

# repeat, but now looking at a range of buffer radii
buffer_range <- c(.01, 100, 187.5, 500, 1000, 2000, 4000)
res_buffer_range_PFIRS <- map(
  buffer_range, 
  ~ calculate_overlap_buffer(pfirs, viirs_nonwf, .x, 'acres_burned', 'burn_date', return_dfs = F), 
  .progress = T)
res_buffer_range_PFIRS_df <- map2_df(res_buffer_range_PFIRS, buffer_range, 
                                     ~ mutate(.x, buffer = .y, .after = category)) %>% 
  mutate(source = ifelse(source=='Agency', 'PFIRS', source))




# visualize ---------------------------------------------------------------


# PFIRS ===================================================
d_recorded_acres <- filter(res_record_acres_PFIRS[[1]], category == 'spatiotemporal') %>% 
  mutate(max_acres_perc = pmax(acresV_perc, acresA_perc),
         mean_acres_perc = (acresV_perc + acresA_perc)/2,
         source = ifelse(source == 'Agency', 'PFIRS', source)) 

p_acres <- res_buffer_range_PFIRS_df %>% 
  filter(category == 'spatiotemporal') %>% 
  mutate(max_acres_perc = pmax(acresV_perc, acresA_perc),
         mean_acres_perc = (acresV_perc + acresA_perc)/2) %>% # print(width = Inf)
  ggplot(aes(buffer, mean_acres_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Estimated acreage (%)', title = 'Spatiotemporal overlap') + 
  geom_hline(data = d_recorded_acres, aes(yintercept = mean_acres_perc), lty = 2) +
  labs(caption = '**Dotted line represents overlap when PFIRS polygon area = recorded acres burned**') +
  scale_y_continuous(limits = c(0, NA))
p_n <- res_buffer_range_PFIRS_df %>% 
  filter(category == 'spatiotemporal') %>% 
  ggplot(aes(buffer, n_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Activities/detections (%)', title = 'Spatiotemporal overlap') + 
  geom_hline(data = d_recorded_acres, aes(yintercept = n_perc), lty = 2) +
  labs(caption = '**Dotted line represents overlap when PFIRS polygon area = recorded acres burned**')+
  scale_y_continuous(limits = c(0, NA))
ggsave(plot = p_acres, 
       file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/figures/pfirs_vs_viirs_buffers_acres.png'), 
       width = 10, height = 5)
ggsave(plot = p_n, 
       file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/figures/pfirs_vs_viirs_buffers_n.png'), 
       width = 10, height = 5)
write_csv(res_buffer_range_PFIRS_df, file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/datasets/buffer_range_PFIRS_df.csv'))




# Task Force ===================================================

p_acres <- res_buffer_range_tf_df %>% 
  filter(category == 'spatiotemporal') %>% 
  mutate(max_acres_perc = pmax(acresV_perc, acresA_perc),
         mean_acres_perc = (acresV_perc + acresA_perc)/2) %>% # print(width = Inf)
  ggplot(aes(buffer, mean_acres_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Estimated acreage (%)', 
       title = 'Spatiotemporal overlap') +
  scale_y_continuous(limits = c(0, NA))
p_n <- res_buffer_range_tf_df %>% 
  filter(category == 'spatiotemporal') %>% 
  ggplot(aes(buffer, n_perc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ source, scales = 'free_y') +
  labs(x = 'Buffer radius (m)', y = 'Activities/detections (%)', title = 'Spatiotemporal overlap') +
  scale_y_continuous(limits = c(0, NA))
ggsave(plot = p_acres, 
       file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/figures/taskforce_vs_viirs_buffers_acres.png'), 
       width = 10, height = 5)
ggsave(plot = p_n, 
       file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/figures/taskforce_vs_viirs_buffers_n.png'), 
       width = 10, height = 5)
write_csv(res_buffer_range_tf_df, file.path(shared_path, 'Lisa_VIIRS_vs_RxFire/datasets/buffer_range_TaskForce_df.csv'))












# venn diagram ------------------------------------------------------------

# get table to make a venn diagram



# haven't updated the script below to reflect generalization from pfirs to agency
# df_VD <- res_record_acres[[1]] %>% 
#   mutate(category = ifelse(category == 'spatiotemporal', 'spatiotemporal', 'none')) %>% 
#   group_by(source, category) %>% 
#   summarise(acresV = sum(acresV), acresA = sum(acresA)) %>% 
#   mutate(across(c(acresV, acresA), ~ get_perc(.x), .names = '{.col}_perc'), 
#          mean_acres = round((acresV  + acresA )/2/1000))
# 
# # make a venn diagram
# PFIRS_only <- df_VD$mean_acres[df_VD$source == 'PFIRS' & df_VD$category == 'none']
# VIIRS_only <- df_VD$mean_acres[df_VD$source == 'VIIRS' & df_VD$category == 'none']
# both <- df_VD$mean_acres[df_VD$source == 'PFIRS' & df_VD$category == 'spatiotemporal']
# p_VD <- c(paste0(rep('PFIRS', PFIRS_only), 1:PFIRS_only), 
#           paste0(rep('both', both), 1:both) )
# v_VD <- c(paste0(rep('VIIRS', VIIRS_only), 1:VIIRS_only), 
#           paste0(rep('both', both), 1:both))
# VennDiagram::venn.diagram(
#   x = list(p_VD, v_VD),
#   category.names = c('PFIRS', 'VIIRS'),
#   filename = 'tmp_outputs/venn_diagramm_pfirs_vs_viirs.png',
#   output=T, 
#   
#   # circles customize
#   lwd = 2, 
#   fill = c('#FF0000', '#5BBCD6'),
#   col = c('#FF0000', '#5BBCD6'), 
#   
#   # change size of names
#   cex = 0, 
#   cat.cex = 0
# )
# 
