

library(fs)
library(sf)
library(tidyverse)

source('functions.R')

# load data ---------------------------------------------------------------

# task force
path <- file.path(ref_path, 'task force/tf_2021_2022.geojson')
tf_2021_2022 <- st_read(path)

# import VIIRS
viirs_path <- file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_pfirs.geojson')
viirs <- st_read(viirs_path)

# import matches
tf_match <- read_csv(file.path(ref_path, 'VIIRS/CA_2021_2022/viirs_taskforce_matches.csv'))


# from VIIRS perspective... -----------------------------------------------

# merge task force and viirs
viirs_breakdown <- left_join(viirs, st_drop_geometry(tf_2021_2022), by = 'tf_id') %>% 
  mutate(acres_per_detect = if_else(record_acres > 34.74906 | is.na(record_acres), 
                                    34.74906, record_acres)) %>% 
  as_tibble %>% 
  mutate(category_tf = ifelse(category_tf == 'Developed-artifactual', 'Developed', category_tf)) %>% 
  group_by(category_tf) %>% 
  summarise(n = n(),
            acres = n*34.74906,
            acres_cor = sum(acres_per_detect, na.rm = T)) %>% 
  mutate(n_percent = n/sum(n)*100, .after = n,
         area_percent = acres_cor/sum(acres_cor)*100)
viirs_breakdown


# from TF perspective... --------------------------------------------------

tf_breakdown <- tf_match %>% 
  full_join(tf_2021_2022) %>% 
  #filter(activity_status == 'COMPLETE') %>% 
  arrange(desc(match)) %>% 
  filter(!duplicated(tf_id)) %>% 
  mutate(match = replace_na(match, 'none'), 
         year = year(activity_end)) %>% 
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

viirs_breakdown$pal <- c('#ffb500', '#6ed73f', '#5BBCD6', '#ff0000', '#666162', '#666162','#660000'   )

# from VIIRS perspective
viirs_breakdown %>% 
  ggplot(aes(area = acres_cor , fill= category_tf, 
             label = glue("{category_tf}"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 12) +
  scale_fill_manual(values = viirs_breakdown$pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_VIIRS_all.png', width = 6, height = 3.5)

# from VIIRS perspective, removing wildfire, crop, dev
viirs_breakdown2 <- viirs_breakdown %>% 
  filter(!category_tf %in% c("Crop", 'Developed', 'Wildfire')) %>% 
  mutate(area_percent = signif(acres_cor/sum(acres_cor)*100, 2))
ggplot(viirs_breakdown2, aes(area = acres_cor , fill= category_tf, 
             label = glue("{category_tf}"))) +
  geom_treemap(size = 4, color = 'grey100') +
  geom_treemap_text(colour = "white", place = "centre", grow = F, min.size = 12) +
  scale_fill_manual(values = viirs_breakdown2$pal) + 
  theme(legend.position = 'none') 
ggsave('tmp_outputs/treemap_VIIRS_subset.png', width = 6, height = 3.5)
# > viirs_breakdown2
# # A tibble: 4 × 7
# category_tf                             n n_percent area_percent  acres acres_cor pal  
# <chr>                               <int>     <dbl>        <dbl>  <dbl>     <dbl> <chr>
# 1 "Spatial overlap"                    3207     0.679           13 1.11e5    90578. #5BB…
# 2 "Spatiotemporal overlap"             2976     0.630           13 1.03e5    92135. #ff0…
# 3 "Unaccounted:\nUS EPA assumed Rx f…  5553     1.17            28 1.93e5   192962. #666…
# 4 "Unaccounted:\nUS EPA assumed Wild…  8851     1.87            45 3.08e5   307564. #666…


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
# > tf_breakdown
# # A tibble: 3 × 7
# match n_activities recorded_acres gis_acres p_n_activities p_recorded_acres p_gis_acres
# <chr>        <int>          <dbl>     <dbl>          <dbl>            <dbl>       <dbl>
#   1 none          3283        111153.   181912.             65               33          24
# 2 spat…         1174        154490.   318857.             23               46          42
# 3 spat…          627         73698.   250146.             12               22          33

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
       acres_375m2 = c(with(viirs_breakdown2, acres_cor[grep('Spatiotemporal overlap', category_tf)]),
                       filter(tf_breakdown, match %in% c('none', 'spatial')) %>% pull(recorded_acres) %>% sum, 
                       filter(viirs_breakdown2, !category_tf %in% grep('temp', category_tf, value = T)) %>% 
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
# category      acres_375m2 acres_polygons p_viirs_375 p_viirs_poly p_TF_375 p_TF_poly
# <chr>               <dbl>          <dbl>       <dbl>        <dbl>    <dbl>     <dbl>
# 1 both_VIIRS_TF      92135.         73698.       0.135        0.111    0.258     0.217
# 2 TF_only           265643.        265643.      NA           NA        0.742     0.783
# 3 VIIRS_only        591103.        591103.       0.865        0.889   NA        NA    


# attempts to make a venn diagram -----------------------------------------

library(VennDiagram)


TF_only <- 234
VIIRS_only <- 591
both <- 42


TF <- c(paste0(rep('TF', TF_only), 1:TF_only), 
        paste0(rep('both', both), 1:both) )
VIIRS <- c(paste0(rep('VIIRS', VIIRS_only), 1:VIIRS_only), 
           paste0(rep('both', both), 1:both))
venn.diagram(
  x = list(TF, VIIRS),
  category.names = c('TF', 'VIIRS'),
  filename = 'tmp_outputs/venn_diagramm.png',
  output=T, 
  
  # circles customize
  lwd = 2, 
  fill = c('#FF0000', '#5BBCD6'),
  col = c('#FF0000', '#5BBCD6'), 
  
  # change size of names
  cex = 0, 
  cat.cex = 0
)

