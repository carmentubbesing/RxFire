# epa estimates vs frap

# load libraries
library(tidyverse)
library(sf)
library(fs)

# load frap data
path <- dir_ls(file.path(ref_path, 'FRAP'), glob = '*.gdb', recurse = T)
frap_wf <- st_read(path, layer = 'firep22_1')
frap_rx <- st_read(path, layer = 'rxburn22_1')

# filter to 2022
frap_wf <- frap_wf %>% 
  filter(year(CONT_DATE) == 2022)#| year(ALARM_DATE) == 2022)
frap_rx <- frap_rx %>% 
  filter(year(END_DATE) == 2022)

# load epa's estimates--CA wildfire acres
nifc_wf <- 309287
nei_wf <- 370766

# compare to frap
wf_area <- st_area(frap_wf$Shape)
frap_wf_acres <- units::set_units(wf_area, acres) %>% sum %>% as.numeric()


tibble(source = c('FRAP', 'NIFC', 'NEI'),
       acres = c(frap_wf_acres, nifc_wf, nei_wf))
# source   acres
# <chr>    <dbl>
# 1 FRAP   478273.
# 2 NIFC   309287 
# 3 NEI    370766 


# compare rx fire ---------------------------------------------------------

# NEI 
NEI_locs <- read_csv(file.path(ref_path, 'US EPA/2022FireLoc_California.csv'))
NEI_locs <- NEI_locs %>% 
  select(date, id, event_name, latitude, longitude, county, area, sources, scc_description)
NEI_locs$scc_description %>% unique

NEI_distinct <- NEI_locs %>% 
  mutate(Rx = grepl('prescribed', scc_description), 
         FS = ifelse(grepl('flaming', scc_description), 'F', 'S') 
           ) %>% 
  filter(Rx == T) %>% 
  distinct(date, id, event_name, latitude, longitude, .keep_all = T) 
nei_rx <- sum(NEI_distinct$area)


# PFIRS
pfirs <- readxl::read_xlsx(file.path(ref_path, 'PFIRS/PFIRS 2017-2022 pulled 2023.xlsx'))
pfirs_2022 <- pfirs %>% 
  filter(year(`Burn Date`) == 2022) %>% 
  # remove exact duplicates
  distinct
pfirs_acres <- sum(pfirs_2022$`Acres Burned`)

# calmapper
path <- file.path(ref_path, 'CalMAPPER/treatment-activities.rds')
CM <- read_rds(path)
CM$UNIT_OF_MEASURE %>% unique # use only those with acres
unique(CM$ACTIVITY_DESCRIPTION)
unique(CM$st)
fire_activities <- c('Pile Burning', 'Broadcast Burn', 'Cultural Burning')
CM_filt <- CM %>% 
  filter(year(ACTIVITY_END) == 2022, 
         UNIT_OF_MEASURE == 'Acres', 
         #ACTIVITY_STATUS == 'Complete', 
         ACTIVITY_DESCRIPTION %in% fire_activities) 
CM_acres <- sum(CM_filt$QUANTITY)

tibble(source = c('NEI', 'PFIRS + CM', 'PFIRS', 'CalMAPPER'),
       acres = c(nei_rx, pfirs_acres + CM_acres, pfirs_acres, CM_acres))
# source       acres
# <chr>        <dbl>
# 1 NEI        186090.
# 2 PFIRS + CM  88179.
# 3 PFIRS       72102.
# 4 CalMAPPER   16078.
 
frap_wf_acres-nei_wf # 107507 (nei underestimates WF compared to FRAP)
nei_rx - (pfirs_acres + CM_acres) # 82254 (nei overestimates Rx compared to PFIRS + CM)





# why dont my values match carmen's ---------------------------------------
CM_carmen <- 'https://github.com/carmentubbesing/RxFire/raw/main/Rdata/CalMapper_activities_fire.Rdata'
load(url(CM_carmen))
CM_carmen <- cm

# compare calmapper to carmen's
full_join(
  CM_carmen %>% 
  st_drop_geometry() %>% 
    mutate(ACTIVITY_END = as.Date(ACTIVITY_END)) %>% 
  filter(ACTIVITY_DESCRIPTION %in% fire_activities) %>% 
  count(ACTIVITY_DESCRIPTION, year(ACTIVITY_END)) %>% 
  mutate(type = 'CM_carmen'),

CM %>% 
  st_drop_geometry() %>% 
  mutate(ACTIVITY_END = as.Date(ACTIVITY_END)) %>% 
  filter(ACTIVITY_DESCRIPTION %in% fire_activities) %>% 
  count(ACTIVITY_DESCRIPTION, year(ACTIVITY_END)) %>% 
  mutate(type = 'CM_lisa')
) %>% 
  pivot_wider(names_from = type, values_from = n) 

# same for 2022. sum up the acres?
CM %>%
  mutate(ACTIVITY_END = as.Date(ACTIVITY_END)) %>%
  filter(year(ACTIVITY_END) == 2022, 
         #UNIT_OF_MEASURE == 'Acres', 
         #ACTIVITY_STATUS == 'Complete',
         ACTIVITY_DESCRIPTION %in% fire_activities) %>% 
  pull(QUANTITY) %>% sum # 33574.59
CM_carmen %>%
  mutate(ACTIVITY_END = as.Date(ACTIVITY_END)) %>%
  filter(year(ACTIVITY_END) == 2022, 
         #UNIT_OF_MEASURE == 'Acres', 
         #ACTIVITY_STATUS == 'Complete', 
         ACTIVITY_DESCRIPTION %in% fire_activities) %>% 
  pull(TREATED_ACRES) %>% sum


# do the same as pfirs
pfirs_carmen <- 'https://github.com/carmentubbesing/RxFire/raw/main/Rdata/PFIRS_2017-2022_pull2023.Rdata'
load(url(pfirs_carmen))
pfirs_carmen <- pfirs

pfirs_carmen %>% 
  mutate(Burn_Date = as.Date(Burn_Date)) %>% 
  filter(year(Burn_Date) == 2022) %>% 
  pull(Acres_Burned) %>% sum # 72102

pfirs_2022 %>% 
  pull(`Acres Burned`) %>% sum # 72102
