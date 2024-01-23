library(sf)
library(fs)
library(tidyverse)

# these are too massive to load, let's load them from carmen's Rdata files
# facts_haz <- file.path(ref_path, 'FACTS/S_USA.Activity_HazFuelTrt_PLgdb') %>% 
#   dir_ls(glob = '*.gdb') %>% 
#   st_read()
# facts_com <- file.path(ref_path, 'FACTS/S_USA.Actv_CommonAttribute_PL') %>% 
#   dir_ls(glob = '*.shp') %>% 
#   st_read()

# load files
load(file.path(ref_path, 'FACTS/facts_haz.rdata'))
load(file.path(ref_path, 'FACTS/facts_common.rdata'))

# convert to a shapefile so i can load into arcpro
# st_write(facts_haz, file.path(ref_path, 'FACTS/facts_haz.shp'))
# st_write(facts_comm, file.path(ref_path, 'FACTS/facts_comm.shp'))


# check out the datasets
head(facts_comm)
head(facts_haz)

# what years are in each dataset?
as.Date(facts_comm$DATE_AWARD) %>% 
  year() %>% 
  unique() %>% 
  sort
as.Date(facts_haz$DATE_AWARD) %>% 
  year() %>% 
  unique() %>% 
  sort

# lets look at just 2021 and just fires...
codes <- c(1111, 1112, 1113, 1130, 3340, 4471, 4481, 4491, 4541, 7050, 7015,6101, 2540)
facts_comm21 <- facts_comm %>% 
  st_drop_geometry() %>% 
  filter(year(as.Date(DATE_AWARD)) == 2021,
         ACTIVITY_C %in% codes) 
facts_haz21 <- facts_haz %>% 
  st_drop_geometry() %>% 
  filter(year(as.Date(DATE_AWARD)) == 2021,
         ACTIVITY_C %in% codes) 

# compare column names...
f_contains <- function(x, y) x %in% y
# are the columns in common a subset of those in hazard?
tibble(commons_col = names(facts_comm), 
       shared = f_contains(names(facts_comm), names(facts_haz))) %>% 
  print(n = Inf)
tibble(hazard_col = names(facts_haz), 
       shared = f_contains(names(facts_haz), names(facts_comm))) %>% 
  print(n = Inf)

# hmm, maybe just join them and see what happens?
# Joining with `by = join_by(
#   FACTS_ID, FEATURE_TY, ACTIVITY_C, ACTIVITY, EQUIPMENT_,EQUIPMENT, FISCAL_YEA,
#   FISCAL_Y_1, DATE_AWARD, DATE_COMPL, TREATMENT_, ADMIN_FORE, STATE_ABBR)`
joined <- full_join(facts_comm21, facts_haz21)
dim(joined)

# ok, comm still has more rows: 823 vs. 729. are there duplicates?
head(facts_comm21)
head(facts_haz21)
duplicated(facts_comm21$FACTS_ID) %>% sum
duplicated(facts_haz21$FACTS_ID) %>% sum

facts_comm21 %>% 
  group_by(FACTS_ID) %>% 
  mutate(n = n()) %>%
  arrange(desc(n)) %>% 
  print(width = Inf)




# figure out if haz is a subset of comm


