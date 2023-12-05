# how do viirs detects change over time at the burning man location?
# will use this to demonstrate the sensitivity of viirs to artificial fire events.

library(tidyverse)
library(sf)
library(mapview)
library(fs)
library(cowplot)

# load data
path <- file.path(ref_path, 'VIIRS/BM') %>% 
  dir_ls(recurse = T, glob = "*.csv")
dat <- read_csv(path) %>% 
  st_as_sf(coords = c("longitude", 'latitude'), crs = 4326)
mapview(dat, legend = F)

# detects over the date range
date_st <- ymd("2019-08-01")
date_ed <- ymd("2019-09-30")
date_seq <- seq(date_st, date_ed, 1)
ymd(dat$acq_date)
date_seq

p1 <- dat %>%
  as_tibble %>% 
  count(acq_date) %>% 
  rename(date = acq_date) %>% 
  right_join(data.frame(date = date_seq)) %>% 
  mutate(n = coalesce(n, 0)) %>% 
  arrange(date) %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  theme_bw() +
  labs(y = 'VIIRS Detects', x = "Date")

p2 <- ggdraw(p1) +
  draw_image('tmp_outputs/BM_map.png', width = .3, y = .2, x = .6) +
  draw_image("https://i.insider.com/5d78fbe16f24eb1d7d61d754?width=1000&format=jpeg&auto=webp", 
             width = .3, y = .2, x = .2)

ggsave(filename = "tmp_outputs/BM_detects.png", plot = p2,
       width = 5, height = 3, units = "in")
