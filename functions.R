plot_pal <- function(pal){
  df <- tibble(color = pal, x = 1:length(pal))
  ggplot(df, aes(x, fill = color)) +
    geom_bar() +
    scale_fill_manual(values = pal)
}

bbox_buffered <- function(spatial_df, buffer = 1000){
  
  st_buffer(spatial_df, dist = buffer) %>% 
    st_bbox() %>% st_make_grid(n = 1)
}

pretty_names <- function(x) tolower(x) %>% tools::toTitleCase(.)

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

f_spatiotemporal_match <- function(x, y,   y_start, y_end = NULL, x_date = 'datetime', x_id = 'viirs_id'){
  
  # x = typically viirs sf dataframe
  # y = typically agency sf dataframe
  # assumes that x and y have id columns
  
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
    x_date <- as.Date(pull(x, x_date)[x_row])
    y_start <- as.Date(pull(spatial, y_start))
    if(!is.null(y_end)){
      y_end <- as.Date(pull(spatial, y_end))
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
  
  # change x_row to IDs. y_id is already there.
  res <- res %>% 
    mutate(x_id = pull(x, x_id)[x_row],
           .after = match,
           x_row = NULL, y_row = NULL) %>% 
    rename({{x_id}} := x_id)

  
  return(res)
} 

# viirs with matches
update_category <- function(viirs_extracted){
  viirs_extracted %>% 
    mutate(category = case_when(
      !is.na(fire_name) ~ 'Wildfire',
      match == 'spatial' ~ 'Spatial overlap',
      match == 'spatiotemporal' ~ 'Spatiotemporal overlap',
      CDL == "crop" ~ 'Crop',
      CDL == "developed" ~ "Developed",
      !is.na(power_source)|!is.na(solar)|!is.na(camping)|!is.na(landfill_nm) ~ 'Developed-artifactual', 
      density_2021 > 20 ~ 'Developed-artifactual',
      density_2022 > 20 ~ 'Developed-artifactual',
      CDL == 'other' ~ 'Developed-artifactual',
      T & month(datetime) >= 5 ~ 'Unaccounted:\nUS EPA assumed Wildfire (May-Dec)',
      T & month(datetime) < 5 ~ 'Unaccounted:\nUS EPA assumed Rx fire (Jan-Apr)'
    )) %>% 
    select(contains('id'), category)
}



# write a function so you can do this for different buffer sizes
calculate_overlap_buffer <- function(agency_records, viirs_nonwf, buffer_amount, acres_field, 
                                     agency_start, agency_end = NULL, return_dfs = F){
  
  
  # delete me
  # agency_records = tf_2021_2022
  # buffer_amount = 0
  # acres_field = 'record_acres'
  # agency_start = 'activity_start'
  # agency_end = 'activity_end'
  # 
  # buffer pfirs
  agency_buffered <- agency_records %>% 
    st_buffer(buffer_amount)
  
  
  # check for a spatio-temporal match with pfirs.
  match_df <- f_spatiotemporal_match(viirs_nonwf, agency_buffered, agency_start, agency_end) %>% 
    select(1:3, all_of(acres_field)) %>% 
    distinct %>% 
    arrange(desc(match == "spatiotemporal")) 
  
  # join back with viirs_nonwf
  viirs_match <- left_join(viirs_nonwf, 
                           # get one row per viirs point
                           match_df %>% 
                             group_by(viirs_id) %>% 
                             slice(1) %>%
                             ungroup())
  
  # join back with PFIRS
  col_name <- names(match_df)[3]
  agency_match <- left_join(mutate(agency_buffered, buffer_radius = buffer_amount), 
                            # get one row per pfirs point
                            match_df %>% 
                              group_by(.data[[col_name]]) %>% 
                              slice(1) %>%
                              ungroup()
  )
  
  # calculate overlap summaries
  
  # from VIIRS perspective
  viirs_summary <- viirs_match %>% 
    st_drop_geometry() %>% 
    # update the categories
    update_category() %>% 
    # filter out development and crop
    filter(category %in% c('Unaccounted:\nUS EPA assumed Rx fire (Jan-Apr)', 
                           'Unaccounted:\nUS EPA assumed Wildfire (May-Dec)',
                           'Spatial overlap',
                           'Spatiotemporal overlap')) %>% 
    # change names of categories
    mutate(category = case_when(
      category == 'Spatial overlap' ~ 'spatial',
      category == 'Spatiotemporal overlap' ~ 'spatiotemporal',
      TRUE ~ 'no_match'
    )) %>%
    count(category) %>% 
    mutate(acresV = n*34.7)
  
  # from agency's perspective
  agency_summary <- agency_match %>% 
    st_drop_geometry() %>% 
    mutate(match = replace_na(match, 'no_match')) %>% 
    group_by(match) %>% 
    summarise(n = n(),
              acresA =  sum(across(all_of(acres_field)), na.rm = T)
    ) %>% 
    rename(category = match)
  
  # merge summaries together. 
  # acresV = estimates acreage by multiplying # viirs points by 34.7 acres
  # acresA = acres burned according to records in agency's df
  merged_summary <- bind_rows(
    viirs_summary %>% mutate(source = 'VIIRS', .before = category),
    agency_summary %>% mutate(source = 'Agency', .before = category)
  ) %>% 
    # refactor category
    mutate(category = factor(category, levels = c('spatiotemporal', 'spatial', 'no_match'))) %>% 
    arrange(source, category)
  # make acresV = acresP when category == spatiotemporal
  merged_summary$acresV[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'Agency'] <- 
    merged_summary$acresV[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'VIIRS']
  merged_summary$acresA[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'VIIRS'] <- 
    merged_summary$acresA[merged_summary$category == 'spatiotemporal' & merged_summary$source == 'Agency']
  # get percentages
  merged_summary <- merged_summary %>% 
    mutate(acresV = coalesce(acresV, acresA), 
           acresA = coalesce(acresA, acresV) 
    ) %>% 
    group_by(source) %>% 
    mutate(across(c(n, acresV, acresA), ~ get_perc(.x), .names = '{.col}_perc')) %>% 
    ungroup() 
  
  if(return_dfs){
    return(list(merged_summary, viirs_match = viirs_match, agency_match = agency_match))
  }else{
    return(merged_summary)
  }
}
