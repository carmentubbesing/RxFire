
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

f_spatiotemporal_match <- function(x, y,  x_date, y_start, y_end = NULL){
  
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
  
  return(res)
}
