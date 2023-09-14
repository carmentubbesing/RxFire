# to generate this project-level .rprofile, I used 
# `usethis::edit_r_profile(scope = "project")`

# Carmen's local paths
reference_path_Carmen <- "C:/Users/ctubbesi/OneDrive - California Air Resources Board/Documents/Reference data"

# Lisa's local path
reference_path_Lisa <- "C:/Users/lrosenth/OneDrive - California Air Resources Board/Desktop/Local_reference_data"

# detect who's directory exists and use that one.
if(fs::dir_exists(reference_path_Lisa)){
  ref_path <- reference_path_Lisa
  message("this is your reference path (`ref_path`) --> ", ref_path)
} else{
  if(fs::dir_exists(reference_path_Carmen)){
    ref_path <- reference_path_Carmen
    message("this is your reference path (`ref_path`) --> ", ref_path)
  }
  else stop("You don't have a valid path to your reference data.")
}

rm(reference_path_Carmen)
rm(reference_path_Lisa)
