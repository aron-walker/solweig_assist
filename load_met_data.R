load_met_data <- function(site){
  
  prefix <- paste0("2023_met_export_",site)
  
  # List all files in the directory
  all_files <- list.files(gee_path, full.names = TRUE)
  
  # Filter files that start with the prefix and end with ".csv"
  matching_files <- grep(paste0("^", prefix, ".*\\.csv$"), basename(all_files), value = TRUE)
  matching_files <- file.path(gee_path, matching_files)
  
  # Check if any matching files were found
  if (length(matching_files) == 0) {
    stop("No matching CSV files found")
  }
  
  # Read the first matching CSV file
  return(read.csv(tail(matching_files,1)))
  
}