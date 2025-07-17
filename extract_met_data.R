## This function formats (and subsets to relevant hours) meteorological data
## Input: .csv export from Google Earth Engine
## Output: SOLWEIG-formatted .txt file

extract_met_data <- function(input_data){

  fill_999 <- rep(-999.0,dim(input_data)[1])
  
  # Transform the dataframe
  dates_split <- input_data %>%
    mutate(
      # Extract the year (first 4 characters)
      iy = as.numeric(substr(system.index, 1, 4)),
      
      # Extract the day of the year (characters 5 to 8)
      day_of_year = substr(system.index, 5, 8),
      
      # Extract the time (last 2 characters)
      it = as.numeric(substr(system.index, 9, 10))
    ) %>%
    mutate(
      # Format day of year as MMDD
      id = as.integer(format(as.Date(day_of_year, format = "%m%d"), "%j"))
    ) %>%
    # Select only needed columns
    dplyr::select(-c(system.index, day_of_year, .geo, Tdew))
  
  output_data <- dates_split
  output_data$imin <- rep(0, dim(input_data)[1]) # data are hourly only
  output_data$qn <- fill_999 # none of these are available from the sources
  output_data$qs <- fill_999
  output_data$qf <- fill_999
  output_data$snow <- fill_999
  output_data$fcld <- fill_999
  output_data$Wuh <- fill_999
  output_data$xsmd <- fill_999
  output_data$lai <- fill_999
  output_data$kdiff <- fill_999
  output_data$kdir <- fill_999
  output_data$wdir <- fill_999
  
  # Select and order output columns
  output_data <- output_data %>% 
    dplyr::select(c(iy, id, it, imin, qn, qh, qe, qs, qf, U, RH, Tair, 
                    pres, rain, kdown, snow, ldown, fcld, Wuh, 
                    xsmd, lai, kdiff, kdir, wdir)) %>%
    round(2)
  
  # Sometimes a row of data is missing. This section completes a missing row.
  complete_grid <- expand_grid(
    id = 100:299,
    it = 0:23
  )
  
  complete_data <- complete_grid %>%
    left_join(output_data, by = c("id", "it"))
  
  interpolated_data <- complete_data %>%
    group_by(id) %>%
    arrange(it) %>%
    mutate(across(everything(), ~ zoo::na.approx(., na.rm = FALSE))) %>%
    ungroup() %>%
    arrange(id)
  
  era5_hours <- (8:20 + time_zone_offset) %% 24
  
  # Filter rows where 'it' values are in time-zone adjusted range
  filtered_data <- interpolated_data %>%
    filter(it %in% era5_hours) %>%
    filter(id > 100 | it > 6) %>%
    filter(id < 299 | it < 6)
  
  properly_ordered <- filtered_data %>% 
    dplyr::select(c(iy, id, it, imin, qn, qh, qe, qs, qf, U, RH, Tair, 
                    pres, rain, kdown, snow, ldown, fcld, Wuh, 
                    xsmd, lai, kdiff, kdir, wdir))
  
  output_file <- paste0(local_path, site, "/met_data/2023_8am_to_8pm.txt")
  
  write.table(properly_ordered,
              file = output_file, sep = " ",
              row.names = FALSE, col.names = TRUE, quote = FALSE)
}