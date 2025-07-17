generate_met_data <- function(site){

    centroids_df <- paste0(local_path,site,
                           "/from_siteplan/narrow_boundary.shp") %>%
      st_read() %>%
      st_centroid() %>%
      st_coordinates
    
    # Extract latitude and longitude
    coordinates <-c(centroids_df[, "X"],
                    centroids_df[, "Y"])
  
  library(rgee)
    
  # Initialize Earth Engine
  ee_Initialize()
  
  # Define start and end dates
  start_date <- ee$Date('2023-04-09')
  end_date <- start_date$advance(200, 'day')
  
  # Load image collections
  rtma <- ee$ImageCollection("NOAA/NWS/RTMA")$filterDate(start_date, end_date)
  era5land <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$filterDate(start_date, end_date)
  
  # Define the target point
  target <- ee$Geometry$Point(coordinates)
  
  # Select relevant bands from RTMA and ERA5-LAND
  x_rtma <- rtma$select(c('TMP', 'DPT', 'PRES', 'WIND'), c('Tair', 'Tdew', 'pres', 'U'))
  x_era <- era5land$select(c('surface_solar_radiation_downwards_hourly', 
                             'surface_thermal_radiation_downwards_hourly',
                             'surface_sensible_heat_flux_hourly', 
                             'surface_latent_heat_flux_hourly',
                             'total_precipitation_hourly'),
                           c('kdown', 'ldown', 'qh', 'qe', 'rain'))
  
  collection1 <- ee$ImageCollection(x_rtma)
  collection2 <- ee$ImageCollection(x_era)
  
  # Convert image collections to lists
  list1 <- x_rtma$toList(collection1$size())
  list2 <- x_era$toList(collection2$size())
  
  #print(list1$size()$getInfo()) # 8758
  #print(list2$size()$getInfo()) # 8760
  
  # Define a function to merge bands from synchronous images
  merge_collections <- function(collection1, collection2) {
    # Function to join two images based on timestamp
    mergeImages <- function(img1, img2) {
      timestamp1 <- ee$Date(img1$get('system:time_start'))
      timestamp2 <- ee$Date(img2$get('system:time_start'))
      
      # Find the corresponding image in the second collection
      isSameTimestamp <- timestamp1$difference(timestamp2, 'second')$abs()$lt(10);
  
      # Merge bands if timestamps match
      return(ee$Algorithms$If(isSameTimestamp,
                              ee$Image(img1)$addBands(img2),
                              NULL))
    }
    
    # Map over list1 to merge images
    merged_images <- list1$map(
      ee_utils_pyfunc(
        function(img1) {
        img1 <- ee$Image(img1)
        timestamp1 <- ee$Date(img1$get('system:time_start'))
        matchingImg2 <- list2$filter(ee$Filter$eq('system:time_start', timestamp1$millis()))$get(0)
        mergeImages(img1, ee$Image(matchingImg2))
        }
      )
    )
    
    # Remove null values and convert to ImageCollection
    return(ee$ImageCollection(ee$List(merged_images)))
  }
  
  # Merge the collections
  merged_collection <- merge_collections(collection1, collection2)
  
  # Calculate relative humidity function
  calculate_rh <- function(Tair, Tdew) {
    es_Tair <- Tair$multiply(17.67)$divide(Tair$add(243.5))$exp()
    es_Tdew <- Tdew$multiply(17.67)$divide(Tdew$add(243.5))$exp()
    RH <- es_Tdew$divide(es_Tair)$multiply(100)
    return(RH)
  }
  
  # Function to extract band values at the point
  extract_values <- function(image) {
    values <- image$reduceRegion(
      reducer = ee$Reducer$first(),
      geometry = target,
      scale = 30
    )
    Tair <- ee$Number(values$get('Tair'))
    Tdew <- ee$Number(values$get('Tdew'))
    U <- ee$Number(values$get('U'))
    pres <- ee$Number(values$get('pres'))$divide(1000)
    kdown <- ee$Number(values$get('kdown'))$divide(3600)
    ldown <- ee$Number(values$get('ldown'))$divide(3600)
    qe <- ee$Number(values$get('qe'))$divide(-3600)
    qh <- ee$Number(values$get('qh'))$divide(-3600)
    RH <- calculate_rh(ee$Number(values$get('Tair')), ee$Number(values$get('Tdew')))
    rain <- ee$Number(values$get('rain'))
    
    return(ee$Feature(NULL, list(
      Tair = Tair,
      Tdew = Tdew,
      U = U,
      pres = pres,
      kdown = kdown,
      ldown = ldown,
      qe = qe,
      qh = qh,
      RH = RH,
      rain = rain
    )))
  }
  
  # Map the extraction function over the merged collection
  extracted <- ee$FeatureCollection(merged_collection$map(extract_values))
  
  task1 <- ee_table_to_drive(
    extracted,
    description = paste0("2023_met_export_",site),
    fileNamePrefix = paste0("2023_met_export_",site),
    fileFormat = 'CSV'
  )
  
  # Start the export task
  task1$start()
}