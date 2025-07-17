Set of three scripts to generate, load, and extract/reformat meteorological data for SOLWEIG.

generate_met_data.R uses rgee to generate an extract combining ERA5 with RTMA, since RTMA is higher resolution (but does not have all of the variables SOLWEIG needs).

Here is how generate_met_data.R, load_met_data.R, and extact_met_data.R work in combination, as R code:

'source(paste0(script_path,"generate_met_data.R")) # Calls Google Earth Engine (GEE)'

'source(paste0(script_path,"load_met_data.R")) # Retrieves the result from GEE'

'source(paste0(script_path,"extract_met_data.R")) # Processes and formats the data for SOLWEIG'

'   generate_met_data(site) # Extracts met data at centroid of project area'

'   print("Pausing 5 minutes for Google Earth Engine to work and Google Drive to sync")'

'   countdown_timer(300) # Enough time for GEE to run and Google Drive to sync'

'   load_met_data(site) %>% extract_met_data()'

'   print("Meteorological data retrieved and formatted")'
