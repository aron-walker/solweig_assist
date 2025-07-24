calc_cooling_efficiency <- function(task){

calc_cooling_efficiency_site <- function(site,name){

# Take the first path, first raster; will need its CRS
raster_crs <- list.files(paste0(local_path,site,"/y0_1_1_0"),
                      pattern = paste0("^","WBGT",".*\\.tif$"),
                      full.names = TRUE) %>%
              first() %>% rast() %>% crs()

# Load and reproject site boundary
boundary <- paste0(local_path,site,"/from_siteplan/narrow_boundary.shp") %>%
  st_read(quiet = TRUE) %>% 
  vect() %>%
  project(raster_crs)

# # Initialize a list to store averages for each path
# averages_list <- list()

get_mean_raster_value <- function(raster){
  raster %>% rast() %>% mask(boundary) %>% global(fun = mean, na.rm = TRUE)
}

get_directory_mean <- function(path, metric){
  list.files(path,
             pattern = paste0("^",metric,".*\\.tif$"),
             full.names = TRUE) %>%
    pblapply(get_mean_raster_value) %>%
    unlist() %>%
    mean(na.rm = TRUE)
}
threshold <- 2240
if (site %in% c("chicago","chicago_add_canopies")){threshold <- 2440}

get_usable_fraction <- function(path){
  paste0(local_path,site,"/intermediates/",
         path,
         "_",threshold,
         "_","narrow_boundary",".rds") %>%
    readRDS() %>%
    unlist() %>%
    as.numeric() %>%
    mean(na.rm = TRUE)
}

WBGT_results <- c(get_directory_mean(paste0(local_path,site,"/y0_1_1_0"),"WBGT"),
                  get_directory_mean(paste0(local_path,site,"/y5_1_1_0"),"WBGT"),
                  get_directory_mean(paste0(local_path,site,"/y10_1_1_0"),"WBGT"),
                  get_directory_mean(paste0(local_path,site,"/y15_1_1_0"),"WBGT"),
                  get_directory_mean(paste0(local_path,site,"/y20_1_1_0"),"WBGT"),
                  if (site == "corlears") {get_directory_mean(paste0(local_path,site,"/y5_1_1_1"),"WBGT")}
                  else {get_directory_mean(paste0(local_path,site,"/y5_0_1_1"),"WBGT")})

MRT_results <- c(get_directory_mean(paste0(local_path,site,"/y0_1_1_0"),"Tmrt"),
                  get_directory_mean(paste0(local_path,site,"/y5_1_1_0"),"Tmrt"),
                  get_directory_mean(paste0(local_path,site,"/y10_1_1_0"),"Tmrt"),
                  get_directory_mean(paste0(local_path,site,"/y15_1_1_0"),"Tmrt"),
                  get_directory_mean(paste0(local_path,site,"/y20_1_1_0"),"Tmrt"),
                 if (site == "corlears") {get_directory_mean(paste0(local_path,site,"/y5_1_1_1"),"Tmrt")}
                 else {get_directory_mean(paste0(local_path,site,"/y5_0_1_1"),"Tmrt")})

UTCI_results <- c(get_directory_mean(paste0(local_path,site,"/y0_1_1_0"),"UTCI"),
                 get_directory_mean(paste0(local_path,site,"/y5_1_1_0"),"UTCI"),
                 get_directory_mean(paste0(local_path,site,"/y10_1_1_0"),"UTCI"),
                 get_directory_mean(paste0(local_path,site,"/y15_1_1_0"),"UTCI"),
                 get_directory_mean(paste0(local_path,site,"/y20_1_1_0"),"UTCI"),
                 if (site == "corlears") {get_directory_mean(paste0(local_path,site,"/y5_1_1_1"),"UTCI")}
                 else {get_directory_mean(paste0(local_path,site,"/y5_0_1_1"),"UTCI")})

usability_results <- c(get_usable_fraction("y0_1_1_0"),
                       get_usable_fraction("y5_1_1_0"),
                       get_usable_fraction("y10_1_1_0"),
                       get_usable_fraction("y15_1_1_0"),
                       get_usable_fraction("y20_1_1_0"),
                       if (site == "corlears") {get_usable_fraction("y5_1_1_1")}
                       else {get_usable_fraction("y5_0_1_1")})

results <- data.frame(years = c(5,10,15,20),
                      MRT = MRT_results[2:5],
                      scaled_MRT = (MRT_results[2:5] - MRT_results[1])/(MRT_results[6] - MRT_results[1]),
                      WBGT = WBGT_results[2:5],
                      scaled_WBGT = (WBGT_results[2:5] - WBGT_results[1])/(WBGT_results[6] - WBGT_results[1]),
                      UTCI = UTCI_results[2:5],
                      scaled_UTCI = (UTCI_results[2:5] - UTCI_results[1])/(UTCI_results[6] - UTCI_results[1]),
                      usability = usability_results[2:5],
                      scaled_use = (usability_results[2:5] - usability_results[1])/(usability_results[6] - usability_results[1]))
                      


merged_df <- results


df_long <- merged_df %>%
  pivot_longer(cols = starts_with("scaled"),
               names_to = "variable",
               values_to = "value")

df_long$variable <- rep(c("average MRT decrease","average WBGT decrease","average UTCI decrease","usability increase"),4)
df_long$site <- rep(name, each = 16)

return(df_long)
}

if (task == 1){
df_full <- rbind(calc_cooling_efficiency_site("chicago",chicago_short),
                 calc_cooling_efficiency_site("corlears",corlears_short),
                 calc_cooling_efficiency_site("los_angeles",los_angeles_short),
                 calc_cooling_efficiency_site("vince",vince_short))
}

if (task == 2){
  df_full <- rbind(calc_cooling_efficiency_site("los_angeles",
                                                paste0(los_angeles_short, " as renovated")),
                   calc_cooling_efficiency_site("los_angeles_big_trees",
                                                paste0(los_angeles_short, " with bigger trees")))
  }

if (task == 3){
  df_full <- rbind(calc_cooling_efficiency_site("chicago",
                                                paste0(chicago_short, " as renovated")),
                   calc_cooling_efficiency_site("chicago_add_canopies",
                                                paste0(chicago_short, " with added canopies")))
  }

ggplot(df_full, aes(x = years, y = value*100, color = site, linetype = variable)) +
  geom_line() +
  labs(title = "Cooling Efficiency Over Time",
       x = "Years",
       y = "percent of maximum possible") +
  theme_minimal()

}
