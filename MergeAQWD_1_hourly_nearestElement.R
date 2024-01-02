##################################################
## Project: AirOldUpdate
## Date: 08.06.2023
## Author: Mika Erdmann
##################################################
# This script matches the air quality stations for germany to the weather stations,
# based on an approximate nearest neighbour search. Weather station Ids are merged
# to the Air Quality dataset based on this matching result

# Imports
library(dplyr)
library(sf)
library(RANN)
library(lubridate)
library(data.table)
library(readr)
library(readxl)
library(stringr)
savepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim"
wdpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/WD_hourly"
aqpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/AQ_hourly"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/Stations"
subelements <- c("precipitation", "air_temperature", "dew_point", "wind", "extreme_wind", "pressure", "sun", "cloudiness", "moisture")
short_sub <- c("TU", "TD","FF", "FX", "TF", "RR", "P0", "SD", "N") # short identifier used in file zip names


# Data --------------------------------------------------------------------

# This section loads the data for weather stations (`DWD_stations.rds`) and 
# air quality stations (`AQ_stations.rds`) into the respective variables (`WD_stations` and `AQ_stations`).

# WD stations
AQ_stations <- readRDS(paste0(stationpath, "/AQ_stations.rds")) # AQ stations

# Construct elemenwise WD-Stations
for (el in 1:9){
  element <- subelements[el]
  #WD_stations <- read_delim(paste0(stationpath, "/hourly_", element, "_Stationen.txt"))
  WD_stations <- read_delim(paste0(stationpath, "/hourly_", element, "_Stationen.csv"), delim = ";")
  WD_stations <- WD_stations %>% 
    rename(STATIONS_ID = Stations_id) %>%
    mutate(STATIONS_ID = as.integer(STATIONS_ID)) %>%
    # mutate(geoBreite = as.character(geoBreite)) %>%
    # mutate(geoLaenge = as.character(geoLaenge)) %>%
    # mutate(geoBreite= str_remove(geoBreite, "\\.")) %>%
    # mutate(geoLaenge= str_remove(geoLaenge, "\\.")) %>%
    mutate(geoBreite = as.numeric(str_replace(geoBreite, pattern ="[:digit:]{4}$", replacement = paste0(".", str_sub(geoBreite, start = -4L, end = -1L))))) %>%
    mutate(geoLaenge = as.numeric(str_replace(geoLaenge, pattern ="[:digit:]{4}$", replacement = paste0(".", str_sub(geoLaenge, start = -4L, end = -1L)))))
    
  dwd <- readRDS(paste0(wdpath, "/hourly_dwd_", element, "_2000-2023.rds")) %>%
    mutate(MESS_DATUM = as_datetime(MESS_DATUM))
  #counts <- dwd %>%
  #  group_by(STATIONS_ID) %>%
  #  count()
  dwd_stations <- dwd %>%
    #group_by(STATIONS_ID) %>% 
    #filter(n()>mean(counts$n)) %>%
    #ungroup() %>%# filter only those where for each stationsID there is more than X obs WHy
    select(STATIONS_ID) %>%
    unique() 
# Filter WD stations for elementwise WD-stations
  WD_stations <- WD_stations %>% 
    filter(STATIONS_ID %in% dwd_stations$STATIONS_ID)
  
  WD_coord <- WD_stations[,c(6,5)] # Get coordinate matrices
  AQ_coord <- AQ_stations[, c(2,3)] 
  
  #Transform coordinates into simple features (sf) and extract geometry object for nn2 function
  # This section converts the coordinate matrices into simple features using the 
  # st_as_sf` function from the `sf` package. It creates spatial objects 
  # (`WD_sf` and `AQ_sf`) that contain the station data along with their spatial coordinates.
  
  WD_sf <- st_as_sf(WD_stations, coords = c(6,5))
  AQ_sf <- st_as_sf(AQ_stations, coords = c(2,3))
  WD_sfc <- do.call(rbind, st_geometry(WD_sf))
  AQ_sfc <- do.call(rbind, st_geometry(AQ_sf))
  
  
  # Nearest neighbour search ------------------------------------------------
  # This section performs a fast nearest neighbor search using the `nn2` function 
  # from the `RANN` package. It finds the nearest weather station for each 
  # air quality station within a specified radius. The results are stored in the 
  # `closest` variable.
  
  
  # The radius is in decimal degrees. The value 0.2 corresponds to 22 km at the equator and approx. 14km in germany)
  # See https://en.wikipedia.org/wiki/Decimal_degrees
  # The largest distance that is inclueded in this calculation for r = 0.2 is 20.44 km for the AQ station debb007
  # The largest distance included for r = 0.3 is 26.06 km for the station dest089.
  # For 0.4, the value is approx. 44km max
  
  #closest <- nn2(WD_sfc,AQ_sfc ,k = 1, searchtype = "radius", radius = 0.4)
  closest <- nn2(WD_sfc,AQ_sfc ,k = 10, searchtype = "radius", radius = 0.4)
  # might have to adapt the following lines when I use k = 10 here. 
  # 
  closest_indx <- closest$nn.idx %>% as_tibble()
  # Merge AQ stations to matched table 
  distances <- closest$nn.dists %>% as_tibble()
  closest_matched <-closest_indx %>%
    bind_cols(AQ_sf) 
  
  
  # # find weather station id based rann function result --------------------
  
  # This section assigns an index (`nn.idx`) to each weather station in the 
  # `WD_sf` dataset. It is used to match the real weather station IDs in the 
  # `closest_df` table.
  
  row.names(WD_sf) <- 1:nrow(WD_sf)
  WD_sf_idx_ID <- WD_sf %>%
    mutate(nn.idx = row_number())
  
  # Merge and save ----------------------------------------------------------
  # first column
  idx_var <- 1
  var_name <- str_glue("V{idx_var}")
  closest_m <- closest_matched %>%
    rename("nn.idx" = {{var_name}})
  # Merge matched real WD station.ids in closest_df with the row ids from input matrix
  closest_df_all<- left_join(closest_m, WD_sf_idx_ID, by = c("nn.idx"))
  
  closest_df_all <- closest_df_all %>%
    select(-starts_with("V")) %>%
    select(-7) %>%
    #rename("geometry.AQ" = geometry.x) %>%
    #rename("geometry.WD" = geometry.y) %>%
    rename("AQ_station_id" = station) %>%
    rename( !! str_glue("WD_station_id_{idx_var}") := STATIONS_ID) %>%
    select(-nn.idx) %>%
    select(-bis_datum) %>% select(-Stationshoehe) %>%
    select(-starts_with("geometry"))
  
  # find unmatched stations
  closest_df_unmatched <- closest_df_all %>%
    filter(is.na(str_glue("WD_station_id_{idx_var}")))
  nrow(closest_df_unmatched)
  # Should be 0
  # Delete unmatched stations from closest_df
  closest_df_all <- closest_df_all %>%
    filter(!is.na(str_glue("WD_station_id_{idx_var}")))
  
  # rest of elements
  for (idx_var in 2:10){
    var_name <- str_glue("V{idx_var}")
    closest_m <- closest_matched %>%
      rename("nn.idx" = {{var_name}})
  # Merge matched real WD station.ids in closest_df with the row ids from input matrix
    closest_df<- left_join(closest_m, WD_sf_idx_ID, by = c("nn.idx"))
    
    closest_df <- closest_df %>%
      select(-starts_with("V")) %>%
      select(-7) %>%
      #rename("geometry.AQ" = geometry.x) %>%
      #rename("geometry.WD" = geometry.y) %>%
      rename("AQ_station_id" = station) %>%
      rename( !! str_glue("WD_station_id_{idx_var}") := STATIONS_ID) %>%
      select(-nn.idx) %>%
      select(-bis_datum) %>% select(-Stationshoehe) %>%
      select(-starts_with("geometry"))
    
    # find unmatched stations
    closest_df_unmatched <- closest_df %>%
      filter(is.na(str_glue("WD_station_id_{idx_var}")))
    nrow(closest_df_unmatched)
    # Should be 0
    # Delete unmatched stations from closest_df
    closest_df <- closest_df %>%
      filter(!is.na(str_glue("WD_station_id_{idx_var}")))
    closest_df_all <- left_join(closest_df_all, closest_df , by = c("AQ_station_id"))
  }
  

  saveRDS(closest_df_all, paste0(stationpath,"/AQWD_matchedstations_",element, ".rds"))
    
  
  
}



# Test Matching for mistakes ----------------------------------------------

# calculate distance 
dist<- tibble(AQ_station_id =character(), distance = numeric() )

for (i in 1:nrow(closest_df)){
  disti <- st_distance(closest_df$geometry.AQ[i], closest_df$geometry.WD[i] )
  dist <- dist %>% add_row(AQ_station_id = closest_df$AQ_station_id[i], distance = disti)
}
compare_dist <- left_join(closest_df, dist, by = "AQ_station_id")

