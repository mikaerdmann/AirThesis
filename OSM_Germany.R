library(sf)
library(tidyverse)
# Using osmdata
library(osmdata)


externalpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external"
stationspath_int <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/Stations"
# for each station download closest osm data (radius 1000m)
# count number of res. buildings
# number of closest roads or some index to indicate traffic

# Import all stations with coordinates
stat_coord <- readRDS(paste0(stationspath_int, "/AQ_stations.rds"))
stat_coord <- stat_coord %>%
  mutate(buildings_resi = NA) %>%
  mutate(distance_primaryroad = NA) %>%
  mutate(distance_secondaryroad = NA) %>%
  mutate(distance_tertiaryroad = NA) %>%
  mutate(number_primtotert = NA) %>%
  mutate(number_resi = NA) %>%
  mutate(distance_resi = NA)
  
for (i in 1:nrow(stat_coord)){
#for (i in 1:1){
  lat <- stat_coord$station_longitude_d[i]
  long <- stat_coord$station_latitude_d[i]
  rad <- 1000.0
  station_p<- tibble(lat = lat, long = long)
  # overpass query string
  s <- str_glue("[out:xml];(
    way(around:{rad},{long},{lat})[building]; 
  )  ->.a;
    (
      way(around:{rad},{long},{lat})[landuse != residential];
    ) -> .b;
  
  way.a.b -> .c;
  
  (way(around:{rad},{long},{lat})[highway];) ->.d;
  
  (.c;.d;); (._;>;);
  out meta;")
  
  stat <- osmdata_sf(s)
  stat_point <- st_point(c(long, lat))
  stat_name <- stat_coord[i,1]
  
  # Count the number of residential buildings in area
  # These are the tags for residential buildings that imply high prob. for fuelwood heating
  # from: https://wiki.openstreetmap.org/wiki/Map_features#Accommodation
  buildings <- c("residential", "house", "yes", "barracks", "cabin", 
                 "detached", "farm", "bungalow", "semidetached_house", "terrace")
  if ("building" %in% colnames(stat$osm_polygons)){
    poly <- stat$osm_polygons %>% 
      filter(!is.na(building))
    try(poly <- poly %>%
      filter(is.na(name)) %>%
      filter(is.na(amenity)) %>%
      filter(`building:levels` < 5))
    poly <- poly %>%
      filter(building %in% buildings) %>%
      select(c(osm_id, building))
    house_count <- count(poly)$n
    stat_coord[i,4] <- house_count
  }
  else {
    stat_coord[i,4] <- 0
    
  }
  # Proximity to primary road
  if ("highway" %in% colnames(stat$osm_lines)){
    lines <- stat$osm_lines %>%
      filter(!is.na(highway)) 
    if (nrow(lines) <1 | ! "osm_id" %in% colnames(lines)){
      print(str_glue("There are no streets for station {stat_name}(residential or 1-3"))
      stat_coord[i,5] <-Inf
      stat_coord[i,6] <- Inf
      stat_coord[i,7] <- Inf
      stat_coord[i,8] <- 0
      stat_coord[i,9] <- 0
      stat_coord[i,10] <- Inf
    }
    else {
      lines <- lines %>%
        select(c(osm_id, highway))
      
      primary <- lines %>%
        filter(highway == "primary")
      
      stat_point <- st_as_sf(x = station_p,                         
               coords = c("lat","long"),
               crs = "+proj=longlat +datum=WGS84")
      stat_point <-st_transform(stat_point, crs = st_crs(primary))
      
      dist_prim <- st_distance(stat_point, primary)
      min_prim<- as.numeric(min(dist_prim))
      
      # Distance to secondary road
      second <- lines %>%
        filter(highway == "secondary")
      
      dist <- st_distance(stat_point, second)
      min_second<- as.numeric(min(dist))
      
      tert <- lines %>%
        filter(highway == "tertiary")
      
      dist <- st_distance(stat_point, tert)
      min_tert<- as.numeric(min(dist))
      
      resi <- lines %>%
        filter(highway == "residential")
      
      dist <- st_distance(stat_point, resi)
      min_resi<- as.numeric(min(dist))
      
      # number of roads 1-3
      ptot <- c("primary", "secondary", "tertiary")
      prim_to_tert <- lines %>%
        filter(highway %in% ptot)
      ptot_count <- count(prim_to_tert)$n
      
      # number of res. roads
      resiroads <- lines %>%
        filter(highway == "residential")
      resi_count <- count(resiroads)$n
          stat_coord[i,5] <-min_prim
      stat_coord[i,6] <- min_second
      stat_coord[i,7] <- min_tert
      stat_coord[i,8] <- ptot_count
      stat_coord[i,9] <- resi_count
      stat_coord[i,10] <- min_resi
    }
  }
  else {
    print(str_glue("There are no streets for station {stat_name}(residential or 1-3"))
    stat_coord[i,5] <-Inf
    stat_coord[i,6] <- Inf
    stat_coord[i,7] <- Inf
    stat_coord[i,8] <- 0
    stat_coord[i,9] <- 0
    stat_coord[i,10] <- Inf
  }
}

saveRDS(stat_coord, paste0(stationspath_int, "/AQ_OSM_stations.rds"))
