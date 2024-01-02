##################################################
## Project: AirOld Update
## Date: 09.05.2023
## Author: Mika Erdmann
##################################################

library(RCurl)
library(stringr)
library(data.table)
library(tidyverse)
library(sf)
library(terra)
library(openxlsx)
library(tidyr)
library(dplyr)
library(readr)
# Path management
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data"
shapepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external"
# Import data
#AQ_all_20_22 <- readRDS("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld Update/AirOldUpdate/AQ_all_20_22.rds")
# import stationsinfo
Bericht_EU_Meta_Stationen <- read_delim(paste0(stationpath, "/Bericht_EU_Meta_Stationen_withhistory.csv"), 
                                        delim = ";",col_select = c(station_code, station_name, station_latitude_d, station_longitude_d, type_of_station, station_type_of_area), escape_double = FALSE, trim_ws = TRUE, 
                                        skip = 1)




## merge stationinfo and shapefile data like in DWD.R -> stationinfo_lk.R
# Match coordinates to Landkreise

# !! important: first save folder "Shapefile" I sent you in "Data" and unzip it
# 5000nuts3 source: 
# Bundesamt für Kartographie und Geodäsie. (n.d.). NUTS- Gebiete 1:5 000 000, Stand 31.12. Retrieved 2021-10-19, 
# from https://gdz.bkg.bund.de/index.php/default/open-data/ nuts-gebiete-1-5-000-000-stand-31-12-nuts5000-31-12.html
# lk_id-NUTS.xlsx was made by hand

# read shapefile for German Landkreise (including polygon coordinates of all Landkreise)
polygonSF <- read_sf(dsn = paste0(shapepath, "/Shapefile/5000_NUTS3.shp"))
# create df that only includes station ID and (point) coordinates 
coord_stations <- unique(subset(Bericht_EU_Meta_Stationen, select=c("station_code","station_longitude_d","station_latitude_d")))
# transform into shapefile
pointSF <- st_as_sf(x = coord_stations,                         
                    coords = c("station_longitude_d","station_latitude_d"),
                    crs = "+proj=longlat +datum=WGS84")
# change crs of polygon sf to that of point sf
polygonSF <- st_transform(polygonSF, crs = st_crs(pointSF))
# intersect point sf with polygon sf (merge both sf by in which polygon a point lies)
stations_nuts3.sf <- st_intersection(pointSF, polygonSF)
# create df that only includes station ID and corresponding nuts3
stations_nuts3 <- data.frame(station_code=stations_nuts3.sf$station_code,NUTS_CODE=stations_nuts3.sf$NUTS_CODE)

# get Landkreis ID -- nuts3 keys and merge to stations
id_nuts <- read.xlsx(paste0(shapepath, "/Shapefile/lk_id-NUTS.xlsx"))
id_nuts <- id_nuts %>%
  mutate_at("nuts", str_replace, " ", "")
stations_nuts3 <- merge(stations_nuts3, id_nuts, by.x="NUTS_CODE", by.y="nuts")
# stations_nuts3 <- stations_nuts3 %>%
#   mutate(station_code = tolower(station_code)) %>%
#   add_row(station_code="demv031" ,NUTS_CODE="DE803", lk_id =13003, kreis = 
#             "Rostock, Hansestadt") %>%
#   add_row(station_code="demv027" ,NUTS_CODE="DE80N", lk_id =13075, kreis = 
#             "Vorpommern-Greifswald") %>%
#   add_row(station_code="desn052" ,NUTS_CODE="DED2F", lk_id =14628, kreis = 
#             "Sächsische Schweiz-Osterzgebirge") %>%
#   add_row(station_code="deth013" ,NUTS_CODE="DEG0P", lk_id =16063, kreis = 
#             "Wartburgkreis")

# merge to data
# AQ_lk <- merge(AQ_all_20_22,stations_nuts3,by.x="station",by.y="station_code")
# coord_stations <- coord_stations %>%
#   mutate(station_code = tolower(station_code))
# 
# AQ_lk <- merge(AQ_lk, coord_stations, by.x = "station", by.y = "station_code")
# AQ_lk <- mutate(AQ_lk, kreis = gsub(",","",kreis))
# saveRDS(AQ_lk, "AQ_lk_all_2020-2023.rds")
# write_csv(AQ_lk, "AQ_lk_all_2020-2023.csv")
# save stations-nuts3
saveRDS(stations_nuts3, paste0(stationpath, "/stations_nuts3.rds"))

# Test for missing stations in outcome
# 
# diff <- anti_join(AQ_all_20_22, AQ_lk)
# unique(diff$station)
# # - > missin g 4 stations "demv031" "desn052" "deth013" "demv027"
# #Testing for missing stations in data
# EU <-tolower(unique(Bericht_EU_Meta_Stationen$station_code))
# AQ <- tolower(unique(AQ_all_20_22$station))
# miss <- AQ[!(AQ %in% EU)]
# # -> not missing in data
# # -> must be missing in polygon sf -> added by hand above






