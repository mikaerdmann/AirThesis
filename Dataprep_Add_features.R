# This script adds all features from external datasourxes to the merged aqwd data
# This includes socio-economic data from "regionalatlas", a covid index and 
# features downloaded from OpenStreetMaps in a different script (OSM_GErmany.R)

library(tidyverse)
library(dplyr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(stargazer)
library(fixest)
library(zoo)
library(readr)
library(imputeTS)
library(naniar)
library(sf)
library(terra)

processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data"
figurepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Figures"
Components <- c("PM10", "PM25", "CO", "NO", "NO2", "SO2", "O3")
externalpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external"
stationspath_int <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/Stations"
gc()
# import dataset
aqwd <- readRDS(paste0(processedpath, "/AQWD_merge_2010-2023_p.rds"))
aqwd <- aqwd %>%
  mutate(datetime = as.character(datetime)) %>%
  mutate(datetime = ymd_hms(datetime))


#aqwd <- aqwd %>% 
 # select(c(all_of(component), all_of(positions)))

# Include crisis dummy
aqwd <- aqwd %>%
  mutate(crisis = case_when(datetime > ymd(20220223) ~ 1,  
                            datetime < ymd(20220223) ~ 0 
  ))

# HDH function
T_base <- 15.5
hdh <- function(Temp){
  Baseline <- T_base
  Values <- list()
  for (i in 1:length(Temp)) {
    t <- Temp[i]
    if (is.na(t)){
      value <- NA
      Values[i] <- value
    }
    if (!is.na(t)) {
      Cond <- Baseline - t
      if (Cond > 0) {
        value <- Baseline - t
      }
      else {
        value <- 0
      }
      Values[i] <- value
    }

  }
  return(Values)
}
# Decompose time variables and construct HDH
aqwd <- aqwd %>% ungroup() 
aqwd <- aqwd %>%
  mutate(HoD = hour(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(DoW = wday(datetime)) %>%
  mutate(Year = year(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate(Quarter = quarter(datetime)) %>%
  mutate(HDH = hdh(TT_TU)) %>%
  mutate(month_Year = floor_date(datetime, unit = "month")) %>%
  arrange(month_Year) %>%
  group_by(month_Year) %>%
  mutate(Trend_month = cur_group_id()) %>%
  ungroup()

# Add type of station info
# import stationsinfo
AQ_Stations <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data/Bericht_EU_Meta_Stationen_withhistory.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                          trim_ws = TRUE, skip = 1)
AQ_Stations <- AQ_Stations %>%
  select(c(station_code , type_of_station, station_type_of_area)) %>%
  mutate(station_code = tolower(station_code)) %>%
  distinct()

aqwd <- left_join(aqwd, AQ_Stations, by = c("station" = "station_code"))

# construct background dummy
aqwd <- aqwd %>%
  mutate(background = case_when(type_of_station == "background" ~ 1, .default = 0))
#filter for area type
aqwd <- aqwd %>%
  mutate(suburban = case_when(station_type_of_area== "suburban" ~ 1, station_type_of_area == "rural" ~ 2, .default = 0))

# Insert LK id
# import AQ stations and lk id table
lk_stations <- readRDS(paste0(stationpath, "/stations_nuts3.rds")) %>%
  mutate(station_code = tolower(station_code)) %>%
  distinct()

aqwd <- left_join(aqwd, lk_stations, by = c("station" = "station_code"))
aqwd <- aqwd %>% distinct()

# construct bundesland id

aqwd <- aqwd %>% mutate(bundesland = substring(station, 1,4))

# Add geographic data
stations <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data/Bericht_EU_Meta_Stationen_withhistory.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE, skip = 1)
stations <- stations %>% mutate(station_code = tolower(station_code))
# missing_geo <- aqwd %>% filter(is.na(Geogr.Breite))
# missing_geo <- missing_geo %>% select(station) %>% distinct()
# missing_geo <- left_join(missing_geo, stations, by = c("station" ="station_code"))
# 
stations <- stations %>% select(c(station_code, station_longitude_d, station_latitude_d)) %>% distinct(station_code, .keep_all = TRUE)
aqwd <- left_join(aqwd, stations, by = c("station" = "station_code"))
# 
#   mutate(Geogr.Breite = ifelse(station %in% missing_geo$station, 
#                 Geogr.Breite, 
#                 missing_geo$Geogr.Breite[station_code == station])) %>%
#   mutate(Geogr.Laenge = ifelse(station %in% missing_geo$station, 
#                 Geogr.Laenge, 
#                 missing_geo$Geogr.Laenge[station_code == station]))
aqwd <- aqwd %>%
  rename(Geogr.Breite = station_latitude_d) %>%
  rename(Geogr.Laenge = station_longitude_d)
missing_geo_2 <- aqwd %>% filter(is.na(Geogr.Breite)) %>% select(station) %>% distinct()


saveRDS(aqwd, paste0(processedpath, str_glue("/AQWD_hourly_modelprep_2010-2023_f.rds")))


# Merge Confounding Data --------------------------------------------------
gc()
aqwd <- readRDS(paste0(processedpath,str_glue("/AQWD_hourly_modelprep_2010-2023_f.rds") ))

# Join forest data
Codes <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Codes_GemeindeRegionalatlas.csv", 
                         delim = ";", escape_double = FALSE, col_names = FALSE, 
                         locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE, skip = 6)
Codes <- Codes %>% 
  select(-c(X1, X4)) %>%
  rename(code = X2, name = X3)
# Join Gemeinde_ids to stations
coord_stations <- unique(subset(aqwd, select=c("station","Geogr.Breite","Geogr.Laenge")))
data_sf <- read_sf(dsn = paste0(externalpath, "/Shapefiles_Regionalatlas/Shapefile_Alter/Alter.shp"))
pointSF <- st_as_sf(x = coord_stations,                         
                    coords = c("Geogr.Laenge","Geogr.Breite"),
                    crs = "+proj=longlat +datum=WGS84")
# change crs of polygon sf to that of point sf
data_sf <- st_transform(data_sf, crs = st_crs(pointSF))
# intersect point sf with polygon sf (merge both sf by in which polygon a point lies)
stations_gid.sf <- st_intersection(pointSF, data_sf)
# create df that only includes station ID and corresponding nuts3
stations_gid <- data.frame(station=stations_gid.sf$station,Gemeinde_id=stations_gid.sf$schluessel)


forest <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/AnteilWald.csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                                      skip = 9)
# Impute Missing Values
forest <- forest %>% mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert))
#forest <- forest %>% mutate(wert = case_when(is.na(wert) ~ mean(wert, na.rm = TRUE), .default = wert))
# Linear imputation (mean between neighbouring regions)
forest <- forest %>% mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3))
# clean codenames
forest <- forest %>%
  rename(code = schluessel, name = regionaleinheit, value = wert) %>%
  mutate(code = str_replace(code, "000$", ""))
forest_stations <- left_join(stations_gid, forest, by = c("Gemeinde_id" = "code"))
# Impute value for Berlin with value for Bremen
forest_stations <- forest_stations %>%
  mutate(name = case_when(is.na(value) ~ "Berlin", .default = name)) %>%
  mutate(value = case_when(is.na(value) ~ 0.7 , .default = value)) %>%
  rename(Forest = value) %>%
  rename(Gemeinde = name) 

aqwd <- left_join(aqwd, forest_stations, by = c("station"))

# Join other social components
Area_resi_traffic <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/AnteilSiedlung_verkehrsflaeche.csv", 
                                delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE, skip = 9) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, area_resiandtraffic = wert) %>%
  mutate(code = str_replace(code, "000000$", "")) %>%
  mutate(code = str_replace(code, "000$", ""))
  # 
area_resi <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/AnteilSiedlungsflaeche.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                        trim_ws = TRUE, skip = 9)  %>% 
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, area_resi = wert) %>%
  mutate(code = str_replace(code, "000000$", "")) %>%
  mutate(code = str_replace(code, "000$", ""))

GDPpercap_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/BIPjeEWLK.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                        trim_ws = TRUE, skip = 2) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, GDPpCap = wert)

Avg_age <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/Durchschnittsalter.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                         skip = 9) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>% 
  rename(code = schluessel, avg_age = wert) %>%
  mutate(code = str_replace(code, "000000$", "")) %>%
  mutate(code = str_replace(code, "000$", ""))
  
Earnings <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/Einkünfte.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                       skip = 9) %>% 
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, earnings = wert) %>%
  mutate(code = str_replace(code, "000000$", "")) %>%
  mutate(code = str_replace(code, "000$", ""))

householdsize_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/HaushaltsgrößeLK.csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                                   skip = 2)  %>% 
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, hhsize = wert)

cars_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/PKW_bestandLK.csv", 
                               delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE, skip = 2) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, cars = wert)

disp_income_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/VerfuegbaresEinkommenLK.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE, skip = 2) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, disp_income = wert)

flatsperhouse_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/WohnungenjeWohngebaeudeLK.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                             trim_ws = TRUE, skip = 2)  %>% 
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, flatsperhouse = wert)

areaperflat_lk <- read_delim("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Regionalatlas/WohnflaechejeWohnungLK.csv", 
                               delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE, skip = 2) %>%
  mutate(wert = case_when(wert == 2222222222.0 ~ NA, .default = wert)) %>% 
  mutate(wert = na.approx(wert, na.rm = FALSE, rule = 2, maxgap = 3)) %>%
  select(-regionaleinheit) %>%
  rename(code = schluessel, areaperflat_lk = wert)

list_confoundings_gid <- list(area_resi, Area_resi_traffic, Earnings, Avg_age)
list_confoundings_lk <- list(GDPpercap_lk, areaperflat_lk, flatsperhouse_lk, 
                          disp_income_lk, cars_lk, householdsize_lk)
confoundings <- left_join(area_resi, Area_resi_traffic) %>%
  left_join(Earnings) %>% left_join(GDPpercap_lk) %>% left_join(areaperflat_lk) %>%
  left_join(flatsperhouse_lk) %>% left_join(disp_income_lk) %>% left_join(cars_lk) %>%
  left_join(householdsize_lk) %>% left_join(Avg_age)
# Prepare data for merge -> Adapt LK_id from stralsund and rügen station
# They were assigned wrong lk_ids that are only present in the nuts_code dataset
# but not in the Regionalatlas

aqwd <- aqwd %>% 
  mutate(Gemeinde_id = as.integer(Gemeinde_id)) %>%
  mutate(lk_id = as.integer(lk_id)) %>%
  mutate(lk_id = case_when(station == "demv025" ~13005, .default = lk_id)) %>%
  mutate(lk_id = case_when(station == "deub028" ~ 13061, .default = lk_id))
gc()

for (i in 1:length(list_confoundings_gid)){
  df<-list_confoundings_gid[[i]]
  df <- df %>% mutate(code = as.integer(code))
  aqwd <-left_join(aqwd, df, by = c("Gemeinde_id" = "code"))
}
for (i in 1:length(list_confoundings_lk)){
  df<-list_confoundings_lk[[i]]
  df <- df %>% mutate(code = as.integer(code))
  aqwd <- left_join(aqwd, df, by = c("lk_id" = "code"))
}
# Impute the missing landkreise values in Mecklenbur vorpommern by hand
aqwd <- aqwd %>% 
  mutate(disp_income = case_when(station == "deub028" ~ 21062, .default = disp_income)) %>%
  mutate(cars = case_when(station == "deub028" ~ 557.7, .default = cars)) %>%
  mutate(GDPpCap = case_when(station == "deub028" ~ 25969, .default = GDPpCap)) %>%
  mutate(GDPpCap = case_when(station == "deub025" ~ 25969, .default = GDPpCap)) %>%
  mutate(cars = case_when(station == "deub025" ~ 557.7, .default = cars)) %>%
  mutate(disp_income = case_when(station == "deub025" ~ 21062, .default = disp_income))
  
covid <- read_csv("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Corona_Maßnahmen_index/kr_massnahmen_index_tag.csv")
covid <- covid %>%
  select(c(ags5, datum, kr_mn_idx_t)) %>%
  rename(code = ags5) %>%
  mutate(date = as.Date(datum)) %>%
  select(-datum) %>%
  rename(covid = kr_mn_idx_t) %>%
  mutate(code = as.integer(code))
aqwd <- aqwd %>%
  mutate(date = date(datetime)) %>%
  mutate(lk_id = case_when (station == "demv025" ~ 13073, .default = lk_id)) %>%
  mutate(lk_id = case_when(station == "deub028" ~13073, .default = lk_id))

aqwd <- left_join(aqwd, covid, by = c("lk_id" = "code", "date" = "date"))
aqwd <- aqwd %>% 
  mutate(covid = replace_na(covid, 0))

## Add stationsurrounding data from OSM (Script: OSM_Germany.R)

stations_osm <- readRDS(paste0(stationspath_int, "/AQ_OSM_stations.rds"))
stations_osm <- stations_osm %>%
  select(-c(station_longitude_d, station_latitude_d))
aqwd <- left_join(aqwd, stations_osm, by = "station")



saveRDS(aqwd, paste0(processedpath, str_glue("/AQWD_hourly_modelprep_f_2010-2023_conf_cov.rds")))

