
library(tidyverse)
library(dplyr)
library(dbplyr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)


savepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim"
wdpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/WD_hourly"
aqpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/AQ_hourly"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/Stations"

subelements <- c("precipitation", "air_temperature", "dew_point", "wind", "extreme_wind", "pressure", "sun", "cloudiness", "moisture")

short_sub <- c("TU", "TD","FF", "FX", "TF", "RR", "P0", "SD", "N") # short identifier used in file zip names

# read in AQ 

# Merge weather data by matched station and date
#AQ <- readRDS(paste0(aqpath, "/AQ_all_Hourly_2000-2022.rds"))  # 
#
##mutate(across(where(is.Date), ~ format(.x, "%Y.%m.%d"))) %>%
#AQ <- AQ %>%
#  mutate(date = ymd(date)) %>%
#  mutate(Uhrzeit = hm(str_remove_all(Uhrzeit, "'")))
#AQ <- AQ %>%
#  mutate(datetime = ymd_hms(paste(date, Uhrzeit))) %>%
#  select(- c(Uhrzeit, date))
#
#saveRDS(AQ, paste0(aqpath, "/AQ_all_Hourly_2000-2022.rds"))

## Merge AQWD
## # Merge Weather data and Air Quality data first best
# first element
AQWD <- readRDS(paste0(aqpath, "/AQ_all_Hourly_2000-2022.rds")) %>%
  filter(datetime > ymd_hms("20100101000000"))
element <- subelements[1]
# Read in matched stations
closest_df <- readRDS(paste0(stationpath, "/AQWD_matchedstations_", element,".rds"))

# Join the wd_ids to air data
AQWD <- inner_join(AQWD, closest_df, by = c("station"="AQ_station_id"))

# read in element
dwd<-readRDS(paste0(wdpath, "/hourly_dwd_", element, "_2000-2023.rds")) %>%
  mutate(MESS_DATUM = as_datetime(MESS_DATUM))  %>%
  filter(MESS_DATUM > ymd_hms("20100101000000"))
dwd <- dwd %>% as_tibble()

idx_var <- 1
col_namestat <- str_glue("WD_station_id_{idx_var}")
# rename columns and merge dwd in loop
AQWD <- AQWD %>%
  rename(STATIONS_ID = {{col_namestat}})

AQWD <- left_join(AQWD, dwd, by = c("datetime" = "MESS_DATUM", "STATIONS_ID" = "STATIONS_ID"))
new_name <- paste0("STATIONS_ID","_", idx_var)
# rename final
AQWD <- AQWD %>%
  rename(!! new_name := STATIONS_ID)

# select only relevant columns of dwd
dwd <- dwd %>%
  select(-c(Geogr.Breite, Geogr.Laenge))
colnam <- colnames(dwd)
colnam <- colnam[colnam %in% "MESS_DATUM" == FALSE] 
colnam <- colnam[colnam %in% "STATIONS_ID" == FALSE] 

for (idx_var in 2:5) {
  col_namestat <- str_glue("WD_station_id_{idx_var}")
  AQWD <- AQWD %>%
    rename(STATIONS_ID = {{col_namestat}})
  # rename elements
  
  dwd_idx <- dwd %>%
    rename_with(.fn = ~paste0(., "_", idx_var),
                .cols = all_of(colnam))
  
  AQWD <- left_join(AQWD, dwd_idx, by = c("datetime" = "MESS_DATUM", "STATIONS_ID" = "STATIONS_ID"))
  new_name <- paste0("STATIONS_ID","_", idx_var)
  
  # rename final
  AQWD <- AQWD %>%
    rename(!! new_name := STATIONS_ID)
}
# find NA Values and use case_when to merge next closest stations


# for every subelement 
for (sub in 1:length(colnam)){
  col_1 <- paste0(colnam[sub])
  col_2 <- paste0(colnam[sub],"_2")
  col_3 <- paste0(colnam[sub],"_3")
  col_4 <- paste0(colnam[sub],"_4")
  col_5 <- paste0(colnam[sub],"_5")
  AQWD <- AQWD %>%
    mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_2, .default = STATIONS_ID_1))
  AQWD <- AQWD %>%
    mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_2]], .default = .data[[col_1]]))
  AQWD <- AQWD %>%
    mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_3, .default = STATIONS_ID))
  AQWD <- AQWD %>%
    mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_3]], .default = .data[[col_1]]))
  AQWD <- AQWD %>%
    mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_4, .default = STATIONS_ID))
  AQWD <- AQWD %>%
    mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_4]], .default = .data[[col_1]]))
  AQWD <- AQWD %>%
    mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_5, .default = STATIONS_ID))
  AQWD <- AQWD %>%
    mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~.data[[col_5]], .default = .data[[col_1]]))
}

# select only merged columns
AQWD <- AQWD %>%
  select(all_of(c(colnam, "STATIONS_ID", "datetime", "station", "NO2", "PM25", 
                  "PM10", "SO2", "NO", "SO2", "O3", "CO", "Geogr.Laenge", "Geogr.Breite")))

saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2000-2023_", element, ".rds"))
colnam_all <- colnam
# Loop
#
 
gc()

for (el in 2:9){
  print(paste0("round ", el))
  element <- subelements[el]
  AQ <- readRDS(paste0(aqpath, "/AQ_all_Hourly_2000-2022.rds")) %>%
    filter((datetime > ymd_hms("20100101000000")) & (datetime < ymd_hms("20120101000000")))
  # Read in matched stations
  closest_df <- readRDS(paste0(stationpath, "/AQWD_matchedstations_", element,".rds"))
  
  # Join the wd_ids to air data
  AQWD_el <- inner_join(AQ, closest_df, by = c("station"="AQ_station_id"))
  
  # read in element
  dwd<-readRDS(paste0(wdpath, "/hourly_dwd_", element, "_2000-2023.rds")) %>%
    mutate(MESS_DATUM = as_datetime(MESS_DATUM))  %>%
    filter((MESS_DATUM > ymd_hms("20100101000000")) & (MESS_DATUM < ymd_hms("20120101000000")))
  dwd <- dwd %>% as_tibble()
  
  idx_var <- 1
  col_namestat <- str_glue("WD_station_id_{idx_var}")
  # rename columns and merge dwd in loop
  AQWD_el <- AQWD_el %>%
    rename(STATIONS_ID = {{col_namestat}})
  
  AQWD_el <- left_join(AQWD_el, dwd, by = c("datetime" = "MESS_DATUM", "STATIONS_ID" = "STATIONS_ID"))
  new_name <- paste0("STATIONS_ID","_", idx_var)
  # rename final
  AQWD_el <- AQWD_el %>%
    rename(!! new_name := STATIONS_ID)
  
  # select only relevant columns of dwd
  #dwd <- dwd %>%
  #  select(-c(Geogr.Breite, Geogr.Laenge))
  colnam <- colnames(dwd)
  colnam <- colnam[colnam %in% "MESS_DATUM" == FALSE] 
  colnam <- colnam[colnam %in% "STATIONS_ID" == FALSE] 
  
  for (idx_var in 2:5) {
    col_namestat <- str_glue("WD_station_id_{idx_var}")
    AQWD_el <- AQWD_el %>%
      rename(STATIONS_ID = {{col_namestat}})
    # rename elements
    
    dwd_idx <- dwd %>%
      rename_with(.fn = ~paste0(., "_", idx_var),
                  .cols = all_of(colnam))
    
    AQWD_el <- left_join(AQWD_el, dwd_idx, by = c("datetime" = "MESS_DATUM", "STATIONS_ID" = "STATIONS_ID"))
    new_name <- paste0("STATIONS_ID","_", idx_var)
    
    # rename final
    AQWD_el <- AQWD_el %>%
      rename(!! new_name := STATIONS_ID)
  }
  # find NA Values and use case_when to merge next closest stations
  
  
  # for every subelement 
  for (sub in 1:length(colnam)){
    col_1 <- paste0(colnam[sub])
    col_2 <- paste0(colnam[sub],"_2")
    col_3 <- paste0(colnam[sub],"_3")
    col_4 <- paste0(colnam[sub],"_4")
    col_5 <- paste0(colnam[sub],"_5")
    AQWD_el <- AQWD_el %>%
      mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_2, .default = STATIONS_ID_1))
    AQWD_el <- AQWD_el %>%
      mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_2]], .default = .data[[col_1]]))
    AQWD_el <- AQWD_el %>%
      mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_3, .default = STATIONS_ID))
    AQWD_el <- AQWD_el %>%
      mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_3]], .default = .data[[col_1]]))
    AQWD_el <- AQWD_el %>%
      mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_4, .default = STATIONS_ID))
    AQWD_el <- AQWD_el %>%
      mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~ .data[[col_4]], .default = .data[[col_1]]))
    AQWD_el <- AQWD_el %>%
      mutate(STATIONS_ID = case_when(is.na(.data[[col_1]]) ~ STATIONS_ID_5, .default = STATIONS_ID))
    AQWD_el <- AQWD_el %>%
      mutate({{col_1}} := case_when(is.na(.data[[col_1]]) ~.data[[col_5]], .default = .data[[col_1]]))
  }
  #colnam_all <- c(colnam_all, colnam)
  # select only merged columns
  AQWD_el <- AQWD_el %>%
    select(all_of(c(colnam, "STATIONS_ID", "datetime", "station")))
  saveRDS(AQWD_el, paste0(savepath, "/AQWD_merge_2010-2023_10", element, ".rds"))
  
}


#### Last step merge ############
# Merge all elements
element <- subelements[1]
AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2000-2023_", element, ".rds"))

for (el in 2:8){
  gc()
  element <- subelements[el]
  AQWD_el <- readRDS(paste0(savepath, "/AQWD_merge_2000-2023_", element, ".rds"))
  
  AQWD <- left_join(AQWD, AQWD_el, by = c("datetime", "station"))

}
saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2010-2023_update.rds"))

#View(AQWD %>% slice_sample(n = 1000))
el <- 9
#AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_update.rds"))
AQWD <- AQWD %>% 
  select(-starts_with("STATIONS_I"))
try(AQWD <- AQWD %>% select(-c(V_N_I, TT, RS_IND, Geogr.Breite, Geogr.Laenge)))

y <- 10

element <- subelements[el]
AQWD_el <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_",y, element, ".rds"))
try(AQWD_el <- AQWD_el %>%
      select(-starts_with("STATIONS_I")))
try(AQWD_el <- AQWD_el %>% select(-c( TT_STD, RF_STD, TD_STD, P_STD )))
for (y in c(12, 15)){
  gc()
  element <- subelements[el]
  AQWD_el2 <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_",y, element, ".rds"))
  try(AQWD_el2 <- AQWD_el2 %>%
    select(-starts_with("STATIONS_I")))
  try(AQWD_el2 <- AQWD_el2 %>% select(-c( TT_STD, RF_STD, TD_STD, P_STD )))
  #AQWD <- AQWD %>%
   # select(-c(TT_STD, RF_STD, TD_STD, P_STD, RS_IND, TT, V_N_I, RS_IND))
  
  AQWD_el <- bind_rows(AQWD_el, AQWD_el2)
  
}


gc()
saveRDS(AQWD_el, paste0(savepath, "/AQWD_merge_2010-2023_moisture.rds"))


# try to merge moisture to rest
AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_update.rds")) %>%
  select(-WRTR)
AQWD_moist <-readRDS(paste0(savepath, "/AQWD_merge_2010-2023_moisture.rds"))
# check both
View(AQWD %>% slice_sample(n= 1000))
View(AQWD_moist %>% slice_sample(n= 1000))

AQWD <- AQWD %>% ungroup() %>%
  distinct()
AQWD_moist <- AQWD_moist %>% ungroup() %>%
  distinct()
gc()
AQWD <- left_join(AQWD, AQWD_moist, by = c("datetime", "station"))
saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2010-2023_update_f.rds"))

# AQWD_el <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_moisture.rds")) 
# AQWD_el <- AQWD_el %>%
#   filter(datetime > ymd_hms("20150101000000"))
# AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_update.rds"))
# AQWD <- AQWD %>%
#   filter(datetime > ymd_hms("20150101000000"))
# AQWD <- left_join(AQWD, AQWD_el, by = c("datetime", "station"))
# saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2015-2023_update.rds"))
# 
# # 10-12
# AQWD_el <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_moisture.rds")) 
# AQWD_el <- AQWD_el %>%
#   filter(datetime < ymd_hms("20120101000000"))
# AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_update.rds"))
# AQWD <- AQWD %>%
#   filter(datetime < ymd_hms("20120101000000"))
# gc()
# AQWD <- left_join(AQWD, AQWD_el, by = c("datetime", "station"))
# saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2010-2012_update.rds"))
# 
# # 12-15
# gc()
# AQWD_el <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_moisture.rds")) 
# 
# AQWD_el <- AQWD_el %>%
#   filter((datetime > ymd_hms("20120101000000")) & (datetime < ymd_hms("20150101000000")))
# AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2010-2023_update.rds"))
# AQWD <- AQWD %>%
#   filter((datetime > ymd_hms("20120101000000")) & (datetime < ymd_hms("20150101000000")))
# AQWD <- left_join(AQWD, AQWD_el, by = c("datetime", "station"))
# remove(AQWD_el)
# gc()
# saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2012-2015_update.rds"))
# 
# remove(AQWD_el)
# remove(AQWD_dates)
# remove(AQWD)
# gc()
# AQWD_1 <- readRDS(paste0(savepath, "/AQWD_merge_2010-2015_update.rds"))
# AQWD <- readRDS(paste0(savepath, "/AQWD_merge_2015-2023_update.rds"))
# AQWD <- bind_rows(AQWD_1, AQWD)
# 
saveRDS(AQWD, paste0(savepath, "/AQWD_merge_2010-2023_update_f.rds"))
