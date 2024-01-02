
##################################################
## Project:AirOld Update
## Date: 15.04.2023
## Author: Mika Erdmann

## This script joins together the UBA data by measurment for all years from 2000-2019 and all AQ elements (Hourly Data)
##################################################

##### Data reading ###############
# Import all data on Element

library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(stringr)


savepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/AQ_hourly"
getfrom <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/raw/Hourly_UBA_Data_2000-2022"

Elements <- c("CO", "NO", "NO2", "O3", "PM1", "PM2", "SO2")

create_long <- function(year){
  if (year < 2020){
  Element <- as_tibble(read_delim(paste0(getfrom, "/DE",year, Elements[element],"_inv1SMW_20230508.csv"), 
                                  delim = ";", escape_double = FALSE, col_names = TRUE, 
                                  locale = locale(date_format = "%y%m%d"), trim_ws = TRUE))
  }
  else{
  Element <- as_tibble(read_delim(paste0(getfrom, "/DE",year, Elements[element],"_inv1SMW_20230403.csv"), 
                                  delim = ";", escape_double = FALSE, col_names = TRUE, 
                                  locale = locale(date_format = "%y%m%d"), trim_ws = TRUE))
  }
  Stat_inf <- Element %>%
    slice(1:3) %>%
    pivot_longer(cols = starts_with("DE"), names_to = "AQ_stationId") %>%
    select(-Komponente) %>%
    select(-Datum) %>%
    pivot_wider(names_from = Uhrzeit, values_from = value )
  
  # transform and select only one station type and clean column names
  
  Element_long <- Element %>%
    slice(4:nrow(Element)) %>%
    pivot_longer(cols = starts_with("DE"), names_to = "AQ_stationId" )
  Element_long <- left_join(Element_long, Stat_inf, by = "AQ_stationId")
  return(Element_long)
}

years <- 2000:2022
list <- 1:7
element <- 1
#for (element in 1:length(Elements)){
for (element in list){
  Element_all <- create_long(years[1])
  for (y in 2:length(years)){
    year <- years[y]
    try(Element_long <- create_long(year))
    try(Element_all <- bind_rows(Element_all, Element_long))
  }
  
  # remove all "" 
  Element_all <- Element_all %>%
    mutate(TypeOfArea = gsub("'","",Element_all$TypeOfArea)) %>%
    mutate(TypeOfData = gsub("'","",Element_all$TypeOfData))%>%
    mutate(TypeOfStation = gsub("'","",Element_all$TypeOfStation)) %>%
    mutate(Datum = gsub("'","",Element_all$Datum)) %>%
    mutate(Komponente = gsub("'","",Element_all$Komponente)) %>%
    mutate(date = ymd(Datum)) %>% 
    mutate_at(vars(date), funs(year, month, day)) %>%
    select(-Datum)
  
  
  # replace all -999 with NaN 
  Element_all <- Element_all %>%
    mutate(value = replace(value, value == -999, NA)) %>%
    mutate(value = replace(value, value == -444, NA)) 

  saveRDS(Element_all, paste0(savepath, "/",Elements[element], "_Hourly_2000_2022.rds"))
  
}

# Unitl here ran at 15:08
# Merge together all Hourly measures

## Merge all measurments together

Elements = c("CO", "NO", "NO2", "O3", "PM1", "PM2", "SO2")


for (element in 1:length(Elements)){
  # import new values
  Element_20_22 <- readRDS(paste0(savepath,"/" ,Elements[element],"_Hourly_2000_2022.rds"))
  # rename all columns
  Element_20_22 <- Element_20_22 %>%
    rename(station = AQ_stationId) %>%
    rename("{Elements[element]}" := value) %>%
    mutate(station = str_to_lower(str_sub(station, 1, 7))) %>%
    arrange(desc(TypeOfData)) %>%
    arrange(date) %>%
    select(-Komponente) %>%
    select(-TypeOfStation) %>%
    select(-TypeOfArea) %>%
    select(-TypeOfData) %>%
    select(-year) %>%
    select(-month) %>%
    select(-day)
  saveRDS(Element_20_22, paste0(savepath,"/", Elements[element], "_Hourly_prepmerge_2000_2022.rds"))
}

## Merge together all measures for 20-22

Element1 <- Elements[1]
prep_20_22 <- readRDS(paste0(savepath,"/", Element1, "_Hourly_prepmerge_2000_2022.rds"))

for (element in 2:length(Elements)){
  new_prep <- readRDS(paste0(savepath,"/", Elements[element], "_Hourly_prepmerge_2000_2022.rds"))
  prep_20_22 <- full_join(prep_20_22, new_prep, by = c("station", "date", "Uhrzeit"))
}

AQ_all_20_22 <- prep_20_22 %>%
  relocate(date, .after = station) %>%
  rename(PM10 = PM1) %>%
  rename(PM25 = PM2)%>%
  relocate(O3, PM10,PM25, NO2, SO2, .after = CO)
saveRDS(AQ_all_20_22, paste0(savepath, "/AQ_all_Hourly_2000-2022.rds"))
