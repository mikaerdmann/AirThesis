# Code for downloading and merging historical and recent weather data from DWD + identifying Germand county for each station
# Leonie Etzold, 13.03.2023
# Adapted for Airold Update by Mika Erdman 08.06.23

# This script is downloading all hourly data for 15 elements and saves them in 
# a database per element, where the data is filtered and precleaned. Then it
# merges all element tables together in one dataframe

library(RCurl)
library(stringr)
library(sawfish)
library(data.table)
library(tidyverse)
library(sf)
library(terra)
library(openxlsx)
library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)
library(sjmisc)

# path management ---------------------------------------------------------

savepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/WD_hourly"
getfrom <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/WD_hourly"
#savepath <- "C:/Users/mikae/Documents/SHK Huse/Master thesis/[old] German Air data"
#getfrom <- "C:/Users/mikae/Documents/SHK Huse/Master thesis/[old] German Air data"

# Download into subolders -------------------------------------------------
# !! important: create main folder in directory and "zip" within this
# Extract middle part of file names from website
subelements <- c("air_temperature", "dew_point", "wind", "extreme_wind", "moisture", "precipitation", "pressure", "sun", "cloudiness")
short_sub <- c("TU", "TD","FF", "FX", "TF", "RR", "P0", "SD", "N") # short identifier used in file zip names

# create folders for subelements
for (direlement in subelements){
  mainDir <- paste0(getfrom, "/zip/")
  subDir <- direlement
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
  mainDir <- paste0(getfrom, "/unzip/")
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
}
## Historical data
# Loop over all subelements to download
# 
for (el in 1:length(subelements)){
  element <- subelements[el]
  ## For every subelent, all values will be downloaded
  url <-  paste0("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/", element, "/historical/")
  
  
  middle <- str_extract_all(getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE), paste0("(?<=stundenwerte_",short_sub[el], "_).{23}(?=_hist.zip)")) 
  middle <- c(middle)[[1]]
  middle <- unique(middle)
  # create file names with loop
  zip_names <- c()
  for (i in middle) {
    zip_names <- append(zip_names,paste0("stundenwerte_", short_sub[el],"_",i,"_hist.zip")) 
  }
  
  # Download all zip files in folder
  for (i in zip_names) { 
    try({download.files(paste0(url,i),destfile = paste0(savepath, "/zip/",element,"/",i))
    },
    silent=FALSE)
  }
  
  ## Recent data 
  url_r <-  paste0("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/", element, "/recent/")
  
  middle_r <- str_extract_all(getURL(url_r, ftp.use.epsv = FALSE, dirlistonly = TRUE),paste0("(?<=stundenwerte_",short_sub[el],"_).{5}(?=_akt.zip)")) 
  middle_r <- c(middle_r)[[1]]
  middle_r <- unique(middle_r)
  
  zip_names_r <- c()
  for (i in middle_r) {
    zip_names_r <- append(zip_names_r,paste0("stundenwerte_", short_sub[el], "_",i,"_akt.zip")) 
  }
  
  for (i in zip_names_r) {
    try({download.files(paste0(url_r,i),destfile = paste0(savepath, "/zip/", element, "/",i))
    },
    silent=TRUE)
  }
}


# Unzip and merge element data --------------------------------------------

# Loop over all elements in subfolders and create one large df for each element

dwd_el <- data.frame(matrix(nrow=0,ncol=25)) # do i need to adapt ncol?

el <- 2 # try here once after downloading el = 1:length(subelements)
# test with the first zip folder
# !! first need to create "unzip" folder in "Data"
test_func <- function(el) {
  i <- dir(paste0(getfrom,"/zip/", subelements[el], "/"))[1]
  unzip(paste0(getfrom,"/zip/", subelements[el],"/",i),
        files=c(grep("^produkt", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE),
                grep("^Metadaten_Geographie", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),
        exdir=paste0(getfrom,paste0("/unzip/", subelements[el])))
  temp1 <- fread(paste0(savepath, "/unzip/",subelements[el],"/",grep("^Metadaten_Geographie", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),na.strings=c("-999"))
  temp1 <- temp1[dim(temp1)[1]]
  temp2 <- fread(paste0(savepath,"/unzip/",subelements[el], "/",grep("^produkt", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),na.strings=c("-999")) %>%
    #mutate(MESS_DATUM = IDateTime(gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$', '\\1-\\2-\\3', MESS_DATUM))) %>% # covert in posit + itime
    mutate(MESS_DATUM = ymd_h(MESS_DATUM)) %>%
    filter(MESS_DATUM > (ymd_h("2000010100"))) # use only observations from 2000 on
  station <- unique(temp2$STATIONS_ID)
  temp2 <- subset(temp2, MESS_DATUM > max(subset(dwd_el, STATIONS_ID==station)$MESS_DATUM, na.rm = TRUE))
  temp <- merge(temp2,temp1,by.x="STATIONS_ID",by.y="Stations_id")
  
  # create empty data frame dwd_el  and use test "temp" for column names
  
  dwd_el <- data.frame(matrix(nrow=0,ncol=ncol(temp)))
  names(dwd_el) <- names(temp)
  return(dwd_el)
  print("hello")
}

dwd_el <- test_func(el)
# This needs to be done within looping over all elements. This is why the above
# section is created to be a function that returns the test dataset. 
# 
# el 5: moisture: 1600 files, 10GB, 20GB on disk.
# air temperature: 1700 files, 6.6, 13GB on Disk
# for el 5: divide into 2015-2023 and do it 


#for (el in 9:length(subelements)){  # loop through all subelements
for (el in 8:8){  # loop through all subelements
  dwd_el <- test_func(el)
  for (i in dir(paste0(getfrom,"/zip/", subelements[el], "/"))) {  # loop thorough subelement folder
    try({unzip(paste0(getfrom,"/zip/", subelements[el],"/",i),
               files=c(grep("^produkt", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE),
                       grep("^Metadaten_Geographie", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),
               exdir=paste0(getfrom,paste0("/unzip/", subelements[el])))
      
      temp1 <- fread(paste0(savepath, "/unzip/",subelements[el],"/",grep("^Metadaten_Geographie", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),na.strings=c("-999"))
      temp1 <- temp1[dim(temp1)[1]]
      temp2 <- fread(paste0(savepath,"/unzip/",subelements[el], "/",grep("^produkt", unzip(paste0(getfrom,"/zip/",subelements[el], "/",i),list=TRUE)$Name,value=TRUE)),na.strings=c("-999")) %>%
        #mutate(MESS_DATUM = IDateTime(gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$', '\\1-\\2-\\3', MESS_DATUM))) %>% # covert in posit + itime
        mutate(MESS_DATUM = ymd_h(MESS_DATUM)) %>%
        filter(MESS_DATUM > (ymd_h("2000010100")) ) %>%# use only observations from 2000 on
        filter(MESS_DATUM < (ymd_h("2010010100")) )
      station <- unique(temp2$STATIONS_ID)
      temp2 <- subset(temp2, MESS_DATUM > max(subset(dwd_el, STATIONS_ID==station)$MESS_DATUM, na.rm = TRUE))
      temp <- merge(temp2,temp1,by.x="STATIONS_ID",by.y="Stations_id")
      dwd_el <- rbind(dwd_el,temp)})
  }
  # This warning message is to be expected: 
  # In max(subset(dwd_el, STATIONS_ID == station)$MESS_DATUM,  ... :
  # no non-missing arguments to max; returning -Inf
  
  # delete commas in station names to allow to save as csv 
  #dwd_el <- mutate(dwd_el, Stationsname = gsub(",","",Stationsname))
  try(dwd_el <- dwd_el %>% select(- eor))
  try(dwd_el <- dwd_el %>% select(- Stationshoehe))
  try(dwd_el <- dwd_el %>% select(- von_Datum))
  try(dwd_el <- dwd_el %>% select(- bis_Datum))
  try(dwd_el <- dwd_el %>% select(- Stationsname))
  
  
  
  
  #write_csv(dwd_el,paste0(savepath, "/hourly_dwd_",subelements[el],"_2000-2023.csv"))
  try(saveRDS(dwd_el, paste0(savepath, "/hourly_dwd_",subelements[el],"_2000-2010.rds") ))
  # For all elements, create database and filter for date
  print(paste0("The saving for",subelements[el]," has been tried"))
  
}

#for merging moisture:
for (el in 5:5){  # loop through all subelements
  dwd_el <- test_func(el)
# for (i in dir(paste0(getfrom,"/unzip/", subelements[el], "/"))) {  # loop thorough subelement folder
  for (i in dir(paste0(getfrom,"/unzip/", subelements[el], "/"))){
      if (str_contains(i, c("produkt_tf_stunde_2018", "produkt_tf_stunde_2019", "produkt_tf_stunde_202"), logic = "or") == TRUE){
      print(i)
      temp2 <- fread(paste0(savepath,"/unzip/",subelements[el], "/", i))
      temp2 <- temp2 %>%
        #mutate(MESS_DATUM = IDateTime(gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$', '\\1-\\2-\\3', MESS_DATUM))) %>% # covert in posit + itime
        mutate(MESS_DATUM = ymd_h(MESS_DATUM)) %>%
        filter(MESS_DATUM > (ymd_h("2017010100")) ) #%>%# use only observations from 2000 on
      station <- unique(temp2$STATIONS_ID)
      temp2 <- subset(temp2, MESS_DATUM > max(subset(dwd_el, STATIONS_ID==station)$MESS_DATUM, na.rm = TRUE))
      dwd_el <- rbind(dwd_el,temp2)
      
      }
  }
  # This warning message is to be expected:
  # In max(subset(dwd_el, STATIONS_ID == station)$MESS_DATUM,  ... :
  # no non-missing arguments to max; returning -Inf

  # delete commas in station names to allow to save as csv
  #dwd_el <- mutate(dwd_el, Stationsname = gsub(",","",Stationsname))
  try(dwd_el <- dwd_el %>% select(- eor))
  try(dwd_el <- dwd_el %>% select(- Stationshoehe))
  try(dwd_el <- dwd_el %>% select(- von_datum))
  try(dwd_el <- dwd_el %>% select(- bis_datum))
  try(dwd_el <- dwd_el %>% select(- Stationsname))




  #write_csv(dwd_el,paste0(savepath, "/hourly_dwd_",subelements[el],"_2000-2023.csv"))
  try(saveRDS(dwd_el, paste0(savepath, "/hourly_dwd_",subelements[el],"_2018-2023.rds") ))
  # For all elements, create database and filter for date
  print(paste0("The saving for",subelements[el]," has been tried"))

}
#   
# Clean element tables -------------------------------------------
# precipitation
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_precipitation_2000-2010.rds" ))
prec2 <- readRDS(paste0(savepath, "/hourly_dwd_precipitation_2010-2023.rds" ))
prec1 <- rbind(prec1, prec2)
prec1 <- prec1 %>%
  select(-bis_datum)%>%
  select(-von_datum)
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_precipitation_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_8)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_precipitation_2000-2023.rds"))

# moisture  # done
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_moisture_2000-2010.rds" ))
prec2 <- readRDS(paste0(savepath, "/hourly_dwd_moisture_2010-2015.rds" )) %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge)
#prec1 <- dplyr::full_join(prec1, prec2, by = c("MESS_DATUM", "STATIONS_ID"))
prec1 <- rbind(prec1, prec2)
prec2 <- readRDS(paste0(savepath, "/hourly_dwd_moisture_2015-2023.rds" )) %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge)
prec1 <- rbind(prec1, prec2)
  
saveRDS(prec1, paste0(savepath, "/hourly_dwd_moisture_2000-2023.rds"))
# delete Quality
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_moisture_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_8)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_moisture_2000-2023.rds"))


# pressure # 
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_pressure_2000-2010.rds" ))
prec2 <- readRDS(paste0(savepath, "/hourly_dwd_pressure_2010-2023.rds" ))
prec1 <- rbind(prec1, prec2)
prec1 <- prec1 %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge)

  
saveRDS(prec1, paste0(savepath, "/hourly_dwd_pressure_2000-2023.rds"))

prec1 <- readRDS(paste0(savepath, "/hourly_dwd_pressure_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_8)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_pressure_2000-2023.rds"))

# sun # merging prec1
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_sun_2000-2010.rds" ))
prec2 <- readRDS(paste0(savepath, "/hourly_dwd_sun_2010-2023.rds" ))
prec1 <- rbind(prec1, prec2)
prec1 <- prec1 %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge)
  
saveRDS(prec1, paste0(savepath, "/hourly_dwd_sun_2000-2023.rds"))

prec1 <- readRDS(paste0(savepath, "/hourly_dwd_sun_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_7)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_sun_2000-2023.rds"))

# wind
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_wind_2000-2023.rds" ))
prec1 <- prec1 %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge) %>%
  select(-QN_3)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_wind_2000-2023.rds"))

# cloudiness
prec1 <- readRDS(paste0(savepath, "/hourly_dwd_cloudiness_2000-2023.rds" ))

prec1 <- prec1 %>%
  select(-bis_datum)%>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge)
  
saveRDS(prec1, paste0(savepath, "/hourly_dwd_cloudiness_2000-2023.rds"))

prec1 <- readRDS(paste0(savepath, "/hourly_dwd_cloudiness_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_8)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_cloudiness_2000-2023.rds"))

# temperature
library(readr)
hourly_dwd_air_temperature_2000_2023 <- read_csv("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim/WD_hourly/hourly_dwd_air_temperature_2000-2023.csv")
hourly_dwd_air_temperature_2000_2023 <- hourly_dwd_air_temperature_2000_2023 %>%
  select(-eor) %>%
  select(-bis_datum) %>%
  select(-von_datum) %>%
  select(-Geogr.Breite) %>%
  select(-Geogr.Laenge) %>%
  select(-Stationsname) %>%
  select(-Stationshoehe)
  
saveRDS(hourly_dwd_air_temperature_2000_2023, paste0(savepath, "/hourly_dwd_air_temperature_2000-2023.rds"))

prec1 <- readRDS(paste0(savepath, "/hourly_dwd_air_temperature_2000-2023.rds" ))
prec1 <- prec1 %>% select(-QN_9)

saveRDS(prec1, paste0(savepath, "/hourly_dwd_air_temperature_2000-2023.rds"))

# extreme wind
dwd <- readRDS(paste0(savepath, "/hourly_dwd_extreme_wind_2000-2023.rds" ))
dwd <- dwd %>% select(-QN_8) %>% select(-Geogr.Breite) %>% select(-Geogr.Laenge) %>%
  select(-von_datum) %>% select(-bis_datum)

saveRDS(dwd, paste0(savepath, "/hourly_dwd_extreme_wind_2000-2023.rds"))

# dew point

dwd <- readRDS(paste0(savepath, "/hourly_dwd_dew_point_2000-2023.rds" ))
dwd <- dwd %>% select(-QN_8) %>% select(-Geogr.Breite) %>% select(-Geogr.Laenge) %>%
  select(-von_datum) %>% select(-bis_datum)

saveRDS(dwd, paste0(savepath, "/hourly_dwd_dew_point_2000-2023.rds"))

