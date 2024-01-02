
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)
library(readr)
library(imputeTS)
library(naniar)
rm(list = ls())
library(VIM)
#library(missForest)
#library(VIM)

processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data"
figurepath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Figures"
Components <- c("PM10", "PM25", "CO", "NO", "NO2", "SO2", "O3")
externalpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external"
Components_exPM <- c("CO", "NO", "NO2", "SO2", "O3")
# import filtered stationsids
# year_min <- 2010
# perc_max_char <- "05"
# stat_filtered <- readRDS(paste0(stationpath, str_glue("/list_filtered_stations_{year_min}_{perc_max_char}.rds")))

# Filter outliers -----------------------------------

aqwd <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_modelprep_f_2010-2023_conf_cov.rds")))

# Filter out new years eve
aqwd <- aqwd %>%
  ungroup() %>%
  filter(!(Day ==1 & Month ==1))
# filter station type
aqwd <- aqwd %>%
  filter(!type_of_station == "industry")
# Detect and filter out outliers
# zscore function to remove outliers
isnt_out_z <- function(x, thres = 20, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm) # median instead would work
}
# remove outliers of all components
aqwd <- aqwd %>%
  mutate(across(all_of(Components),~ case_when(. < 0 ~ NA, . >=0 ~ .))) %>%
  mutate(across(all_of(Components_exPM), ~ case_when(!isnt_out_z(.) ~ NA, isnt_out_z(.)~.))) %>%
  mutate(across(all_of(c("PM10", "PM25")), ~case_when(. > 500 ~ NA, .default = .)))



# Impute missing values for Dep. Variable
aqwd <- aqwd %>% group_by(station) %>% arrange(station, datetime) %>%
  # Approximate each observation based on groups of station Ids -> arrange by date
  # what should be the maximum missing valule approx. allowed?
  #mutate(across(all_of(Components), ~ na.approx(., na.rm = FALSE, rule = 2, maxgap = 24)))
  mutate(PM10 =  na.approx(PM10, na.rm = FALSE, rule = 2, maxgap = 24))
gc()
aqwd <- aqwd %>%
  mutate(PM25 = na.approx(PM25, na.rm = FALSE, rule = 2, maxgap = 24))
aqwd <- aqwd %>%
  mutate(NO2 = na.approx(NO2, na.rm = FALSE, rule = 2, maxgap = 24))
aqwd <- aqwd %>%
  mutate(O3 = na.approx(O3, na.rm = FALSE, rule = 2, maxgap = 24))
gc()
aqwd <- aqwd %>%
  mutate(NO = na.approx(NO, na.rm = FALSE, rule = 2, maxgap = 24))
gc()
aqwd <- aqwd %>%
  mutate(SO2 = na.approx(SO2, na.rm = FALSE, rule = 2, maxgap = 24))
gc()
aqwd <- aqwd %>%
  mutate(CO = na.approx(CO, na.rm = FALSE, rule = 2, maxgap = 24))

saveRDS(aqwd, paste0(processedpath, str_glue("/AQWD_hourly_model_comimpute.rds")))

# Impute Missing values for weather
include <- c("datetime", "R1","WRTR", "TT_TU", "RF_TU", "TD", "F",
             "D", "FX_911", "P", "P0", "SD_SO", "V_N", "ABSF_STD", "VP_STD", 
             "TF_STD", "Geogr.Laenge", "Geogr.Breite")
include_WOWRTR <- c("datetime", "R1", "TT_TU", "RF_TU", "TD", "F",
                    "D", "FX_911", "P", "P0", "SD_SO", "V_N", "ABSF_STD", "VP_STD", 
                    "TF_STD", "Geogr.Laenge", "Geogr.Breite")
ignore <- colnames(aqwd)[!colnames(aqwd) %in% include]
# MIssRanger Imputation -> too large data
#RHS <- paste(include, collapse = "+")
#LHS_ignore <-c("station", "datetime", "Geogr.Laenge", "Geogr.Breite")
#LHS_plus <- paste(ignore[!RHS %in% LHS_ignore] )
#LHS <- paste0(".")
#LHS_ignore_all <- c(ignore, LHS_ignore)
#for (i in 1:length(LHS_ignore_all)){
#  LHS <- paste0(LHS, "-", LHS_ignore_all[i], " ")
#}
#form <- as.formula(paste0(LHS, "~", RHS))
##aqwd <- mlim(aqwd, m=1,ignore = c("station"), seed = 2023) 
#aqwd <- missRanger(aqwd,
#  formula = form,
#  pmm.k = 0,
st_year <- 2020
for (st_year in c(2010, 2015, 2020)){
  if (st_year < 2020) {
    end_year <- st_year + 4
  }
  
  if(st_year == 2020){
    end_year <-2023
  }
  #KNN imputation with VIM -> Too much space
  weather <- c( "R1","WRTR", "TT_TU", "RF_TU", "TD", "F",
               "D", "FX_911", "P", "P0", "SD_SO", "V_N", "ABSF_STD", "VP_STD", 
               "TF_STD")
  #aqwd <- VIM::kNN(aqwd,variable = weather, dist_var = c("Geogr.Breite", "Geogr.Laenge", "datetime"))
  
  
  aqwd <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_model_comimpute.rds"))) %>%
    ungroup() 
  aqwd <- aqwd %>%
    distinct(across(c(station, datetime)), .keep_all = TRUE)
  gc()
  aqwd <- aqwd %>% filter(Year >= st_year & Year <= end_year)
  # Regression Imputation for only R1
  #aqwd <- regressionImp(
  # as.formula(paste0("R1 ~ ", paste(include[!include %in% "R1"], collapse = "+"))),
  #  aqwd,
  gc()
  
  
  i <- 1
  weather_num <- weather[!weather %in% "WRTR"]
  # for all weather components
  for (i in 1:length(weather_num)){
  #for (i in 1:1){
    wcomp <- weather_num[i]
    # Train regression model for one component with data w/o nan values
    Train <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_model_comimpute.rds"))) %>%
      ungroup()%>% filter(Year >= st_year & Year <= end_year) %>%
      select(c(all_of(include_WOWRTR), "station")) %>%
      distinct(across(c(station, datetime)), .keep_all = TRUE)
    
    Train <- Train %>%
      group_by(station) %>% 
      arrange(station, datetime) %>%
      mutate(across(all_of(weather_num[!weather_num %in% wcomp]), ~ replace_na(., median(.,na.rm = TRUE)))) %>%
      ungroup()
    # 
    Train <- regressionImp(
      as.formula(paste0(wcomp," ~ ", paste(include_WOWRTR[!include_WOWRTR %in% wcomp], collapse = "+"))),
      Train) %>%
      select(c(station, datetime, all_of(wcomp)))
    
    aqwd <- aqwd %>%
      select(-all_of(wcomp))
    
    aqwd <- left_join(aqwd, Train, by = c("station", "datetime"))
    gc()
  }
  
  #For WRTR
  
  
  
  # Use imri method 
  #list_models <- list()
  #for (i in 1:length(weather)){
  #  wcomp <- weather[i]
  #  form_comp <- paste0(wcomp," = c('" , paste(include[!include %in% wcomp], collapse = "','" ), "')", sep = "")
  #  list_models[[i]] <- form_comp
  #}
  #aqwd <- aqwd %>% filter(Year > 2020)
  #aqwd <- irmi(aqwd, modelFormulas = list_models,maxit = 1, init.method = "median", imp_var = FALSE)
  # Maybe:
  # Filter out stations with too little ovservations
  #stat_com <- stat_filtered %>%
  #  pull(c(component))
  #aqwd <- aqwd %>% 
  #  filter(station %in% stat_com$station) 
  
  #save
  saveRDS(aqwd, paste0(processedpath, str_glue("/AQWD_hourly_model_final_{st_year}_{end_year}.rds")))

}
# Add years together
aqwd <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_model_final_2010_2014.rds")))


#for (st_year in c(2005, 2010, 2015, 2020)){
for (st_year in c(2015, 2020)){
  if (st_year <2020){
    end_year <- st_year + 4
  }
  if (st_year == 2020){
    end_year <- 2023
  } 
  new_aqwd <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_model_final_{st_year}_{end_year}.rds")))
  aqwd <- bind_rows(aqwd, new_aqwd)
  
}
saveRDS(aqwd, paste0(processedpath, str_glue("/AQWD_hourly_model_final_all.rds")))

