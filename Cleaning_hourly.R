library(tidyverse)
library(dplyr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(stargazer)
library(naniar)


processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
datapath <-  "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/interim"

aqwd <- readRDS(paste0(datapath, "/AQWD_merge_2010-2023_update_f.rds"))

aqwd <- aqwd %>%
  #replace_with_na_at(.vars = c("ABSF_STD","VP_STD", "TF_STD", "P_STD", "TT_STD", "RF_STD", "TD_STD"), condition = ~.x == -99.9 | .x == 999)
  replace_with_na(replace = list(ABSF_STD = c(-99.9, -999), 
                                 VP_STD = c(-99.9, -999), 
                                 TF_STD = c(-99.9, -999),
                                 RF_TU = c(-99.9, -999), 
                                 
                                P = c(-99.9, -999), 
                                P0 = c(-99.9, -999), 
                                SD_SO = c(-99.9, -999),
                                 RF_STD = c(-99.9, -999), 
                                 D = c(990)))

# change types of AQ data
aqwd <- aqwd %>%
  mutate(across(where(is.character) & !c(station, datetime), as.numeric))
aqwd <- aqwd %>%
  distinct() 

saveRDS(aqwd, paste0(processedpath, "/AQWD_merge_2010-2023_p.rds"))
