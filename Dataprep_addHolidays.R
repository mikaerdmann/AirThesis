
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)

processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"

aqwd <- readRDS(paste0(processedpath, str_glue("/AQWD_hourly_model_final_all.rds")))
years <- 2000:2023

### list of public holidays for all states ####
holidays_all <- c("01-01", "05-01", "10-03", "12-25", "12-26")
holidays_all_date <- c()
for (i in 1:length(holidays_all)){
  holidays_all_date <- append(holidays_all_date,paste(years,holidays_all[i], sep = "-"))
}


# Dates of easter sunday from 2000-2023
easter_sunday <- c("04-23","04-15", "03-31", "04-20", "04-11", "03-27", "04-16", "04-08",
                   "03-23", "04-12", "04-04", "04-24", "04-08", "03-31", "04-20", 
                   "04-05", "03-27", "04-16", "04-01", "04-21", 
                   "04-12", "04-04", "04-17", "04-09")
easter_date <- c()
easter_date <- append(easter_date,paste(years,easter_sunday, sep = "-"))

easter_holidays <- c()
sfd <- stamp_date("2000-04-31")

for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  easter_holidays <- append(easter_holidays, c(easter, 
                     sfd(ymd(easter) + days(2)), 
                     sfd(ymd(easter) + days(1)), 
                     sfd(ymd(easter) + days(39)),
                    sfd(ymd(easter) + days(50)))
                     
)
}

### Varying public holidays by date #######
# Bawü
bw_hol <- c("01-06", "11-01")
bw_hol_date <- c()
for (i in 1:length(bw_hol)){
  bw_hol_date <- append(bw_hol_date,paste(years,bw_hol[i], sep = "-"))
}

# Bawü easter
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  bw_hol_date <- append(bw_hol_date,sfd(ymd(easter) + days(60)))
}

# By
by_hol <- c("01-06", "08-15", "11-01")
by_hol_date <- c()
for (i in 1:length(by_hol)){
  by_hol_date <- append(by_hol_date,paste(years,by_hol[i], sep = "-"))
}
#easter
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  by_hol_date <- append(by_hol_date,sfd(ymd(easter) + days(60)))
}

# BE
be_hol <- c("03-08")
be_hol_date <- c()
for (i in 1:length(be_hol)){
  be_hol_date <- append(be_hol_date,paste(years,be_hol[i], sep = "-"))
}

# BB
bb_hol <- c("10-31")
bb_hol_date <- c()
for (i in 1:length(bb_hol)){
  bb_hol_date <- append(bb_hol_date,paste(years,bb_hol[i], sep = "-"))
}

# HB
hb_hol <- c("10-31")
hb_hol_date <- c()
for (i in 1:length(hb_hol)){
  hb_hol_date <- append(hb_hol_date,paste(2018:2023,hb_hol[i], sep = "-"))
}

# HH
hh_hol <- c("10-31")
hh_hol_date <- c()
for (i in 1:length(hh_hol)){
  hh_hol_date <- append(hh_hol_date,paste(2018:2023,hh_hol[i], sep = "-"))
}

# HE
#easter
he_hol_date <- c()
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  he_hol_date <- append(he_hol_date,sfd(ymd(easter) + days(60)))
}

# MV
mv_hol <- c("03-08", "10-31")
mv_hol_date <- c()
for (i in 1:length(mv_hol)){
  mv_hol_date <- append(mv_hol_date,paste(years,mv_hol[i], sep = "-"))
}

# NI
ni_hol <- c("10-31")
ni_hol_date <- c()
for (i in 1:length(ni_hol)){
  ni_hol_date <- append(ni_hol_date,paste(2018:2023,ni_hol[i], sep = "-"))
}

# NW
nw_hol <- c("11-01")
nw_hol_date <- c()
for (i in 1:length(nw_hol)){
  nw_hol_date <- append(nw_hol_date,paste(2018:2023,nw_hol[i], sep = "-"))
}
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  nw_hol_date <- append(nw_hol_date,sfd(ymd(easter) + days(60)))
}

# RP
rp_hol <-  c("11-01")
rp_hol_date <- c()
for (i in 1:length(rp_hol)){
  rp_hol_date <- append(rp_hol_date,paste(2018:2023,rp_hol[i], sep = "-"))
}
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  rp_hol_date <- append(rp_hol_date,sfd(ymd(easter) + days(60)))
}

# SL

sl_hol <- c("08-15", "11-01")
sl_hol_date <- c()
for (i in 1:length(sl_hol)){
  sl_hol_date <- append(sl_hol_date,paste(years,sl_hol[i], sep = "-"))
}
#easter
for (eas in 1:length(easter_date)){
  easter <- easter_date[eas]
  sl_hol_date <- append(sl_hol_date,sfd(ymd(easter) + days(60)))
}

# SN
sn_hol <- c("10-31")
sn_hol_date <- c()
for (i in 1:length(sn_hol)){
  sn_hol_date <- append(sn_hol_date,paste(years,sn_hol[i], sep = "-"))
}

# ST
st_hol <- c("10-31", "01-06")
st_hol_date <- c()
for (i in 1:length(st_hol)){
  st_hol_date <- append(st_hol_date,paste(years,st_hol[i], sep = "-"))
}

# SH
sh_hol <- c("10-31")
sh_hol_date <- c()
for (i in 1:length(sh_hol)){
  sh_hol_date <- append(sh_hol_date,paste(2018:2023,sh_hol[i], sep = "-"))
}

# TH
th_hol <- c("10-31")
th_hol_date <- c()
for (i in 1:length(th_hol)){
  th_hol_date <- append(th_hol_date,paste(years,th_hol[i], sep = "-"))
}
th_hol_date <- append(th_hol_date, paste(2019:2023, "09-20", sep = "-"))

## Join to data ##########

# 
holiday_func <- function(Dates, bundeslander){
  values <- c()
  for (i in 1:length(Dates)){
    Date <- sfd(Dates[i])
    bundesland <- bundeslander[i]
    if (Date %in% holidays_all_date| Date %in% easter_holidays){
      values[i] <- TRUE
    }
    
    else {
      if (bundesland == "debw"){
        if (Date %in% bw_hol_date){
          values[i] <- TRUE
        }
        else {
          values[i] <- FALSE
        }
      }
      if (bundesland == "deby"){
        if (Date %in% by_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i]<- FALSE
        }
      }
      if (bundesland == "dehe"){
        if (Date %in% he_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "dehh"){
        if (Date %in% hh_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "dehb"){
        if (Date %in% hb_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "deni"){
        if (Date %in% ni_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "demv"){
        if (Date %in% mv_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "debb"){
        if (Date %in% bb_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "debe"){
        if (Date %in% be_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "desa"){
        if (Date %in% sa_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "desn"){
        if (Date %in% sn_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "desl"){
        if (Date %in% sl_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "derp"){
        if (Date %in% rp_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "denw"){
        if (Date %in% nw_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "deth"){
        if (Date %in% th_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
      if (bundesland == "dest"){
        if (Date %in% st_hol_date){
          values[i] <- TRUE
        }
        else{
          values[i] <- FALSE
        }
      }
    }
  }
  
  return(values)
}

# case_when function

case_holiday <- function(dates, bundeslander){
  case_when(
    dates %in% holidays_all_date| dates %in% easter_holidays ~ 1, 
    bundeslander == "debw" & dates %in% bw_hol_date ~1,
    bundeslander == "debb" & dates %in% bb_hol_date ~1,
    bundeslander == "debe" & dates %in% be_hol_date ~1,
    bundeslander == "deby" & dates %in% by_hol_date ~1,
    bundeslander == "dehb" & dates %in% hb_hol_date ~1,
    bundeslander == "dehh" & dates %in% hh_hol_date ~1,
    bundeslander == "desh" & dates %in% sh_hol_date ~1,
    bundeslander == "deni" & dates %in% ni_hol_date ~1,
    bundeslander == "demv" & dates %in% mv_hol_date ~1,
    bundeslander == "denw" & dates %in% nw_hol_date ~1,
    bundeslander == "derp" & dates %in% rp_hol_date ~1,
    bundeslander == "dest" & dates %in% st_hol_date ~1,
    bundeslander == "desn" & dates %in% sn_hol_date ~1,
    bundeslander == "dehe" & dates %in% he_hol_date ~1,
    bundeslander == "deth" & dates %in% th_hol_date ~1,
    bundeslander == "slbw" & dates %in% sl_hol_date ~1,
    .default = 0
  )
}

dates <- aqwd %>% slice_sample(n = 50) %>% pull(date)
bundeslander <- aqwd  %>% select(bundesland) %>% count(bundesland)
new <- bundeslander%>% drop_na(bundesland)

#values <- holiday_func(dates, bundeslander)


aqwd<- aqwd %>%
  mutate(holiday = case_holiday(sfd(date), bundesland))

saveRDS(aqwd, paste0(processedpath, "/AQWD_hourly_model_final_holidays.rds" ))
## Add brennholzpreise

                      