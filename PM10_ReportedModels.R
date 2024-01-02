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
library(modelsummary)
library(latex2exp)
## Prep #########
processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data"
report_scenario2 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/scenario2"
figurepath_scenario2 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/figures/scenario2"
report_scenario2_app <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/scenario2/app"

# set up fixest dictionaries
setFixest_dict(c(PM10 = "PM10 [$\\mu g /m^3$]", TT_TU = "Temperature [°C]",
                 F = "Wind Speed [m/s]", D = "Wind Direction [°]", RF_TU = "Relative Humidity [%]", 
                 V_N = "Cloud Coverage [1/8]", VP_STD = "Vapor Pressure [hPa]",
                 FX_911 = "Extreme Wind Speed [m/s]", R1 = "Hourly Precipitation [mm]", WRTR = "Type of Precipitation [code]", 
                 P = "Pressure [hPa]", SD_SO = "Hourly Sunshine [min]", ABSF_STD = "Absolute Humidity [$\\mu g /m^3$]", 
                 TF_STD = "Humid Temperature [°C]", HDH = "Heating Degree Hour", 
                 lk_id = "Landkreis", background = "Background", crisis = "Energy Crisis", 
                 HoD = "Hour of Day", DoW = "Day of Week",  Forest = "Forest Area (Kreis)", area_resiandtraffic = "Residential and Traffic Area (Kreis)", 
                 disp_income = "Disposable Income (Landkreis)", hhsize = "Householdsize (Landkreis)", flatsperhouse = "Number of flats per building (Landkreis)", cars = "Car Registration (Landkreis)",
                 areaperflat_lk = "Area per flat (Landkreis)" ), Trend_Month = "Monthly Trend", month_Year = "Year-Month", meantemp_month = "Mean Monthly Temperature", monthstocrisis = "Months to beginning of crisis")
weather <- c("TT_TU", "F", "D", "TD", "RF_TU", "V_N", "VP_STD", "FX_911", "R1", "WRTR", 
             "P", "SD_SO", "ABSF_STD", "TF_STD")
weather_woWRTR <- c("TT_TU", "F", "D", "TD", "RF_TU", "V_N", "VP_STD", "FX_911", "R1", 
                    "P", "SD_SO", "ABSF_STD", "TF_STD")
conf <-  c("Forest" ,"area_resiandtraffic" , "disp_income" ,"hhsize" , "flatsperhouse" , "cars" , "areaperflat_lk")
geo <- c( "buildings_resi", 
          "distance_primaryroad", "distance_secondaryroad", "distance_tertiaryroad", 
          "number_primtotert", "number_resi", "distance_resi", "Geogr.Laenge", 
          "Geogr.Breite", "lk_id", "kreis", "NUTS_CODE", "Gemeinde_id", "Gemeinde")

setFixest_fml(..ctrl = ~ .[weather_woWRTR], ..ctrl_squared = ~.[weather_woWRTR]^2, reset = TRUE)


aqwd <- readRDS(paste0(processedpath, "/model_final_ready.rds")) %>%
  select(- c(all_of(conf), all_of(geo)))
aqwd <- aqwd %>% 
  mutate(crisis_month = case_when(crisis == 1 ~ Month, .default = 0))
aqwd <- aqwd %>% 
  mutate(crisis_HDH = case_when(crisis == 1 ~ HDH, .default = 0))
aqwd <- aqwd %>%
  mutate(monthsincecrisis = case_when(crisis == 1 ~ Month - 2, .default = 0))
aqwd <- aqwd %>%
  mutate(timesincecrisis = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 26, .default = 0 ))
aqwd <- aqwd %>%
  mutate(timeofday = case_when(HoD < 5|HoD >= 23 ~ "Night", HoD >=5 & HoD < 11 ~"Morning", 
                               HoD >= 11 & HoD < 16 ~ "Noon", HoD >= 16 & HoD < 23 ~"Evening" ))
aqwd <- aqwd %>%
  mutate(timeofday_3h = HoD %/% 3)
aqwd <- aqwd %>%
  group_by(Trend_month) %>%
  mutate(meantemp_month = mean(TT_TU, na.rm = TRUE)) %>%
  ungroup()
aqwd <- aqwd %>%
  mutate(monthstocrisis = case_when(crisis == 1 ~ Trend_month - 146, crisis == 0 & (Year == 2022 | (Year == 2021 & Month > 10)) ~ Trend_month - 146, .default = 0 ))
aqwd <- aqwd %>%
  mutate(months2tocrisis = case_when(monthstocrisis %in% c(-1,-2,-3) ~ -1 , monthstocrisis == 0 ~ 0, monthstocrisis %in% c(1,2,3) ~ 1 , monthstocrisis %in% c(4,5,6) ~ 2, monthstocrisis %in% c(7,8,9) ~ 3, monthstocrisis == 10 ~ 4, .default = 0))
aqwd <- aqwd %>%
  mutate(crisi_resi2 = case_when(monthstocrisis > -3 & resi_station_2 == 1 ~ 1,  .default = 0))
## FE ######
# Model without varying slopes

gc()
feols <- feols(PM10 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + sw(station + Year + Month, station^Year + Month)
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
# Model that includes varying slopes 
feols[[3]] <- feols(PM10 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + station[Trend_month] + Month ,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)

etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario2, "/FE_scenario2.tex"), 
       label = "tab:FE_scenario2", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model: Including different time fixed effects")
etable(feols, tex = TRUE, file=paste0(report_scenario2_app, "/all_FE_scenario2.tex"), 
       label = "tab:all_FE_scenario2", adjustbox = TRUE,
       replace = TRUE , title = "FE Model with station and time or station-time FEs")
# Robustness test
remove(feols)
gc()
feols <- feols(PM10 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |csw0(station, Year, Month, HoD + DoW, holiday)
               ,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario2_app, "/all_FE_scenario2_robustnessFEs.tex"), 
       label = "tab:all_FE_scenario2_robustnessFE", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of different fixed effects")
# Robustness other confounders
remove(feols)
gc()
feols <- feols(PM10 ~ crisis
               + ..ctrl + csw0(..ctrl_squared,
                               covid) +
                 no_rain:P +no_rain:cold_day +
                 cold_day:P + low_wind:P +
                 TT_TU:RF_TU + TT_TU:SD_SO
               |station + Year + Month + HoD + DoW +holiday
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario2_app, "/all_FE_scenario2_robustness_confs.tex"), 
       label = "tab:all_FE_scenario2_robustness_confs", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of different additional confounding variables")
gc()
feols <- feols(PM10 ~ crisis
               + ..ctrl + csw0(no_rain:P  + no_rain:cold_day,
                               cold_day:P +low_wind:P,
                               TT_TU:RF_TU + TT_TU:SD_SO) + ..ctrl_squared +
                 covid
               |station + Year + Month + HoD + DoW +holiday
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols, tex = TRUE, file=paste0(report_scenario2_app, "/all_FE_scenario2_robustness_ints.tex"), 
       label = "tab:all_FE_scenario2_robustness_ints", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of different weather interaction variables")

# FE Model event study (scenario 2)
# Model that includes varying slopes 
## with time to crisis 
remove(feols)
gc()
feols <- feols(PM10 ~ i(monthstocrisis, ref = 0)
               + .[weather_woWRTR]
               + .[weather_woWRTR]^2 
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW
               + holiday
               + station[Trend_month] + Month,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario2, "/FE_scenario2_Event_timetomonths.tex"), 
       label = "tab:FE_scenario2_Event_timetomonths", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "Event Study where crisis begins in month = 0 and a time trend and Month fixed effects account for time varying factors")

png(filename=paste0(figurepath_scenario2, "/FE_scenario2_event_study_timetomonths.png"),
    width=600, height=350)
iplot(feols, main = "FE model: Effect on PM10", ref.line = TRUE)

dev.off()
etable(feols, tex = TRUE, file=paste0(report_scenario2_app, "/all_FE_scenario2_Event_timetomonths.tex"), 
       label = "tab:all_FE_scenario2_Event_timetomonths", adjustbox = TRUE,
       replace = TRUE , title = "Event Study where crisis begins in month = 0 and a time trend and Month fixed effects account for time varying factors")



## DID ########
remove(feols)
gc()

feols <- feols(PM10 ~ HDH + crisis:HDH
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               +station[Trend_month] + Month , # +station^Month^Year,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_scenario2_HDHcrisis_m-y_fsplitt.tex"), label = "tab:DID_HDHcrisis_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Using Heating Degree as Continous Treatment")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_Scenario2_HDHcrisis__fsplitt.tex"), label = "tab:all_DID_HDHcrisis_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Using Heating Degree as Continous Treatment")

# Event of DID with monthly FEs as dynamic effects
remove(feols)
gc()
feols <- feols(PM10 ~ i(monthstocrisis,crisis_HDH, ref = 0) + HDH
               + .[weather_woWRTR]
               + .[weather_woWRTR]^2
               + covid 
               |HoD
               +DoW + holiday
               +station[Trend_month]
               + Month,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)

png(filename=paste0(figurepath_scenario2, "/DID_event_scenario2_HDHcrisis_full.png"),
    width=600, height=350)

iplot(feols[[1]],drop = "12", main = "Effect on PM10")# a function call to save the file
dev.off()

png(filename=paste0(figurepath_scenario2, "/DID_event_scenario2_HDHcrisis_bg.png"),
    width=600, height=350)

iplot(feols[2],drop = "12", main = "Effect on PM10")# a function call to save the file
dev.off()

png(filename=paste0(figurepath_scenario2, "/DID_event_Scenario2_HDHcrisis_traffic.png"),
    width=600, height=350)

iplot(feols[3],drop = "12", main = "Effect on PM10")# a function call to save the file
dev.off()


etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_event_scenario2_HDHcrisis_fsplitt.tex") , label = "tab:DID_HDHcrisis_event", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square"), 
       title = "MOnthly Difference in Difference strategy: Continous Treatment based on Heating Degree.")


## DID Resi ######
remove(feols)
gc()
# with year-month
feols <- feols(PM10 ~  crisis:resi_station_2 + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               +station + Year^Month , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_resi2_m-y_fsplitt.tex"), label = "tab:DID_resi2_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_resi2_m-y_fsplitt.tex"), label = "tab:all_DID_resi2_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
# resi station 3
remove(feols)
gc()

feols <- feols(PM10 ~  crisis:resi_station_3 + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + Month^Year +station , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_resi3_m-y_fsplitt.tex"), label = "tab:DID_resi3_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_resi3_m-y_fsplitt.tex"), label = "tab:all_DID_resi3_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")

# with trend and month
feols <- feols(PM10 ~  crisis:resi_station_2 + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               +station[Trend_month] + Month , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_resi2_trendandmonth_fsplitt.tex"), label = "tab:DID_resi2_trendandmonth_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_resi2_trendandmonth_fsplitt.tex"), label = "tab:all_DID_resi2_trendandmonth_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
# resi station 3
remove(feols)
gc()

feols <- feols(PM10 ~  crisis:resi_station_3 + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               +station[Trend_month] + Month , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_resi3_trendandmonth_fsplitt.tex"), label = "tab:DID_resi3_trendandmonth_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_resi3_trendandmonth_fsplitt.tex"), label = "tab:all_DID_resi3_trendandmonth_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")

## Did event resi
remove(feols)
gc()
# with year-month
feols <- feols(PM10 ~  i(monthstocrisis,crisi_resi2, ref = 0) + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               +station[Trend_month] + Month , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)


etable(feols, tex = TRUE, file = paste0(report_scenario2, "/DID_event_resi2_monthtrend.tex"), label = "tab:DID_event_resi2_monthtrend_", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario2_app, "/all_DID_event_resi2_monthtrend_.tex"), label = "tab:all_DID_resi2_monthtrend_event", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")


png(filename=paste0(figurepath_scenario2, "/DID_event_scenario2_resi2_monthtrend.png"),
    width=600, height=350)

iplot(feols, main = "Effect on PM10")# a function call to save the file
dev.off()

