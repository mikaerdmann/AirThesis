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
report_scenario1 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/PM25"
figurepath_scenario1 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/figures/PM25"
report_scenario1_app <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/PM25/app"

# set up fixest dictionaries
setFixest_dict(c(PM25 = "PM25 [$\\mu g /m^3$]", TT_TU = "Temperature [°C]", 
                 F = "Wind Speed [m/s]", D = "Wind Direction [°]", RF_TU = "Relative Humidity [%]", TD = "Dew point Temperature [°C]",
                 V_N = "Cloud Coverage [1/8]", VP_STD = "Vapor Pressure [hPa]",
                 FX_911 = "Extreme Wind Speed [m/s]", R1 = "Hourly Precipitation [mm]", WRTR = "Type of Precipitation [code]", 
                 P = "Pressure [hPa]", SD_SO = "Hourly Sunshine [min]", ABSF_STD = "Absolute Humidity [$\\mu g /m^3$]", 
                 TF_STD = "Humid Temperature [°C]", HDH = "Heating Degree Hour", 
                 lk_id = "Landkreis", background = "Background", crisis = "Energy Crisis", 
                 HoD = "Hour of Day", DoW = "Day of Week",  Forest = "Forest Area (Kreis)", area_resiandtraffic = "Residential and Traffic Area (Kreis)", 
                 disp_income = "Disposable Income (Landkreis)", hhsize = "Householdsize (Landkreis)", flatsperhouse = "Number of flats per building (Landkreis)", cars = "Car Registration (Landkreis)",
                 areaperflat_lk = "Area per flat (Landkreis)" ), Trend_Month = "Monthly Trend", month_Year = "Year-Month", meantemp_month = "Mean Monthly Temperature", monthstocrisis = "Months to beginning of crisis", 
               stat_type = "Type of Station and Area")
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
  mutate(timesincecrisis26 = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 26, (Year == 2022 & yday(datetime) < 55 & yday(datetime) > 27) ~ -1, .default = -2))
aqwd <- aqwd %>%
  mutate(dayssincecrisis = (daily_trend- 4425))
aqwd <- aqwd %>%
  mutate(timesincecrisis26_indays = case_when(timesincecrisis26 == -1 ~-26, .default = timesincecrisis26*26))
aqwd <- aqwd %>%
  mutate(timesincecrisis26_indays_2 = case_when(timesincecrisis26 == -1 ~-26, timesincecrisis26 == -2 ~ 0, timesincecrisis26 == 0 ~ -1, .default = timesincecrisis26*26))
aqwd<- aqwd %>%
  mutate(timesincecrisis28 = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 28, (Year == 2022 & yday(datetime) < 55 & yday(datetime) > 27) ~ -1, .default = -2))
aqwd <- aqwd %>%
  mutate(timesincecrisis28_indays = case_when(timesincecrisis28 == -1 ~-28, .default = timesincecrisis28*28))
aqwd<- aqwd %>%
  mutate(timesincecrisis27 = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 27, (Year == 2022 & yday(datetime) < 55 & yday(datetime) > 27) ~ -1, .default = -2))
aqwd <- aqwd %>%
  mutate(timesincecrisis27_indays = case_when(timesincecrisis27 == -1 ~-27, .default = timesincecrisis27*27))
aqwd<- aqwd %>%
  mutate(timesincecrisis30 = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 30, (Year == 2022 & yday(datetime) < 55 & yday(datetime) > 27) ~ -1, .default = -2))
aqwd <- aqwd %>%
  mutate(timesincecrisis30_indays = case_when(timesincecrisis30 == -1 ~-30, .default = timesincecrisis30*30))
aqwd<- aqwd %>%
  mutate(timesincecrisis29 = case_when(crisis == 1 ~ (lubridate::yday(datetime) - 53) %/% 29, (Year == 2022 & yday(datetime) < 55 & yday(datetime) > 27) ~ -1, .default = -2))
aqwd <- aqwd %>%
  mutate(timesincecrisis29_indays = case_when(timesincecrisis29 == -1 ~-29, .default = timesincecrisis29*29))




aqwd <- aqwd %>%
  mutate(timeofday = case_when(HoD < 5|HoD >= 23 ~ "Night", HoD >=5 & HoD < 11 ~"Morning", 
                               HoD >= 11 & HoD < 16 ~ "Noon", HoD >= 16 & HoD < 23 ~"Evening" ))
aqwd <- aqwd %>%
  mutate(monthstocrisis = case_when(crisis == 1 ~ Trend_month - 146, crisis == 0 & (Year == 2022 | (Year == 2021 & Month > 10)) ~ Trend_month - 146, .default = 0 ))
aqwd <- aqwd %>%
  mutate(stat_type = str_c(station_type_of_area, type_of_station, sep = " "))

## FE ######
# Model without varying slopes
aqwd <- aqwd %>%
  drop_na(all_of(c(weather_woWRTR, "PM25")))

remove(feols)
gc()
feols <- feols(PM25 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + sw(station + Year^Month, station^Year^Month)
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
# Model that includes varying slopes 
feols[[3]] <- feols(PM25 ~ crisis
                    + ..ctrl + ..ctrl_squared
                    + covid 
                    + no_rain:P +no_rain:cold_day +
                      cold_day:P+ low_wind:P+
                      TT_TU:RF_TU + TT_TU:SD_SO 
                    |HoD
                    +DoW + holiday
                    + station[Trend_month] + Month^Year,
                    data = aqwd, cluster= c("station "), collin.tol = 1e-12)

etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_stat-timeFes.tex"), 
       label = "tab:FE_stat-timeFEs", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model: Including different time fixed effects")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_stat-timeFes.tex"), 
       label = "tab:all_FE_stat-timeFEs", adjustbox = TRUE,
       replace = TRUE , title = "FE Model with station and time or station-time FEs")
# FE fsplit
remove(feols)
gc()
feols <- feols(PM25 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd, fsplit= ~type_of_station, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-m_fsplit.tex"), 
       label = "tab:FE_fsplit", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model for different types of stations")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-m_fsplit.tex"), 
       label = "tab:all_FE_fsplit", adjustbox = TRUE,
       replace = TRUE , title = "FE Model for different types of stations")

# heterogeneous effects stations
remove(feols)
gc()
feols <- feols(PM25 ~ i(type_of_station, crisis)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-m_hetstat.png"),
    width=600, height=350)
iplot(feols)
dev.off()

etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-m_hetstat.tex"), 
       label = "tab:FE_hetstat", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model for different types of stations")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-m_hetstat.tex"), 
       label = "tab:all_FE_hetstat", adjustbox = TRUE,
       replace = TRUE , title = "FE Model for different types of stations")
# resi
remove(feols)
gc()
aqwd <- aqwd %>%
  mutate(resi_stat = case_when(resi_station_2 == 1 ~ "residential", .default =  "non-residential"))

feols <- feols(PM25 ~ i(resi_stat, crisis)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-m_hetstat_resi.png"),
    width=600, height=350)
iplot(feols)
dev.off()

etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-m_hetstat_resi.tex"), 
       label = "tab:FE_hetstat_resi", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model for different types of stations")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-m_hetstat_resi.tex"), 
       label = "tab:all_FE_hetstat_resi", adjustbox = TRUE,
       replace = TRUE , title = "FE Model for different types of stations")

# all type combis

remove(feols)
gc()
feols <- feols(PM25 ~ i(stat_type, crisis)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-m_hetstat_areatype.png"),
    width=650, height=350)
iplot(feols, drop = "rural traffic", main = "Effect on PM25")
dev.off()

etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-m_hetstat_areatype.tex"), 
       label = "tab:FE_hetstat_areatype", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model for different types of stations")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-m_hetstat_areatype.tex"), 
       label = "tab:all_FE_hetstat_areatype", adjustbox = TRUE,
       replace = TRUE , title = "FE Model for different types of stations")


# Robustness test

remove(feols)
gc()
feols <- feols(PM25 ~ crisis
               + ..ctrl + ..ctrl_squared
               + covid
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |csw0(station, Year^Month, HoD + DoW, holiday)
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_robustnessFEs.tex"), 
       label = "tab:all_FE_robustnessFE", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of different fixed effects")
# Robustness other confounders
remove(feols)
gc()
feols <- feols(PM25 ~ crisis
               + ..ctrl + sw0(.[weather_woWRTR]^2, covid)
               + no_rain:P +no_rain:cold_day +
                 cold_day:P + low_wind:P +
                 TT_TU:RF_TU + TT_TU:SD_SO
               |station + Year^Month + HoD + DoW +holiday
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_robustness_confs.tex"), 
       label = "tab:all_FE_robustness_confs", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of different additional confounding variables")

remove(feols)
gc()

# robustenss interactions
feols <- feols(PM25 ~ crisis
               + ..ctrl + csw0(no_rain:P  + no_rain:cold_day,
                               cold_day:P +low_wind:P,
                               TT_TU:RF_TU + TT_TU:SD_SO) + ..ctrl_squared +
                 covid
               |station + Year^Month + HoD + DoW +holiday
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_robustness_interactions.tex"), 
       label = "tab:all_FE_robustness_interactions", adjustbox = TRUE,
       replace = TRUE , title = "FE Model: Robustness to inclusion of weather interactions")

# FE Model event study  ###########
# -> monthstocrisis does not work (collinearity)


# with no FEs in the crisis period
# aqwd <- aqwd %>%
#   mutate(monthstocrisis = case_when(crisis == 1 ~ Trend_month - 146, crisis == 0 & (Year == 2022 | (Year == 2021 & Month > 10)) ~ Trend_month - 146, .default = 0 ))
# aqwd <- aqwd %>%
#   mutate(year_month = case_when(monthstocrisis == 0 & Trend_month != 146 ~str_c(as.character(Year), as.character(Month), sep = "_"), .default = "0")) %>%
#   arrange(date) %>%
#   group_by(year_month) %>%
#   mutate(year_month_id = case_when(year_month == "0" ~ 0, .default = cur_group_id()) )%>%
#   ungroup()
# remove(feols)
# gc()
# feols <- feols(PM25 ~ i(monthstocrisis, ref = 0) 
#                + ..ctrl + ..ctrl_squared
#                + covid 
#                + no_rain:P +no_rain:cold_day +
#                  cold_day:P+ low_wind:P+
#                  TT_TU:RF_TU + TT_TU:SD_SO
#                |HoD
#                +DoW + holiday
#                +station + year_month_id
#                ,
#                data = aqwd, cluster= c("station "), collin.tol = 1e-12)
# etable(feols)
# png(filename=paste0(figurepath_scenario1, "/FE_y-muntilcrisis_event.png"),
#     width=600, height=350)
# iplot(feols)
# dev.off()
# etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-muntilcrisis_event.tex"), 
#        label = "tab:FE_y-muntilcrisis_event", adjustbox = TRUE,
#        replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study (FEs until crisis)")
# etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-muntilcrisis_event.tex"), 
#        label = "tab:all_FE_y-muntilcrisis_event", adjustbox = TRUE,
#        replace = TRUE , title = "FE Event Study: Fes until crisis")
# 
# remove(feols)

# 
### Event month + Year + trend with month*crisis 


gc()
aqwd <- aqwd %>%
  mutate(doy = yday(datetime))
feols <- feols(PM25 ~ i(Month, crisis, ref = 2) 
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               +station[Trend_month] + Year + doy
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_month_event.png"),
    width=600, height=350)
iplot(feols, main = "Effect on PM25 over time", xlab= "Month")
dev.off()

#
gc()
feols <- feols(PM25 ~ i(timesincecrisis_indays, crisis, ref = 0) 
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               +station + Year^Month
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-dayssincecrisis26_event_ref5.png"),
    width=600, height=350)
iplot(feols, main = "Effect on PM25 over time", xlab= "days since beginning of crisis", drop = "-26")
dev.off()
etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-mFE_y-daysincecrisis26_event.tex"), 
       label = "tab:FE_y-mFE_y-daysincecrisis26_event_ref5", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-mFE_y-dayssincecrisis26_event_ref5.tex"), 
       label = "tab:all_FE_y-mFE_y-dayssincecrisis26_event_ref5", adjustbox = TRUE,
       replace = TRUE , title = "FE Event Study")
remove(feols)
gc()
feols <- feols(PM25 ~ i(timesincecrisis3_indays,crisis, ref = 0)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               +station + Year^Month
               ,
               data = aqwd, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-dayssincecrisis29_event_ref0.png"),
    width=600, height=350)
iplot(feols, main = "Effect on PM25 over time", xlab= "days since beginning of crisis", drop = "-")
dev.off()
etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-mFE_y-daysincecrisis29_event.tex"), 
       label = "tab:FE_y-mFE_y-daysincecrisis29_event", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-mFE_y-dayssincecrisis29_event.tex"), 
       label = "tab:all_FE_y-mFE_y-dayssincecrisis29_event", adjustbox = TRUE,
       replace = TRUE , title = "FE Event Study")

dayss <- 26:30
vars = c(str_glue("timesincecrisis{dayss}_indays"))
coefs <- c()
p <- c()
confint <- c()
gc()
i <- 4
for (i in 1:5){
  days <- dayss[i]
  gc()
  feols <- feols(xpd(PM25 ~ i(..time,crisis, ref = 0)
                     + ..ctrl + ..ctrl_squared
                     + covid 
                     + no_rain:P +no_rain:cold_day +
                       cold_day:P+ low_wind:P+
                       TT_TU:RF_TU + TT_TU:SD_SO
                     |HoD
                     +DoW + holiday
                     +station + Year^Month
                     , ..time= vars[i] ),
                 data = aqwd, cluster= c("station "), collin.tol = 1e-12)
  etable(feols)
  png(filename=paste0(figurepath_scenario1, str_glue("/FE_y-dayssincecrisis{days}_event_ref0.png")),
      width=14, height=10, unit = "cm", res = 400)
  iplot(feols, main = "Effect on PM25 over time", xlab= "days since beginning of crisis", drop = "-")
  dev.off()
  etable(feols, tex = TRUE, file=paste0(report_scenario1, str_glue("/FE_y-mFE_y-daysincecrisis{days}_event.tex")), 
         label = str_glue("tab:FE_y-mFE_y-daysincecrisis{days}_event"), adjustbox = TRUE,
         replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study")
  etable(feols, tex = TRUE, file=paste0(report_scenario1_app, str_glue("/all_FE_y-mFE_y-dayssincecrisis{days}_event.tex")), 
         label = str_glue("tab:all_FE_y-mFE_y-dayssincecrisis{days}_event"), adjustbox = TRUE,
         replace = TRUE , title = "FE Event Study")
  coefs[[i]] <- coeftable(feols)
  p[[i]] <- pvalue(feols)
  confint[[i]] <- confint(feols)
  
}

sum_dummies <- c(coefs, p, confint)
saveRDS(sum_dummies, paste0(report_scenario1_app, "/sumcoefs_event_dummies.rds"))
# feols <- feols(PM25 ~ crisis
#                + ..ctrl + ..ctrl_squared
#                + covid 
#                + no_rain:P +no_rain:cold_day +
#                  cold_day:P+ low_wind:P+
#                  TT_TU:RF_TU + TT_TU:SD_SO
#                |HoD
#                +DoW + holiday
#                +station + year_month_id
#                ,
#                data = aqwd, cluster= c("station "), collin.tol = 1e-12)
# etable(feols)
# etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-muntilcrisis_agg.tex"), 
#        label = "tab:FE_y-muntilcrisis_agg", adjustbox = TRUE,
#        replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study (FEs until crisis) ")
# etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-muntilcrisis_agg.tex"), 
#        label = "tab:all_FE_y-muntilcrisis_agg", adjustbox = TRUE,
#        replace = TRUE , title = "FE Event Study: Fes until crisis")

# 
# HOD FE model

remove(feols)
gc()

feols <- feols(PM25 ~ i(HoD, crisis, ref = 1)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |station + HoD 
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd %>% filter(date > ymd("20150101")), fsplit = ~type_of_station, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-m_HoD_fsplit_type.png"),
    width=14, height=10, unit = "cm", res = 400)
iplot(feols, sep = 0.3, main = "Effect on PM25")
legend("bottomright", col = 1:3, pch = 20,
       legend = c("All Stations", "Background", "Traffic"), title = "Type of station")
dev.off()

etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-m_HoD_fsplit_type.tex"), 
       label = "tab:FE_FE_y-m_HoD_fsplittype", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE Model: Effect Hour of Day")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-m_HoD_fsplit_type.tex"), 
       label = "tab:all_FE_y-m_HoD_fsplittype", adjustbox = TRUE,
       replace = TRUE , title = "FE Event Study: Fes until crisis")
# test heterogeneity with iplot
aqwd <- aqwd %>%
  mutate(type_HoD = str_c(type_of_station, as.character(HoD)))

feols <- feols(PM25 ~ i(type_HoD, crisis)
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |station + HoD 
               +DoW + holiday
               + station + Year^Month
               ,
               data = aqwd %>% filter(date > ymd("20150101")), fsplit = ~type_of_station, cluster= c("station "), collin.tol = 1e-12)
etable(feols)

iplot(feols, sep = 0.3)
legend("bottomright", col = 1:3, pch = 20,
       legend = c("All Stations", "Background", "Traffic"), title = "Type of station")

## DID ########
remove(feols)
gc()

feols <- feols(PM25 ~ HDH + crisis:HDH
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + Month^Year +station , # +station^Month^Year,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_HDHcrisis_m-y_fsplitt.tex"), label = "tab:DID_HDHcrisis_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Using Heating Degree as Continous Treatment")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_HDHcrisis_m-y_fsplitt.tex"), label = "tab:all_DID_HDHcrisis_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Using Heating Degree as Continous Treatment")

# Event study DID HDH
remove(feols)
gc()
feols <- feols(PM25 ~ i(timesincecrisis,crisis_HDH, ref = 0) + HDH
               + .[weather_woWRTR]
               + .[weather_woWRTR]^2
               + covid 
               |HoD
               +DoW + holiday
               +station
               + Month^Year,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)

png(filename=paste0(figurepath_scenario1, "/DID_event_HDHcrisis_full.png"),
    width=600, height=350)

iplot(feols[[1]],drop = "12", main = "Effect on PM25")# a function call to save the file
dev.off()

png(filename=paste0(figurepath_scenario1, "/DID_event_HDHcrisis_bg.png"),
    width=600, height=350)

iplot(feols[2],drop = "12", main = "Effect on PM25")# a function call to save the file
dev.off()

png(filename=paste0(figurepath_scenario1, "/DID_event_HDHcrisis_traffic.png"),
    width=600, height=350)

iplot(feols[3],drop = "12", main = "Effect on PM25")# a function call to save the file
dev.off()


etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_event_HDHcrisis_fsplitt.tex") , label = "tab:DID_HDHcrisis_event", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square"), 
       title = "MOnthly Difference in Difference strategy: Continous Treatment based on Heating Degree.")

## DID Resi ######
remove(feols)
gc()

feols <- feols(PM25 ~  crisis:resi_station_2 + crisis + resi_station_2
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + Month^Year +station , # +station^Month^Year,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)
feols[[3]] <- NULL
etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_resi_m-y_fsplitt.tex"), label = "tab:DID_resi_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_resi_m-y_fsplitt.tex"), label = "tab:all_DID_resi_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
# resi station 3
remove(feols)
gc()

feols <- feols(PM25 ~  crisis:resi_station_3 + crisis
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
etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_resi3_m-y_fsplitt.tex"), label = "tab:DID_resi1_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_resi3_m-y_fsplitt.tex"), label = "tab:all_DID_resi1_m-y_fsplitt", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")

remove(feols)
gc()

aqwd <- aqwd %>%
  mutate(resi2_crisis = case_when(resi_station_2 == 1 & crisis == 1 ~ 1, .default = 0))

feols <- feols(PM25 ~  i(Month,resi2_crisis, ref = 2) + crisis + resi_station_2
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
iplot(feols, drop = "^(1{1})$")

etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_event_resi2_y-m.tex"), label = "tab:DID_event_resi2_y-m", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_event_resi2_y-m_.tex"), label = "tab:all_DID_resi2_monthtrend_event", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")


png(filename=paste0(figurepath_scenario1, "/DID_event_scenario1_resi2_y-m.png"),
    width=600, height=350)

iplot(feols, main = "Effect on PM25", drop = "^(1{1})$")# a function call to save the file
dev.off()

aqwd <- aqwd %>%
  mutate(resi2_crisis = case_when(resi_station_2 == 1 & crisis == 1 ~ 1, .default = 0))

# Hod REsi
feols <- feols(PM25 ~  i(HoD,resi2_crisis, ref = 16) + crisis + resi_station_2
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

png(filename=paste0(figurepath_scenario1, "/DID_HoD_scenario1_resi2_y-m.png"),
    width=600, height=350)

iplot(feols, main = "Effect on PM25")# a function call to save the file
dev.off()

etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_HoD_resi2_y-m.tex"), label = "tab:DID_HoD_resi2_y-m", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_HoD_resi2_y-m_.tex"), label = "tab:all_DID_resi2_HoD_event", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")

gc()
feols <- feols(PM25 ~  crisis:resi_station_2 + crisis + resi_station_2
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + Month^Year +station , # +station^Month^Year,
               data = aqwd,cluster= c("station"), collin.tol = 1e-12)
etable(feols)

etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_full_resi2_y-m.tex"), label = "tab:DID_full_resi2_y-m", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_full_resi2_y-m_.tex"), label = "tab:all_DID_resi2_full", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")


# DOW resi
gc()
feols <- feols(PM25 ~  i(DoW,resi2_crisis, ref = "4") + crisis + resi_station_2
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

png(filename=paste0(figurepath_scenario1, "/DID_DoW_scenario1_resi2_y-m.png"),
    width=600, height=350)

iplot(feols, main = "Effect on PM25")# a function call to save the file
dev.off()

etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_DoW_resi2_y-m.tex"), label = "tab:DID_DoW_resi2_y-m", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_DoW_resi2_y-m_.tex"), label = "tab:all_DID_resi2_DoW", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")


gc()
feols <- feols(PM25 ~  i(Month,resi2_crisis, ref = "2") + crisis + resi_station_2
               + ..ctrl + ..ctrl_squared
               + covid + i(Month, HDH)
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD 
               +DoW + holiday
               +station + Year^Month , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12)
etable(feols)

png(filename=paste0(figurepath_scenario1, "/DID_HDH_scenario1_resi2_y-m.png"),
    width=600, height=350)

iplot(feols, main = "Effect on PM25")# a function call to save the file
dev.off()

etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_HDH_resi2_y-m.tex"), label = "tab:DID_HDH_resi2_y-m", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), 
       title = "Diff-in-Diff: Residential Stations as Treatment group")
etable(feols, tex = TRUE, file = paste0(report_scenario1_app, "/all_DID_HDH_resi2_y-m_.tex"), label = "tab:all_DID_resi2_HDH", adjustbox = TRUE,
       replace = TRUE , 
       title = "Diff-in-Diff: Residential Stations as Treatment group")


# EVENt FE fsplit
gc()
feols <- feols(PM25 ~ i(timesincecrisis30_indays, crisis, ref = 0) + resi_station_2
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO
               |HoD
               +DoW + holiday
               +station + Year^Month
               ,
               data = aqwd, fsplit = ~resi_station_2, cluster= c("station "), collin.tol = 1e-12)
etable(feols)
png(filename=paste0(figurepath_scenario1, "/FE_y-dayssincecrisis30_event_ref0_fsplit.png"),
    width=600, height=350)
iplot(feols, main = "Effect on PM25 over time", xlab= "days since beginning of crisis", drop = "-", x.shift = 1)
legend("bottomright", col = 1:3, pch = 20,
       legend = c("All Stations", "Residential Station", "Non-residential"), title = "Type of station")




dev.off()
etable(feols, tex = TRUE, file=paste0(report_scenario1, "/FE_y-dayssincecrisis30_event_ref0_fsplit.tex"), 
       label = "tab:FE_y-dayssincecrisis30_event_ref0_fsplit", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square", "Interactions" = "Pressure|no"), title = "FE event study")
etable(feols, tex = TRUE, file=paste0(report_scenario1_app, "/all_FE_y-dayssincecrisis30_event_ref0_fsplit.tex"), 
       label = "tab:all_FE_y-dayssincecrisis30_event_ref0_fsplit", adjustbox = TRUE,
       replace = TRUE , title = "FE Event Study")

remove(feols)
gc()
feols <- feols(PM25 ~ i(timesincecrisis30_indays,crisis_HDH, ref = 0) + HDH
               + .[weather_woWRTR]
               + .[weather_woWRTR]^2
               + covid 
               |HoD
               +DoW + holiday
               +station
               + Month^Year,
               data = aqwd, fsplit = ~type_of_station, cluster= c("station"), collin.tol = 1e-12)
etable(feols)

png(filename=paste0(figurepath_scenario1, "/DID_event_HDHcrisis_fsplit.png"),
    width=600, height=350)

iplot(feols,drop = "-", main = "Effect on PM25")# a function call to save the file
legend("bottomright", col = 1:3, pch = 20,
       legend = c("All Stations", "Background", "Traffic"), title = "Type of station")

dev.off()


etable(feols, tex = TRUE, file = paste0(report_scenario1, "/DID_event_HDHcrisis_fsplitt.tex") , label = "tab:DID_HDHcrisis_event", adjustbox = TRUE,
       replace = TRUE , group = list("Weather" = "\\]$","Weather_{squared}" = "square"), 
       title = "MOnthly Difference in Difference strategy: Continous Treatment based on Heating Degree.")
