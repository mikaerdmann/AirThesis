library(tidyverse)
library(dplyr)
library(tidyr)
library(fixest)
processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
report_fig <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/figures"
## Prep #########
processedpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/processed"
stationpath <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/Station_Data"
report_scenario1 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/scenario1"
figurepath_scenario1 <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/figures/scenario1"
report_scenario1_app <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/report/Overlead/tables/models/scenario1/app"

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
weather_month <- str_c(weather_woWRTR, "month", sep = "_")

setFixest_fml(..ctrl = ~ .[weather_woWRTR], ..ctrl_squared = ~.[weather_woWRTR]^2, ..ctrl_month = ~.[weather_month], reset = TRUE)

aqwd <- readRDS(paste0(processedpath, "/model_final_ready.rds"))


# Assuming your dataset is named 'air_quality_data' with columns 'station', 'timestamp', and 'air_quality_value'

# Extract the month and year from the timestamp
aqwd$Y_M <- format(aqwd$date, "%Y-%m")

# make monthly distinctions per residential and non-residential
monthly_type <- aqwd%>%
  group_by(resi_station_2, Y_M, bundesland) %>%
  summarise(avg_air_quality = mean(PM10, na.rm = TRUE))%>%
  mutate(date = ymd(paste0(Y_M, "-01"))) %>%
  mutate(resi_stat = case_when(resi_station_2 ==1 ~ "residential station", .default = "non-residential station"))

# Create the plot using ggplot2
ggplot(monthly_type) +
  #geom_point(aes(x = date, y = avg_air_quality,  color = "grey"))+
  geom_smooth(aes(x = date, y = avg_air_quality,  group = resi_stat, color = resi_stat)) + 
  labs(title = "Monthly Air Quality Averages Over Time",
       x = "Time",
       y = "Average Air Quality") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2022-02-01"))+
  geom_text(aes(x=as.Date("2022-02-01"), label="\nEnergy Crisis", y=24), colour="black", angle=90, text=element_text(size=11))+
  #annotate("text", x = as.Date("2022-02-01"), y = 24, "Crisis Beginning") +
  scale_x_date(date_labels = "%Y", breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Station class")) 

ggsave(paste0(report_fig,"/monthly_resi_station_air_quality_trends.png"), width = 9, height = 5, units = "in")

# Predict
gc()
feols <- feols(PM10 ~  crisis:resi_station_2 + crisis
               + ..ctrl + ..ctrl_squared
               + covid 
               + no_rain:P +no_rain:cold_day +
                 cold_day:P+ low_wind:P+
                 TT_TU:RF_TU + TT_TU:SD_SO 
               |HoD
               +DoW + holiday
               + Month^Year +station , # +station^Month^Year,
               data = aqwd, cluster= c("station"), collin.tol = 1e-12, combine.quick = FALSE)

# create to predict data


aqwd_topred_resi <- aqwd %>%
  select(all_of(c(weather_woWRTR, "PM10","datetime", "station", "covid", 
                  "no_rain", "low_wind", "cold_day", "HoD", "DoW", "Year",
                  "Month", "holiday", "GDP","resi_station_2", "crisis", "date"))) %>%
  drop_na()

# predict

predicted_resi <- predict(feols, aqwd_topred_resi)

aqwd_topred_resi$pred <- predicted_resi
# plot
aqwd_topred_resi$Y_M <- format(aqwd_topred_resi$date, "%Y-%m")
aqwd$Y_M <- format(aqwd$date, "%Y-%m")


# plot points

monthly_type_one <- aqwd%>%
  group_by(Y_M, resi_station_2) %>%
  summarise(values = mean(PM10, na.rm = TRUE))%>%
  mutate(type = "data") %>%
  mutate(date = ymd(paste0(Y_M, "-01"))) %>%
  mutate(resi_stat = case_when(resi_station_2 ==1 ~ "residential station", 
                               .default = "non-residential station"))

monthly_pred_one <- aqwd_topred_resi%>%
  group_by(Y_M, resi_station_2) %>%
  summarise(values = mean(pred, na.rm = TRUE))%>%
  mutate(date = ymd(paste0(Y_M, "-01"))) %>%
  mutate(type = "prediction") %>%
  mutate(resi_stat = case_when(resi_station_2 == 1~ "residential station", 
                              .default = "non-residential station"))

monthly_all <- bind_rows(monthly_pred_one, monthly_type_one)
# Create the plot using ggplot2
ggplot(monthly_all) +
  #geom_point(aes(x = date, y = avg_air_quality,  color = "grey"))+
  geom_point(aes(x = date, y = values,  group = type, color =type),size = 0.5)+ 
  geom_point(aes(x = date, y = values,  group = type, color = type), size = 0.5)+
  facet_grid(rows = vars(resi_stat)) +
  labs(title = "Monthly Air Quality Averages Over Time",
       x = "Time",
       y = "Average Air Quality") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2022-02-01"))+
  geom_text(aes(x=as.Date("2022-02-01"), label="\nEnergy Crisis", y=24), colour="black", angle=90, text=element_text(size=11))+
  #annotate("text", x = as.Date("2022-02-01"), y = 24, "Crisis Beginning") +
  scale_x_date(date_labels = "%Y", breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Station class"))
ggsave(paste0(report_fig,"/monthly_predictionvsdata.png"), width = 9, height = 5, units = "in")


# Predict counterfactual for resi stations and compare on the daily level

aqwd_topred_resi <- aqwd %>%
  select(all_of(c(weather_woWRTR, "PM10","datetime", "station", "covid", 
                  "no_rain", "low_wind", "cold_day", "HoD", "DoW", "Year",
                  "Month", "holiday", "GDP","resi_station_2", "crisis", "daily_trend", "date"))) %>%
  drop_na() %>%
  mutate(crisis = 0)

predicted_resi <- predict(feols, aqwd_topred_resi)

aqwd_topred_resi$pred <- predicted_resi


# plot daily
daily_data <- aqwd%>%
  filter(ymd(date) > ymd("20210201")) %>%
  group_by(date, resi_station_2) %>%
  summarise(values = mean(PM10, na.rm = TRUE,))%>%
  mutate(type = "data") %>%
  mutate(resi_stat = case_when(resi_station_2 ==1 ~ "residential \n station", 
                               .default = "non-residential \n station"))

daily_pred<- aqwd_topred_resi%>%
  filter(ymd(date) > ymd("20210201")) %>%
  group_by(date, resi_station_2) %>%
  summarise(values = mean(pred, na.rm = TRUE))%>%
  mutate(type = "counterfactual") %>%
  mutate(resi_stat = case_when(resi_station_2 == 1~ "residential \n station", 
                               .default = "non-residential \n station"))

daily_all <- bind_rows(daily_pred, daily_data)
ggplot(daily_all) +
  #geom_point(aes(x = date, y = avg_air_quality,  color = "grey"))+
  geom_smooth(aes(x = date, y = values,  group = type, color =type),size = 0.5)+ 
  geom_smooth(aes(x = date, y = values,  group = type, color = type), size = 0.5)+
  facet_grid(rows = vars(resi_stat)) +
  labs(title = "Daily Air Quality Averages Over Time",
       x = "Time",
       y = "Average Air Quality") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2022-02-01"))+
  geom_text(aes(x=as.Date("2022-02-01"), label="\nEnergy Crisis", y=20), colour="black", angle=90)+
  #annotate("text", x = as.Date("2022-02-01"), y = 24, "Crisis Beginning") +
  #scale_x_date(date_labels = "%Y", breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Station class"))
ggsave(paste0(report_fig,"/daily_predictionvsdata_counterfactual.png"), width = 9, height = 5, units = "in")

# plot daily averages resi vs. non-resi
daily_type <- aqwd%>%
  group_by(resi_station_2, date) %>%
  summarise(avg_air_quality = mean(PM10, na.rm = TRUE))%>%
  mutate(resi_stat = case_when(resi_station_2 ==1 ~ "residential station", .default = "non-residential station"))

# Create the plot using ggplot2
ggplot(daily_type) +
  #geom_point(aes(x = date, y = avg_air_quality,  color = "grey"))+
  geom_point(aes(x = date, y = avg_air_quality,  group = resi_stat, color = resi_stat), size = 0.25) + 
  geom_smooth(aes(x = date, y = avg_air_quality,  group = resi_stat, color = resi_stat), size = 0.5) + 
  labs(title = "Monthly Air Quality Averages Over Time",
       x = "Time",
       y = "Daily Average PM") +
  geom_vline(xintercept = as.Date("2022-02-01"))+
  geom_text(aes(x=as.Date("2022-02-01"), label="\nEnergy Crisis", y=78), colour="black", angle=90, text=element_text(size=11))+
  #annotate("text", x = as.Date("2022-02-01"), y = 24, "Crisis Beginning") +
  scale_x_date(date_labels = "%Y", breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Station class")) 

ggsave(paste0(report_fig,"/daily_resi_station_air_quality_trends.png"), width = 9, height = 5, units = "in")

# only > 2020
ggplot(daily_type %>% filter(date > ymd("20200101"))) +
  #geom_point(aes(x = date, y = avg_air_quality,  color = "grey"))+
  #g#eom_point(aes(x = date, y = avg_air_quality,  group = resi_stat, color = resi_stat), size = 0.5) + 
  geom_line(aes(x = date, y = avg_air_quality,  group = resi_stat, color = resi_stat), linewidth = 0.3) + 
  labs(title = "Monthly Air Quality Averages Over Time",
       x = "Time",
       y = "Daily Average PM") +
  geom_vline(xintercept = as.Date("2022-02-01"))+
  geom_text(aes(x=as.Date("2022-02-01"), label="\nEnergy Crisis", y=55), colour="black", angle=90, size = 3)+
  #annotate("text", x = as.Date("2022-02-01"), y = 24, "Crisis Beginning") +
  scale_x_date(date_labels = "%Y", breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Station class")) 

ggsave(paste0(report_fig,"/daily_resi_station_air_quality_line_2020-2022.png"), width = 9, height = 5, units = "in")


