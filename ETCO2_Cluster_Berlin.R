#=======================================================================================
#
# Title:       High Spatio-temporal Traffic (car flux and speed) and CO2 Emissions Predictions using Machine Learning.
# Author:      Dr. Max Anjos (maxanjos@campus.ul.pt)
# Description: More details on the approach are available at:
#             https://github.com/ByMaxAnjos/CO2-traffic-emissions
# Data: 09.10.2022
#
#' @param traffic (required). Local traffic data. I must have the following columns named as date, mycars, myspeed, and id. The id means identifier that links traffic data with the count traffic stations.
#' @param traffic_stations (required). Traffic point count stations with latitude, longitude and ID. It could be at shapefile or csv format.
#' @param GIS_features (conditionally required). Landuse, density population and traffic volume in 2019 maps from the Berlin Urban Atlas (fisbroker) site.
#' @param networ_Tomtom (optional). Shapefile of the raod network from the TomTom.
#' @param weather (conditionally required) Hourly meteorological data from the German Weather Service (DWD package).
#' @return CO2 emmissions, csv. table, plotos and sf multipolylines and raster map with 100 meters of resolution.
#' @examples
#=======================================================================================

#Define your directory
setwd("/local_data/anjos/CO2_traffic_emissions")
source("/local_data/anjos/CO2_traffic_emissions/emission_cluster_functions.R")

#Increasing the memory before the calculation (windows)
#mymemory <- memory.limit() * 3
#memory.limit(size = mymemory)

library(lubridate)
library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(openair)
library(caret)
library(recipes)
library(timetk)
library(ranger)

#=================================================================
# Load data and pre-processing
#=================================================================
#Read traffic data file
traffic_data <- fread("Components/Transport/inputs/data/traffic_berlin_2015_2022_07.csv") %>%
  dplyr::select(date, id, kfz, speed_kfz)
traffic_data$id <- as.factor(traffic_data$id)

#Read weather data file
weather <- fread("Components/Transport/inputs/data/weather_berlin_2015_2022_10.csv")

#Read stations ESRI shapefile
stations <- sf::read_sf("Components/Transport/inputs/shps/stations_berlin.shp") %>%
  st_as_sf() %>% # convert to sf object
  st_transform(crs = 4326)

#Read road network ESRI shapefile from TomTom
network_gis <- sf::read_sf("Components/Transport/inputs/shps/network_berlin.shp") %>%
  subset(FRC != "0") %>%
  subset(FRC != "6") %>%
  subset(FRC != "7") %>%
  dplyr::select(Id, StreetName, FRC, Length, SpeedLimit, geometry) %>%
  rename(length = Length) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#Read density population ESRI shapefile from Berlin Atlas
pop_gis <- sf::read_sf("Components/Transport/inputs/shps/pop_2020.shp") %>%
  dplyr::select(ew2020, ew_ha_2020, geometry) %>%
  rename(resident = ew2020, resident_hac = ew_ha_2020) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#Read land use ESRI shapefile from Berlin Atlas
land_gis <- sf::read_sf("Components/Transport/inputs/shps/uso_real_2020.shp") %>%
  dplyr::select(woz, geometry) %>%
  rename(landuseAtlas = woz) %>%
  mutate(landuseAtlas = factor(landuseAtlas)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#Read daily traffic ESRI shapefile from Berlin Atlas
vtm_map19 <- sf::read_sf("Components/Transport/inputs/shps/atlastraffic_ber_2019.shp") %>%
  rename(dtvw_kfz19 = dtvw_kfz) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#Read road_OSM
road_OSM <- sf::read_sf("Components/Transport/inputs/shps/road_osm_feat.shp") %>%
  subset(fclass != "residential") %>%
  subset(fclass != "trunk") %>%
  mutate_if(is.integer, as.factor) %>% st_as_sf() %>% st_transform(crs = 4326)

#=================================================================
# Classify the roads as sampled, nonsampled and motorway
#=================================================================

#Separate sampled road and non-sampled road based on stations counting
road_sampled <- st_join(stations, network_gis, join = st_is_within_distance, dist = 20, left = FALSE) %>%
  as.data.frame() %>% dplyr::select(Id) %>% inner_join(network_gis, by = "Id") %>%
  #distinct(Id, .keep_all = TRUE) %>%
  mutate(category = "sampled") %>% st_as_sf() %>% st_transform(crs = 4326)

road_nonsampled <- network_gis[!network_gis$Id%in%road_sampled$Id,]
road_nonsampled <- mutate(road_nonsampled, category = "nonsampled")
roads_mod <- bind_rows(road_sampled, road_nonsampled)
rm(road_sampled, road_nonsampled, network_gis)

#Joined road OSM with network from the TomTom
GIS_road <- st_join(roads_mod, road_OSM, join =st_nearest_feature, left = FALSE)
GIS_road <- st_join(GIS_road, vtm_map19, st_nearest_feature, st_is_within_distance, dist = 0.1)
GIS_road  <- st_join(GIS_road , pop_gis, st_nearest_feature, st_is_within_distance, dist = 0.1)
GIS_road  <- st_join(GIS_road, land_gis, st_nearest_feature, st_is_within_distance, dist = 0.1)

#Clean the variables
GIS_road <- GIS_road %>%
  dplyr::select(Id, StreetName, FRC, length.x, SpeedLimit, category, fclass, lanes, maxspeed, amenity, leisure, landuse, place,
                natural, shop, dtvw_kfz19, resident, resident_hac, landuseAtlas, geometry) %>%
  rename(length = length.x)

#Split up sampled roads and non-sampled roads. Use only the sample roads to build ML model: training and test with 70/30%
GIS_road_sampled <- GIS_road[GIS_road$category=='sampled',]
GIS_road_nonsampled <- GIS_road[GIS_road$category=='nonsampled',]
GIS_road_sampled$FRC <- as.factor(GIS_road_sampled$FRC)

#IMPORTANT:motorway
network_motor <- sf::read_sf("Components/Transport/inputs/shps/network_berlin.shp") %>%
  filter(FRC == "0") %>%
  mutate(category = "motorway") %>%
  dplyr::select(Id, StreetName, FRC, category, Length, SpeedLimit, geometry) %>%
  rename(length = Length) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

motorway_road <- network_motor %>%
  st_join(road_OSM %>%  filter(fclass == "motorway")) %>%
  st_join(vtm_map19) %>% st_join(pop_gis, st_nearest_feature) %>% st_join(land_gis, st_nearest_feature) %>%
  dplyr::select(Id, StreetName, FRC, length.x, SpeedLimit, category, fclass, lanes, maxspeed, amenity, leisure, landuse, place,
                natural, shop, dtvw_kfz19, resident, resident_hac, landuseAtlas, geometry) %>%
  rename(length = length.x)

#=====================================================================
# Train the ML algorithm with GIS stations and Calculate CO2 emissions
#=====================================================================

#Define the city
#"Berlin"

#Define temporal resolution (tempRes)
#"hourly", "daily", "weekly", "monthly", "seasonal", "annual".

#Define spatial resolution (spatRes)
#"cityMap" -> 100-meter raster emissions and shapefile
#"cityCount" -> Calculates total-wise City Emission. E.g., timeserie.csv

#Output units of the model
# "micro" -> CO2 emissions [micro mole per meter square per second]
#"grams" -> CO2 emissions [grams per meter]
#"gramsCarbon" -> Carbon emissions [grams per meter]

#define the period (inputDates)
#month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month <- c("jan")
year <- c(2020)
input <- expand.grid(month, year)


#Train the ML models for selected period
MLtrafTrain <- function(input, city="Berlin", traffic = traffic_data, tempRes ="hourly", spatRes = "cityCount",  unit ="grams") {

  #I am run following the temporal resolution:
  if (tempRes == "hourly") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic, year = year, month = month, hour = 0:23)

  }

  if (tempRes == "daily") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic, year = year, month = month, hour = 0:23) %>%
      openair::timeAverage(avg.time = "day")

  }

  if (tempRes == "weekly") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic, year = year, month = month, hour = 0:23) %>%
      openair::timeAverage(avg.time = "week")

  }

  if (tempRes == "monthly") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic, year = year, month = month, hour = 0:23) %>%
      openair::timeAverage(avg.time = "month")

  }

  if (tempRes == "seasonal") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic_df, year = year, month = month, hour = 0:23) %>%
      openair::timeAverage(avg.time = "season")

  }

  if (tempRes == "annual") {
    month <- input[1]
    year <- input[2]
    traffic_mod <- openair::selectByDate(traffic_df, year = year, month = month, hour = 0:23) %>%
      openair::timeAverage(avg.time = "year")

  }

  #Join point counting stations
  stations_mod <- st_join(stations, GIS_road_sampled,
                            join = st_is_within_distance, dist = 30, left = FALSE)

  #Join traffic timeseries to stations by using id
  traffic_mod$id <- as.factor(traffic_mod$id)
  stations_mod$id <- as.factor(stations_mod$id)

  traffic_join <- inner_join(traffic_mod, stations_mod, by ="id") # merge with stations

  traffic_join$kfz <- as.numeric(traffic_join$kfz)
  traffic_join$speed_kfz <- as.numeric(traffic_join$speed_kfz)

  features <- traffic_join %>%
    group_by(date, Id) %>%
    summarise(mean_kfz = mean(kfz),
              mean_speedkfz = mean(speed_kfz), .groups = "drop") %>%
    filter(mean_kfz>30, mean_speedkfz >10) %>%
    inner_join(GIS_road_sampled, by= "Id") %>%
    inner_join(weather, by= "date") %>% #Join weather data
    as_data_frame() %>% dplyr::select(-Id, -StreetName, category, -geometry) %>% #Select features
    mutate_if(is.character, as.factor)
  features$SpeedLimit <- as.factor(features$SpeedLimit)

  # Timeseries for ML using the idx_date and signature
  receipe_steps <-
    recipe(mean_kfz + mean_speedkfz ~., data = features) %>% # Depend variable selected
    step_rm(category) %>%
    step_ts_impute(all_numeric_predictors()) %>% #Impute values for numeric predictors and outcomes
    step_impute_mode(SpeedLimit, maxspeed, lanes, landuseAtlas) %>% #Impute values for nominal/categorical variables
    step_unknown(all_nominal_predictors()) %>%
    step_other(all_nominal_predictors(), -FRC, -SpeedLimit, -maxspeed, -lanes, -landuseAtlas) %>%
    #step_normalize(all_numeric_predictors()) %>% # Normalize numeric data to have a standard deviation of one and a mean of zero.
    step_holiday(date, holidays = timeDate::listHolidays("DE")) %>%
    #step_nzv(all_numeric_predictors()) %>% # remove variables that are highly sparse and unbalanced.
    step_timeseries_signature(date) %>% # creating indexes from date-time
    step_rm(date)
  train_recipe <- receipe_steps %>%
    prep(features) %>%
    bake(features)

  #select CARS for prediction
  train_processed <- train_recipe %>%
    dplyr::select(-mean_speedkfz)

  #ML-ranger
  MLcars <- ranger(mean_kfz ~ ., data = train_processed, num.trees = 100)

  #ranger speed
  train_processed <- train_recipe %>%
    dplyr::select(-mean_kfz)

  MLspeed <- ranger(mean_speedkfz ~ ., data = train_processed, num.trees = 100)

  #Daily-Hourly basis
  iday <- traffic_mod %>%
    mutate(day = lubridate::day(date)) %>%
    distinct(day, .keep_all = FALSE) %>%
    expand.grid()

  myCO2day <- function(iday) {

    myday <- iday[1]

    traffic_day <- traffic_mod %>%
      mutate(day = lubridate::day(date)) %>%
      openair::selectByDate(day = myday,  hour = 0:23)
    traffic_day$id <- as.factor(traffic_day$id)

    #Deploying to Non-sampled roads
    date_cross <-  crossing(stuff = GIS_road_nonsampled$Id, traffic_day$date) %>%
      set_names("Id", "date")
    setDT(date_cross)

    model_join_nonsampled <- inner_join(date_cross, GIS_road_nonsampled, by="Id", all.x = TRUE, all.y = FALSE)
    model_join_nonsampled <- inner_join(model_join_nonsampled, weather, by="date", all.x = TRUE, all.y = FALSE)
    rm(date_cross)
    features_nonsampled <- model_join_nonsampled %>%
      dplyr::select(-geometry) %>%
      mutate_if(is.character, as.factor)
    features_nonsampled$FRC <- as.factor(features_nonsampled$FRC)
    features_nonsampled$SpeedLimit <- as.factor(features_nonsampled$SpeedLimit)

    receipe_steps <-
      recipe(date ~., data = features_nonsampled) %>% # Depend variable selected
      step_rm(Id, StreetName, category) %>%
      step_ts_impute(all_numeric_predictors()) %>% #Impute values for numeric predictors and outcomes
      step_impute_mode(SpeedLimit, maxspeed, lanes, landuseAtlas) %>% #Impute values for nominal/categorical variables
      step_unknown(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), -FRC, -SpeedLimit, -maxspeed, -lanes, -landuseAtlas) %>%
      step_holiday(date, holidays = timeDate::listHolidays("DE")) %>%
      step_timeseries_signature(date) %>% # creating indexes from date-time
      step_rm(date)
    dataset_processed <- receipe_steps %>%
      prep(features_nonsampled) %>%
      bake(features_nonsampled)

    options(digits = 1)
    model_pred_cars <- predict(MLcars, data=dataset_processed) #Predicting the cars
    cars_pred <- model_pred_cars$predictions %>%
      as_tibble() %>% bind_cols(model_join_nonsampled %>% dplyr::select(date, Id, StreetName, category)) %>%
      rename(cars=value)

    options(digits = 1)
    model_pred_speed <- predict(MLspeed, data=dataset_processed)   #Predicting the speed
    speed_pred <- model_pred_speed$predictions %>%
      as_tibble() %>% bind_cols(dataset_processed) %>%
      rename(speed=value)

    traffic_pred <- cars_pred %>% #Join cars and speed predication
      bind_cols(speed_pred)

    options(digits = 1)
    traffic_CO2_nonsampled <- ECO2traffic(traffic_pred, coeffs) %>%   #Calculate CO2 emissions
      mutate(cars_nonsampled = cars,
             speed_nonsampled = speed,
             ECO2_gmh_nonsampled = ECO2_gmh,
             ECO2_micro_nonsampled = ECO2_micro,
             EC_gmh_nonsampled = EC_gmh) %>%
      as_tibble()
    traffic_CO2_nonsampled[, order(names(traffic_CO2_nonsampled))]

    #Calculate CO2 for sampled roads
    model_join_sampled <- inner_join(traffic_day, stations_mod, by = "id")

    features_sampled <- model_join_sampled %>%
      group_by(date, Id) %>%
      summarise(cars = mean(kfz),
                speed = mean(speed_kfz), .groups = "drop") %>%
      inner_join(GIS_road_sampled, by= "Id") %>%
      distinct(Id, cars, .keep_all = TRUE) %>%
      inner_join(weather, by= "date") %>%
      as_tibble() %>% dplyr::select(-geometry) %>%
      mutate_if(is.character, as.factor)
    features_sampled$FRC <- as.factor(features_sampled$FRC)
    features_sampled$SpeedLimit <- as.factor(features_sampled$SpeedLimit)

    receipe_steps <-
      recipe(date ~., data = features_sampled) %>% # Depend variable selected
      step_rm(Id, StreetName, category) %>%
      step_ts_impute(all_numeric_predictors()) %>% #Impute values for numeric predictors and outcomes
      step_impute_mode(SpeedLimit, maxspeed, lanes, landuseAtlas) %>% #Impute values for nominal/categorical variables
      step_unknown(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), -FRC, -SpeedLimit, -maxspeed, -lanes, -landuseAtlas) %>%
      step_holiday(date, holidays = timeDate::listHolidays("DE")) %>%
      step_timeseries_signature(date) # creating indexes from date-time
    dataset_processed <- receipe_steps %>%
      prep(features_sampled) %>%
      bake(features_sampled)

    traffic_CO2_sampled <- ECO2traffic(dataset_processed, coeffs) %>%
      bind_cols(features_sampled %>% dplyr::select(Id, StreetName, category)) %>%
      mutate(cars_sampled = cars,
             speed_sampled = speed,
             ECO2_gmh_sampled = ECO2_gmh,
             ECO2_micro_sampled = ECO2_micro,
             EC_gmh_sampled = EC_gmh)
    traffic_CO2_sampled[, order(names(traffic_CO2_sampled))]


    #Include the motorway
    date_cross <-  crossing(stuff = motorway_road$Id, traffic_day$date) %>%
      set_names("Id", "date")
    setDT(date_cross)

    model_join_motorway <- inner_join(date_cross, motorway_road, by="Id", all.x = TRUE, all.y = FALSE)
    model_join_motorway <- inner_join(model_join_motorway, weather, by="date", all.x = TRUE, all.y = FALSE)
    rm(date_cross)

    features_motorway <- model_join_motorway %>%
      mutate(cars= dtvw_kfz19/24, speed = SpeedLimit) %>%
      as_tibble() %>% dplyr::select(-geometry) %>%
      mutate_if(is.character, as.factor)
    features_motorway$FRC <- as.factor(features_motorway$FRC)
    features_motorway$SpeedLimit <- as.factor(features_motorway$SpeedLimit)

    receipe_steps <-
      recipe(date ~., data = features_motorway) %>% #
      step_rm(Id, StreetName, category) %>%
      step_ts_impute(all_numeric_predictors()) %>% #Impute values for numeric predictors and outcomes
      step_impute_mode(SpeedLimit, maxspeed, lanes, landuseAtlas) %>% #Impute values for nominal/categorical variables
      step_unknown(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), -FRC, -SpeedLimit, -maxspeed, -lanes, -landuseAtlas) %>%
      step_holiday(date, holidays = timeDate::listHolidays("DE")) %>%
      step_timeseries_signature(date) # creating indexes from date-time

    dataset_processed <- receipe_steps %>%
      prep(features_motorway) %>%
      bake(features_motorway)

    traffic_CO2_motor <- ECO2traffic(dataset_processed, coeffs) %>%
      bind_cols(features_motorway %>% dplyr::select(Id, StreetName, category)) %>%
      mutate(cars_motorway = cars,
             speed_motorway = speed,
             ECO2_gmh_motorway = ECO2_gmh,
             ECO2_micro_motorway = ECO2_micro,
             EC_gmh_motorway = EC_gmh)
    traffic_CO2_motor[, order(names(traffic_CO2_motor))]

    if (spatRes == "cityCount") {
      #Merge CO2 sampled, nonsampled and motorway

      CO2_sampled <-  dplyr::select(traffic_CO2_sampled, date, starts_with(c("cars_", "ECO2_gmh_", "ECO2_micro_", "EC_gmh_"))) %>%
        openair::timeAverage(avg.time = "hour", statistic = "sum")
      CO2_nonsampled <-  dplyr::select(traffic_CO2_nonsampled, date, starts_with(c("cars_", "ECO2_gmh_", "ECO2_micro_", "EC_gmh_"))) %>%
        openair::timeAverage(avg.time = "hour", statistic = "sum")
      CO2_motorway <-  dplyr::select(traffic_CO2_motor, date, starts_with(c("cars_", "ECO2_gmh_", "ECO2_micro_", "EC_gmh_"))) %>%
        openair::timeAverage(avg.time = "hour", statistic = "sum")

      options(digits = 0)
      trafficCO2cityCount <- CO2_sampled %>%
        inner_join(CO2_nonsampled, by= "date") %>%
        inner_join(CO2_motorway, by= "date")  %>%
        mutate(cars_total = cars_sampled + cars_nonsampled,cars_motorway,
                                     ECO2_gmh_total = ECO2_gmh_sampled + ECO2_gmh_nonsampled + ECO2_gmh_motorway,
                                     ECO2_micro_total = ECO2_micro_sampled + ECO2_micro_nonsampled + ECO2_micro_motorway,
                                     EC_gmh_total = EC_gmh_sampled + EC_gmh_nonsampled + EC_gmh_motorway)

      trafficCO2cityCount$date <- as.POSIXct(trafficCO2cityCount$date, format = "%Y-%m-%d %H:%M:%S")
      write.csv(trafficCO2cityCount, paste0("Components/Transport/output/table/",year,month,myday,"ETCO2.csv"))

    }

    if (spatRes == "cityMap") {

      #Unit for calculations
      if (unit == "grams") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled, traffic_CO2_motor) %>%
          dplyr::select(Id, date, ECO2_gmh) %>%
          group_by(Id, date) %>%
          summarise(ECO2_gmh = sum(ECO2_gmh), .groups = "drop") %>%
          pivot_wider(id_cols = Id, names_from = date, values_from = ECO2_gmh)
      }

      if (unit == "micro") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled, traffic_CO2_motor) %>%
          dplyr::select(Id, date, ECO2_micro) %>%
          group_by(Id, date) %>%
          summarise(ECO2_micro = sum(ECO2_micro), .groups = "drop") %>%
          pivot_wider(id_cols = Id, names_from = date, values_from = ECO2_micro)
      }

      if (unit == "gramsCarbon") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled, traffic_CO2_motor) %>%
          dplyr::select(Id, date, EC_gmh) %>%
          group_by(Id, date) %>%
          summarise(EC_gmh = sum(EC_gmh), .groups = "drop") %>%
          pivot_wider(id_cols = Id, names_from = date, values_from = EC_gmh)
      }

      getgeometry <- bind_rows(GIS_road_sampled %>% dplyr::select(Id, geometry)) %>%
        bind_rows(GIS_road_nonsampled %>% dplyr::select(Id, geometry)) %>%
        bind_rows(motorway_road %>% dplyr::select(Id, geometry))

      ECO2T_cal <- pivot_col %>%
        merge(getgeometry, by= "Id") %>%
        #dplyr::select(-StreetName) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = 4326)

      #st_write(mynames, paste0("output1/traffic/maps/",year,month,"ETCO2.shp"), driver = "ESRI Shapefile")
      #Aggregate with GRID
      #ECO2T_ras <-raster::aggregate(ECO2T_cal, ROTH_grid, FUN=mean)

      #Rasterize as stars
      ECO2T_ras <- pbapply::pblapply(2:(ncol(ECO2T_cal)-1), function(i)
        stars::st_rasterize(ECO2T_cal [, i]))
      #Convert to data.frame
      ECO2T_ras <- pbapply::pblapply(1:length(ECO2T_ras), function(i)
        as.data.frame(ECO2T_ras [[i]]))
      #Convert to raster format
      ECO2T_ras <-  pbapply::pbsapply(1:length(ECO2T_ras), FUN=function(i)
        raster::rasterFromXYZ(xyz = ECO2T_ras[[i]], crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      trafficCO2Map <- raster::stack(ECO2T_ras)
      raster::writeRaster(trafficCO2Map,paste0("Components/Transport/output/maps/",year,month,myday, "ETCO2.TIF"), format="GTiff", overwrite = TRUE)

    }

    return(trafficCO2cityCount)
  }

  mydays <- pbapply::pbapply(iday, 1, myCO2day)
  CO2Traffic_month <- do.call(rbind.data.frame, mydays)
  write.csv(CO2Traffic_month, paste0("Components/Transport/output/table/",year,month,"ETCO2.csv"))
  return(CO2Traffic_month)

}

myMLtraffic <- apply(input, 1, MLtrafTrain)
CO2Traffic_cal <- do.call(rbind.data.frame, myMLtraffic)

