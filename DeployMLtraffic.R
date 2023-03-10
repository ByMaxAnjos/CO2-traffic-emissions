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
#' @return CO2 emissions: csv. table, plots and sf multipolylines and raster map with 100 meters of resolution.
#' @examples
#=======================================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, sf, openair,
               osmdata, tmap, recipes, timetk, caret, ranger, doParallel)

library(tidyverse)
library(data.table)
library(sf)
library(httr)
library(openair)
library(osmdata)
library(tmap)
library(recipes)
library(timeDate)
library(timetk)
library(ranger)
library(caret)
library(viridis)

#Define your directory

setwd("~/Documents/CO2CityMap/Berlin/Components/Transport")
source("Rcode/ZCCM_functions.R")


traffic <- fread("inputs/data/traffic_berlin_2022_08_09.csv") %>%
  dplyr::select(date, id, kfz, speed_kfz) %>%
  rename(icars= kfz, ispeed = speed_kfz)
stations <- sf::read_sf("inputs/shps/stations_ber.shp") %>%
  janitor::clean_names() %>%
  #dplyr::select(l_u_fffd_nge_w, breite_wg, det_id15, geometry) %>% #windows
  dplyr::select(l_nge_w, breite_wg, det_id15, geometry) %>%
  set_names("Latitude","Longitude","id","geometry") %>%
  st_as_sf() %>% # convert to sf object
  st_transform(crs = 4326)
weather <- fread("inputs/data/weather_berlin_2015_2022_10.csv")
var1 <- sf::read_sf("inputs/shps/uso_real_2020.shp") %>%
  dplyr::select(woz, geometry) %>%
  rename(landuseAtlas = woz) %>%
  mutate(landuseAtlas = factor(landuseAtlas)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)


#Get OSM data
#ROI
city <- "Aracaju"
my_area <- osmdata::getbb(city, format_out = "sf_polygon", limit = 1)

my_area <- osmdata::getbb(city, format_out = "sf_polygon", limit = 1)$multipolygon

my_area <- st_make_valid(my_area)

qtm(my_area)


#Get aggregated GIS features roads
#Define the road classes
class_roads <- c("motorway","trunk","primary", "secondary", "tertiary")
#Deploy the function
iNetRoad <- getOSMfeatures(city = "Aracaju",
                           road_class = class_roads,
                           city_area = my_area,
                           ishp = TRUE,
                           iplot = TRUE)

iNetRoad <- sf::read_sf("inputs/shps/road_osm_feat.shp") %>%
  subset(fclass != "residential") %>%
  subset(fclass != "trunk") %>%
  mutate_if(is.integer, as.factor) %>% st_as_sf() %>% st_transform(crs = 4326)

#Join with var1, var2, var3 .....
GIS_road <- st_join(iNetRoad, var1, join =st_nearest_feature, left = FALSE)
#GIS_road <- st_join(GIS_road, var2, st_nearest_feature, st_is_within_distance, dist = 0.1)
#GIS_road <- st_join(GIS_road, var3, st_nearest_feature, st_is_within_distance, dist = 0.1)


#define the period (inputDates)
#month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
imonth <- c(9)
iyear <- c(2022)
input <- expand.grid(imonth, iyear)




CO2trafficMax(input, city = "Berlin", traffic_data = traffic, stations_data = stations,
              weather_data = weather, road_data = GIS_road,
              cityCount = TRUE, cityMap = TRUE, tempRes = "day", spatRes = 100, iunit = "grams", ista = "sum")

myMLtraffic <- apply(input, 1, CO2trafficMax)

CO2_roads <- do.call(rbind.data.frame, myMLtraffic)
write_csv(CO2_roads, "/Users/co2map/Documents/table_job_CO2E_Berlin_sep2022_id.csv")
