#High Spatio-temporal Traffic (car flux and speed) and CO2 Emissions Prediction using Machine Learning

#' @param traffic (required). Local traffic data (eg., csv file with hourly number of vehciles, average speed, and ID of the count traffic stations)
#' @param traffic_stations (required). Traffic point count stations with latitude, longitude and ID. It could be at shapefile or csv format.
#' @param GIS_features (conditionally required).
#' @param weather (optional) Hourly meteorological data from the German Weather Service (DWD package).
#' @return sf multipolylines and table csv
#' @examples
#=================================================================
# Load packages and provide some information
#=================================================================
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
library(doParallel)

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
icity <- "Berlin"
my_area <- osmdata::getbb(icity, format_out = "sf_polygon", limit = 1)

my_area <- osmdata::getbb(icity, format_out = "sf_polygon", limit = 1)$multipolygon

my_area <- st_make_valid(my_area)

qtm(shp_city)


#Get aggregated GIS features roads
#Define the road classes
class_roads <- c("motorway","trunk","primary", "secondary", "tertiary")

#Deploy the function
iNetRoad <- getOSMfeatures(city = icity, road_class = class_roads, city_area = my_area, ishp = FALSE, iplot = FALSE)

#Join with var1, var2, var3 .....
GIS_road <- st_join(iNetRoad, var1, join =st_nearest_feature, left = FALSE)
GIS_road <- st_join(GIS_road, var2, st_nearest_feature, st_is_within_distance, dist = 0.1)
#GIS_road <- st_join(GIS_road, var3, st_nearest_feature, st_is_within_distance, dist = 0.1)

#Separate sampled and non-sampled
road_sampled <- st_join(stations, GIS_road, join = st_is_within_distance, dist = 20, left = FALSE) %>%
  mutate(category = "sampled") %>% st_as_sf() %>% st_transform(crs = 4326)
road_nonsampled <- iNetRoad[!iNetRoad$osm_id%in%road_sampled$osm_id,]
road_nonsampled <- mutate(road_nonsampled, category = "nonsampled")


#Data splitting
#Split 1: stations dataset
stations_split <- road_sampled %>% distinct(id, .keep_all = TRUE)
stations_split$fclass <- as.factor(stations_split$fclass)

set.seed(1232)
Index <- createDataPartition(stations_split$fclass, #Let's split up
                             p = 0.8,
                             list = FALSE)

train_stations <- stations_split[ Index, ]
test_stations  <- stations_split[-Index, ]

#Split up traffic timeseries
df_split <- traffic %>% openair::selectByDate(year = 2022, month = 8:9)
df_split$split <- rep(x = c("training", "test"),
                      times = c(floor(x = 0.8 * nrow(x = df_split)),
                                ceiling(x = 0.2 * nrow(x = df_split))))
traffic_train <- df_split[df_split$split == 'training',]
traffic_test <- df_split[df_split$split == 'test',]


#Join timeseries traffic and weather
train_stations$id <- as.character(train_stations$id)
test_stations$id <- as.character(test_stations$id)
traffic_train$id <- as.character(traffic_train$id)
traffic_test$id <- as.character(traffic_test$id)

train_dataset <- inner_join(traffic_train, train_stations, by ="id")
test_dataset <- inner_join(traffic_test, test_stations, by ="id")

#Features engineering and selection
features_train <- train_dataset %>%
  group_by(date, osm_id) %>%
  summarise(mean_cars = round(mean(icars),digits = 0),
            mean_speed = round(mean(ispeed), digits = 0), .groups = "drop") %>%
  filter(mean_cars>10, mean_speed > 10) %>%
  inner_join(road_sampled, by= "osm_id") %>%
  inner_join(weather, by= "date") %>% #Join weather data
  as_tibble() %>% dplyr::select(-Latitude, -Longitude, -id, -name, -osm_id,-category, -geometry) %>% #Select features
  mutate_if(is.character, as.factor)

features_test <- test_dataset %>%
  group_by(date, osm_id) %>%
  summarise(mean_cars = round(mean(icars),digits = 0),
            mean_speed = round(mean(ispeed), digits = 0), .groups = "drop") %>%
  filter(mean_cars>10, mean_speed > 10) %>%
  inner_join(road_sampled, by= "osm_id") %>%
  inner_join(weather, by= "date") %>% #Join weather data
  as_tibble() %>% dplyr::select(-Latitude, -Longitude, -id, -name, -osm_id, -category, -geometry) %>% #Select features
  mutate_if(is.character, as.factor)


# Timeseries for ML using the idx_date and signature
receipe_steps <-
  recipe(mean_cars + mean_speed ~., data = features_train) %>% # Depend variable selected
  step_ts_impute(all_numeric()) %>% #Impute values for numeric predictors and outcomes
  step_impute_mode(lanes, maxspeed) %>% #Impute values for nominal/categorical variables_cars
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), -lanes, -maxspeed) %>%
  step_timeseries_signature(date) %>% # creating indexes from date-time
  step_rm(date, contains("index.num"), contains("iso"), contains("xts"))

train_recipe <- receipe_steps %>%
  prep(features_train) %>%
  bake(features_train)
test_recipe <- receipe_steps %>%
  prep(features_test) %>%
  bake(features_test)

# Apply the RF for a specific outcome
train_processed <- train_recipe %>%
  dplyr::select(-mean_speed) #Define target as outcome for training and test sets
test_processed <- test_recipe %>%
  dplyr::select(-mean_speed)

all_cores <- parallel::detectCores(logical = FALSE) - 1
cl <- makePSOCKcluster(all_cores)

registerDoParallel(cl)
set.seed(1234)
rfModel_cars <- ranger(dependent.variable.name = "mean_cars",
                       data = train_processed, num.trees = 100,
                       probability = FALSE, importance = "permutation",
                       num.threads = 2) #
stopCluster(cl)

rfModel_pred_cars <- predict(rfModel_cars, data=test_processed) #Make predictions with trained model

rfModel_df_cars <- rfModel_pred_cars$predictions %>% #Organize the dataframe with predictions and other varaibles
  bind_cols(features_test %>% dplyr::select(date)) %>%
  bind_cols(test_processed) %>%
  rename(predcars = ...1)

write_csv(rfModel_df_cars, "MLtest_cars_table.csv")

rfModel_df_cars %>%
  timePlot(pollutant = c("mean_cars", "predcars"), group = TRUE,
           avg.time = "hour",  name.pol = c("Observed", "ML-model"),
           auto.text = TRUE, cols = c("#4a8bad", "#ffa500"),
           fontsize = 16, lwd = 2, lty = 1,
           ylab = "Traffic flow",
           main = "")

rfModel_df_cars %>%
  timeVariation(pollutant = c("mean_cars", "predcars"),
                name.pol = c("Observed", "Modelled"),
                cols = c("#FAAB18", "#1380A1"),
                ci = TRUE, lwd = 3,
                fontsize = 14, ylim = c(0, 800), key.position = "bottom",
                ylab ="Traffic flow")

metrics_cars <- openair::modStats(rfModel_df_cars, mod= "predcars", obs = "mean_cars", type = "hour")
write_csv(metrics_cars, "metrics_cars_table.csv")

#variable importance
variables_cars <- as.data.frame(importance(rfModel_cars), type = 1)
colnames(variables_cars) [1] <- "importance"
variables_cars<- cbind(var.names = rownames(variables_cars), variables_cars)
variables_cars<- mutate(variables_cars, importance = importance / sum(importance) * 100,
                   importance = round(importance, digits = 1)) %>%
  arrange(desc(importance))

write_csv(variables_cars, "importance_cars_table.csv")

iplot <- variables_cars %>%
  head(20) %>%
  ggplot(aes(x=reorder(var.names, importance), y=importance, fill=importance))+
  geom_bar(stat="identity", position="dodge", show.legend = FALSE)+
  ylab("Contribution (%)")+
  coord_flip() +
  xlab("Top 20 features")+
  labs(
    subtitle = "traffic flow predicitions") +
  geom_text(aes(label = importance), hjust = 0, size = 5) +
  #scale_y_continuous(limits = c(0, 20)) +
  scale_fill_viridis_c(direction = -1) +
  theme_classic(base_size = 15)
ggsave("importance_cars_plot.png", iplot)


#ML for speed
train_processed <- train_recipe %>%
  dplyr::select(-mean_cars) #Define target as outcome for training and test sets
test_processed <- test_recipe %>%
  dplyr::select(-mean_cars)

#Random Forest
registerDoParallel(cl)
set.seed(1234)
rfModel_speed <- ranger(dependent.variable.name = "mean_speed",
                        data = train_processed, num.trees = 100,
                        probability = FALSE, importance = "permutation",
                        num.threads = 2) # Train the RF algorithm
stopCluster(cl)

rfModel_pred_speed <- predict(rfModel_speed, data=test_processed) #Make predictions with trained model

rfModel_df_speed <- rfModel_pred_speed$predictions %>% #Organize the dataframe with predictions and other varaibles
  bind_cols(features_test %>% select(date)) %>%
  bind_cols(test_processed) %>%
  rename(predspeed = ...1)

write_csv(rfModel_df_speed, "MLtest_speed_table.csv")

#Assessment model
rfModel_df_speed %>%
  openair::timePlot(pollutant = c("mean_speed", "predspeed"), group = TRUE,
           avg.time = "hour", name.pol = c("Observed", "ML-model"),
           auto.text = TRUE, cols = c("forestgreen", "brown2"),
           fontsize = 16, lwd = 2, lty = 1,
           ylab = "Average speed [km/h]",
           main = "")
metrics_cars <- modStats(rfModel_plot_cars, mod= "predspeed", obs = "mean_speed", type = "hour")
write_csv(metrics_cars, "metrics_speed_table.csv")


#variable importance
variables_speed <- as.data.frame(importance(rfModel_speed), type = 1)
colnames(variables_speed) [1] <- "importance"
variables_speed<- cbind(var.names = rownames(variables_speed), variables_speed)
variables_speed<- mutate(variables_speed, importance = importance / sum(importance) * 100,
                        importance = round(importance, digits = 1)) %>%
  arrange(desc(importance))

write_csv(variables_speed, "importance_speed_table.csv")

iplot <- variables_speed %>%
  head(20) %>%
  ggplot(aes(x=reorder(var.names, importance), y=importance, fill=importance))+
  geom_bar(stat="identity", position="dodge", show.legend = FALSE)+
  ylab("Contribution (%)")+
  coord_flip() +
  xlab("Top 20 features")+
  labs(
    subtitle = "Average speed predicitions") +
  geom_text(aes(label = importance), hjust = 0, size = 5) +
  #scale_y_continuous(limits = c(0, 20)) +
  scale_fill_viridis_c(direction = -1, option = "E") +
  theme_classic(base_size = 15)
ggsave("importance_speed_plot.png", iplot)


