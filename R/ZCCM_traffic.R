#=======================================================================================
#
# Title:       High Spatio-temporal Resolution of traffic CO2 emissions
# Author:      Dr. Max Anjos (maxanjos@campus.ul.pt)
# Description: Interpolation of air temperarure. More details on the approach are available at:
#             https://github.com/ByMaxAnjos/CO2-traffic-emissions

# Data: 16.06.2023

#=======================================================================================


if (!require("pacman")) install.packages("pacman") # if the pacman package is not installed, install it
pacman::p_load(lubridate, tidyverse, httr, data.table, sf, openair, osmdata, tmap, recipes, timetk, caret, ranger, rmarkdown) # use pacman to load the following packages
library(lubridate) # A package that makes it easier to work with dates and times in R.
library(tidyverse) #A collection of packages for data manipulation and visualization, including dplyr, ggplot2, and tidyr
library(data.table) #A package for fast and efficient data manipulation.
library(sf) #A package for working with spatial data using the Simple Features (SF) standard.
library(httr) 
library(openair) #A package for air quality data analysis and visualization.
library(osmdata) #A package for accessing and working with OpenStreetMap data.
library(tmap) #A package for creating static and interactive maps in R.
library(recipes) #A package for preprocessing data using a formula-based interface.
library(timeDate) #A package for working with dates and times in R.
library(timetk) #A package for manipulating time series data in R.
library(ranger) #A package for building fast and accurate random forests models.
library(caret) #A package for training and evaluating machine learning models in R.

setwd("myFolder") #sets the working directory to the specified path
source("R/ZCCM_functions.R") #runs the ZCCM_functions file, which contains all specific functions 


#======================================================================
#LEARN ML MODEL
#======================================================================

#Load data
traffic <- fread("Data/traffic_berlin_2022_08_09.csv") 

traffic <- traffic %>%
  #rename(icars = flow_automovel, ispeed = speed_automovel) %>% #Rename the type of cars and speed
  #group_by(Longitude, Latitude) %>% mutate(id= cur_group_id()) %>% #Create a id for each stations based on latitude and longitue  
  dplyr::select(date, id, icars, ispeed)

#Get station shp
# stations_csv <- fread("Data/counting_stations_berlin.csv", dec=",") #Read cvs counting stations. 
# stations <- sf::st_as_sf(stations_csv, coords = c("Longitude", "Latitude"), crs=4326)
stations <- traffic %>%
  distinct(Longitude, Latitude, .keep_all = TRUE) %>% #Eleminate duplicity 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) #Convert stations csv file to shapefile based on column Latitude and Longitude.

tmap_mode("view")
qtm(stations)#Plot map

#Get meteorological data
weather <- fread("Data/weather_berlin_2022_08_09.csv") %>%  #Read weather csv file
  dplyr::select(-V1) #Delete column

#Load other variables named as var1, var2 var3 ....
var1 <- sf::read_sf("shps/var1_berlin_landuse.shp")
qtm(var1, fill="lndsAtl")#Plot map

# Get study area polygon from OpenStreetMap data
icity <- "Berlin"

shp_verify <- osmdata::getbb(city, format_out = "sf_polygon", limit = 1, featuretype = "city")
# Check if polygon was obtained successfully
if(!is.null(shp_verify$geometry) & !inherits(shp_verify, "list")) {
  study_area <- shp_verify$geometry
  study_area <- st_make_valid(study_area) %>%
    st_as_sf() %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
} else {
  study_area <- shp_verify$multipolygon
  study_area <- st_make_valid(study_area) %>%
    st_as_sf() %>%
    st_transform(crs="+proj=longlat +datum=WGS84 +no_defs")
}

qtm(study_area)# Plot map

#Define the road OSM classes. For more details: https://wiki.openstreetmap.org/wiki/Key:highway
class_roads <- c("motorway","trunk","primary", "secondary", "tertiary") #Define the road classes

iNetRoad <- getOSMfeatures(city = icity, 
                           road_class = class_roads, 
                           city_area = my_area, 
                           ishp = FALSE, #If TRUE, all feature shps are salved in the output folder. 
                           iplot = FALSE) #If TRUE, all feature maps are salved in the output folder. 
st_write(iNetRoad, "shps/iNetroad.shp") 
iNetRoad <- st_read("shps/iNetroad.shp")

#Aggregate var1 to iNetRoad (or var2, var3...)
GIS_road <- st_join(iNetRoad, var1, join =st_nearest_feature, left = FALSE) #Join with var1, var2, var3 .....
#GIS_road <- st_join(GIS_road, var2, st_nearest_feature, st_is_within_distance, dist = 0.1)
#GIS_road <- st_join(GIS_road, var3, st_nearest_feature, st_is_within_distance, dist = 0.1)

#Road Categories
road_sampled <- st_join(GIS_road, stations, join = st_is_within_distance, dist = 20, left = FALSE) %>%
  mutate(category = "sampled") %>% st_as_sf() %>% st_transform(crs = 4326)
road_nonsampled <- GIS_road[!GIS_road$osm_id%in%road_sampled$osm_id,]
road_nonsampled <- mutate(road_nonsampled, category = "nonsampled")

qtm(road_sampled, lines.col = "blue") + qtm(road_nonsampled, lines.col = "orange")

# Data station splitting
stations_split <- road_sampled %>% distinct(id, .keep_all = TRUE) %>% #create a dataframe with the unique station id
  dplyr::select(-id) %>% 
  st_join(stations, join = st_nearest_feature, left = FALSE)
stations_split$fclass <- as.factor(stations_split$fclass) #change the factor class to a factor

set.seed(1232)
Index <- createDataPartition(stations_split$fclass, #create a data partition of the stations
                             p = 0.8, #80/20%
                             list = FALSE)
train_stations <- stations_split[ Index, ] #create a train and test dataframe
test_stations  <- stations_split[-Index, ]

qtm(train_stations, dots.col = "darkblue") + qtm(test_stations, dots.col = "lightblue")

# split traffic data timeseries:training and testing sets
df_split <- traffic %>% openair::selectByDate(year = 2022, month = 8:9) #Split up traffic timeseries 

df_split$split <- rep(x = c("training", "test"),
                      times = c(floor(x = 0.8 * nrow(x = df_split)), #80 % for training
                                ceiling(x = 0.2 * nrow(x = df_split)))) # 20 % for test
traffic_train <- df_split[df_split$split == 'training',] #create a train and test dataframe
traffic_test <- df_split[df_split$split == 'test',]

train_stations$id <- as.character(train_stations$id)
test_stations$id <- as.character(test_stations$id)
traffic_train$id <- as.character(traffic_train$id)
traffic_test$id <- as.character(traffic_test$id)

train_dataset <- inner_join(traffic_train, train_stations, by ="id") #create a traffic and stations by the "id".
test_dataset <- inner_join(traffic_test, test_stations, by ="id")

#Feature selection
features_train <- train_dataset %>% #create a new dataframe with the train dataset
  group_by(date, osm_id) %>% #group by date and osm_id
  summarise(mean_cars = round(mean(icars),digits = 0), #calculate the mean of the cars  and mean speed as depend variables
            mean_speed = round(mean(ispeed), digits = 0), .groups = "drop") %>% 
  filter(mean_cars>10, mean_speed> 10) %>% #filter the dataframe by the mean of cars and speed
  inner_join(road_sampled, by= "osm_id") %>% #join the road sampled dataframe
  inner_join(weather, by= "date") %>% #join the weather dataframe
  as_tibble() %>% dplyr::select(-id, -name, -osm_id,-category, -geometry) %>% #Drop the unsual features
  mutate_if(is.character, as.factor) #mutate the character variables to factor

features_test <- test_dataset %>% #create a new dataframe with the test dataset
  group_by(date, osm_id) %>% #group by date and osm_id
  summarise(mean_cars = round(mean(icars),digits = 0), #calculate the mean of the cars and round to 0 digits
            mean_speed = round(mean(ispeed), digits = 0), .groups = "drop") %>% #calculate the mean of the speed and round to 0 digits
  filter(mean_cars>10, mean_speed> 10) %>% #filter the dataframe by the mean of cars and speed
  inner_join(road_sampled, by= "osm_id") %>% #join the road sampled dataframe
  inner_join(weather, by= "date") %>% #join the weather dataframe
  as_tibble() %>% dplyr::select(-id, -name, -osm_id, -category, -geometry) %>% #select the features
  mutate_if(is.character, as.factor) #mutate the character variables to factor

receipe_steps <-
  recipe(mean_cars + mean_speed ~., data = features_train) %>% # Depend variable selected
  step_ts_impute(all_numeric()) %>% #Impute values for numeric predictors and outcomes
  step_impute_mode(lanes, maxspeed) %>% #Impute values for nominal/categorical variables_cars
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), -lanes, -maxspeed) %>%
  step_timeseries_signature(date) %>% # creating indexes from date-time
  step_rm(date, contains("index.num"), contains("iso"), contains("xts")) 

train_recipe <- receipe_steps %>% # create a recipe for the training data
  prep(features_train) %>%
  bake(features_train)

test_recipe <- receipe_steps %>% # create a recipe for the test data
  prep(features_test) %>%
  bake(features_test)

str(test_recipe)
#Selection and training of ML 
train_processed <- train_recipe %>% #Training the RF for traffic flow predictions
  dplyr::select(-mean_speed) #Delete mean_speed for training and test sets as RF runs the traffic flow
test_processed <- test_recipe %>%
  dplyr::select(-mean_speed)

set.seed(1234)
rfModel_cars <- ranger(dependent.variable.name = "mean_cars",
                       data = train_processed, 
                       num.trees = 100, 
                       importance = "permutation") 

rfModel_pred_cars <- predict(rfModel_cars, data=test_processed) #Make predictions for test data with trained model

rfModel_df_cars <- rfModel_pred_cars$predictions %>% #Create the new dataframe with predictions and other variables
  bind_cols(features_test %>% dplyr::select(date)) %>%
  bind_cols(test_processed) %>%
  rename(predcars = ...1)
write_csv(rfModel_df_cars, "rfModel_df_cars.csv")

str(rfModel_df_cars)
train_processed <- train_recipe %>% #Training the RF for average speed predictions
  dplyr::select(-mean_cars) #Delete mean_cars for training and test sets as RF runs the average speed 
test_processed <- test_recipe %>%
  dplyr::select(-mean_cars)

set.seed(1234)
rfModel_speed <- ranger(dependent.variable.name = "mean_speed",
                        data = train_processed, 
                        num.trees = 100,
                        importance = "permutation") 

rfModel_pred_speed <- predict(rfModel_speed, data=test_processed)

rfModel_df_speed <- rfModel_pred_speed$predictions %>% 
  bind_cols(features_test %>% select(date)) %>%
  bind_cols(test_processed) %>%
  rename(predspeed = ...1)
write_csv(rfModel_df_speed, "rfModel_df_speed.csv")


#Model evaluation
rfModel_df_cars %>% #Plot timeseries for observed and modelled values
  openair::timePlot(pollutant = c("mean_cars", "predcars"), group = TRUE,
                    avg.time = "hour",  name.pol = c("Observed", "ML-model"),
                    auto.text = TRUE, cols = c("#4a8bad", "#ffa500"),
                    fontsize = 16, lwd = 2, lty = 1,
                    ylab = "Traffic flow",
                    main = "")

rfModel_df_cars %>% #Plot time variation 
  openair::timeVariation(pollutant = c("mean_cars", "predcars"),
                         name.pol = c("Observed", "Modelled"),
                         cols = c("#FAAB18", "#1380A1"),
                         ci = TRUE, lwd = 3,
                         fontsize = 14, ylim = c(0, 800), key.position = "bottom",
                         ylab ="Traffic flow")

metrics_cars <- openair::modStats(rfModel_df_cars, mod= "predcars", obs = "mean_cars", type = "hour") #It provides a set of metrics by hour by the argument "type". 
write_csv(metrics_cars, "metrics_cars.csv") #Salve the metrics table.

#variable importance
variables_cars <- as.data.frame(importance(rfModel_cars), type = 1)
colnames(variables_cars) [1] <- "importance"
variables_cars<- cbind(var.names = rownames(variables_cars), variables_cars)
variables_cars<- mutate(variables_cars, importance = importance / sum(importance) * 100,
                        importance = round(importance, digits = 1)) %>%
  arrange(desc(importance))
write_csv(variables_cars, "importance_cars.csv")

variables_cars %>%
  head(20) %>% #Top 20 features
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
#ggsave("importance_cars_plot.png", iplot) #Salve the plot


rfModel_df_speed %>% #plot the timeseries
  openair::timePlot(pollutant = c("mean_speed", "predspeed"), group = TRUE,
                    avg.time = "hour", name.pol = c("Observed", "ML-model"),
                    auto.text = TRUE, cols = c("forestgreen", "brown2"),
                    fontsize = 16, lwd = 2, lty = 1,
                    ylab = "Average speed [km/h]",
                    main = "")
metrics_cars <- modStats(rfModel_plot_cars, mod= "predspeed", obs = "mean_speed", type = "hour") #get the metrics and assess your model
write_csv(metrics_cars, "metrics_speed.csv")

variables_speed <- as.data.frame(importance(rfModel_speed), type = 1)
colnames(variables_speed) [1] <- "importance"
variables_speed<- cbind(var.names = rownames(variables_speed), variables_speed)
variables_speed<- mutate(variables_speed, importance = importance / sum(importance) * 100,
                         importance = round(importance, digits = 1)) %>%
  arrange(desc(importance))
write_csv(variables_speed, "importance_speed.csv")

variables_speed %>%
  head(20) %>% #Top 20 features
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


#======================================================================
#DEPLOY ML MODEL
#======================================================================

#define the period (inputDates)
#imonth <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
#iyear <- c(2015:2020), c(2015, 2017, 2020) or (2020).
imonth <- c("sep")
iyear <- c(2022)
input <- expand.grid(imonth, iyear) 

myMLtraffic <- pbapply::pbapply(input, 1, 
                                DeployMLtraffic(city="Berlin", input, 
                                                traffic_data = traffic, 
                                                stations_data = stations,
                                                weather_data = weather, 
                                                road_data = iNetRoad,
                                                n.trees = 100,
                                                cityStreet = FALSE, 
                                                cityCount = FALSE, 
                                                cityMap = TRUE, 
                                                tempRes = "hour", 
                                                spatRes = 100, 
                                                iunit = "grams", 
                                                ista = "sum"))

#Take your timeseries and salve based on the selected arguments:

CO2_street <- do.call(rbind.data.frame, myMLtraffic) #Use for the cityStreet 
saveRDS(CO2_street, "CO2_street_Berlin_2022_08_09.rds") #salve file

CO2_count <- do.call(rbind.data.frame, myMLtraffic) #Use for the cityCount 
write_csv(CO2_count, "CO2_count_Berlin_2022_08_09.csv") #salve file

CO2_map <- unlist(myMLtraffic) ##Use for the cityMqp
raster::writeRaster(CO2_map,"CO2_map_Berlin_2022_08_09.TIF", format="GTiff", overwrite =TRUE) #salve file

