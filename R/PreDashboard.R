#=======================================================================================
#
# Title:       Pre Dashaborad using outcomes of the ZCCM::traffic emissions
# Author:      Dr. Max Anjos (maxanjos@campus.ul.pt)
# Description: Pre processing data for creation of dashboard. More details on the approach are available at:
#             https://github.com/ByMaxAnjos/CO2-traffic-emissions

# Data: 16.06.2023
#=======================================================================================

if (!require("pacman")) install.packages("pacman") # if the pacman package is not installed, install it
pacman::p_load(lubridate, tidyverse, data.table, sf, httr, openair, osmdata, tmap, recipes, timetk, caret, ranger, rmarkdown) # use pacman to load the following packages
library(lubridate) # A package that makes it easier to work with dates and times in R.
library(tidyverse) #A collection of packages for data manipulation and visualization, including dplyr, ggplot2, and tidyr
library(data.table) #A package for fast and efficient data manipulation.
library(sf) #A package for working with spatial data using the Simple Features (SF) standard.
library(httr).
library(osmdata) #A package for accessing and working with OpenStreetMap data.
library(raster)

setwd("myFolder") #sets the working directory to the specified path

#======================================================================
#PreDashboard function
PrepDash <- function(city = "yourCity", ipath = "output_citystreet/", road = NULL){
  
  #Load data
  df <- list.files(path = ipath,     # Identify all csv files in folder
                   pattern = "*CO2street.Rds", full.names = TRUE) %>%
    lapply(readRDS) %>%                                            # Open and store all files in list
    bind_rows () %>%                                             # Combine data sets into one data.frame
    setkey(date)
  
  # Create a folder name using paste0
  folder <- paste0("dashboard/data/")
  
  # Check if the folder exists
  if (!dir.exists(folder)) {
    # Create the folder if it does not exist
    dir.create(folder)
  }
  
  # Get study area polygon from OpenStreetMap data
  shp_verify <- osmdata::getbb(city, format_out = "sf_polygon", limit = 1, featuretype = "city")
  # Check if polygon was obtained successfully
  if(!is.null(shp_verify$geometry) & !inherits(shp_verify, "list")) {
    study_area <- shp_verify$geometry
    study_area <- st_make_valid(study_area) %>%
      st_as_sf()
  } else {
    study_area <- shp_verify$multipolygon
    study_area <- st_make_valid(study_area) %>%
      st_as_sf() 
  }
  
  # iMap
  imap <- df %>%
    group_by(osm_id) %>%
    summarise(tCO2 = round(sum(ECO2_gmh)/1000, digits = 2)) %>%
    inner_join(road, by= "osm_id") %>%
    dplyr::select(tCO2, name, geometry) %>%
    st_as_sf() %>%
    st_transform(crs = 4326) %>% 
    group_by(name) %>% 
    summarise(tCO2=sum(tCO2),
              geometry = st_union(geometry))
  file <- paste0(folder,"imap.shp")
  st_write(imap, file, append=FALSE)
  
  
  #Trend emissions
  trend <- df %>%
    group_by(date_day) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"trend.csv")
  write_csv(trend, file)
  
  
  #Total of traffic emissions [ktCO2/month]
  mapCount <- df %>%
    group_by(osm_id) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits = 2),
              ktC = round(sum(EC_gmh)/1000/1000, digits = 2),
              mean_cars = round(mean(cars), digits = 2))
  
  file <- paste0(folder,"mapCount.csv")
  write_csv(mapCount, file)
  
  #Top CO2 Amenity sites
  amenity <- df %>%
    subset(amenity !="other") %>%
    subset(amenity !="unknown") %>%
    group_by(amenity) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2)) %>%
    arrange(desc(ktCO2))
  file <- paste0(folder,"amenity.csv")
  write_csv(amenity, file)
  
  #Leisure
  leisure <- df %>%
    subset(leisure !="other") %>%
    subset(leisure !="unknown") %>%
    group_by(leisure) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2)) %>%
    arrange(desc(ktCO2))
  file <- paste0(folder,"leisure.csv")
  write_csv(leisure, file)
  
  
  #Natural 
  natural <- df %>%
    subset(natural !="other") %>%
    subset(natural !="unknown") %>%
    group_by(natural) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2)) %>%
    arrange(desc(ktCO2))
  file <- paste0(folder,"natural.csv")
  write_csv(natural, file)
  
  #Shop
  shop <- df %>%
    subset(shop !="other") %>%
    subset(shop !="unknown") %>%
    group_by(shop) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2)) %>%
    arrange(desc(ktCO2))
  file <- paste0(folder,"shop.csv")
  write_csv(shop, file)
  
  #OSM_road fclass
  OSM_road <- df %>%
    group_by(fclass) %>%
    subset(fclass !="other") %>% 
    subset(fclass !="unknown") %>% 
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"OSM_road.csv")
  write_csv(OSM_road, file)
  
  #Place
  place <- df %>%
    group_by(place) %>%
    subset(place !="other") %>% 
    subset(place !="unknown") %>% 
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"place.csv")
  write_csv(place, file)
  
  #Landuse
  landuse <- df %>%
    group_by(landuse) %>%
    subset(landuse !="other") %>% 
    subset(landuse !="unknown") %>% 
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"landuse.csv")
  write_csv(landuse, file)
  
  #Top 20 Emitter streets in tCO2
  street <- df %>%
    group_by(name) %>%
    subset(name !="other") %>%
    summarise(tCO2 = round(sum(ECO2_gmh)/1000, digits= 2)) %>%
    arrange(desc(tCO2))
  file <- paste0(folder,"street.csv")
  write_csv(street, file)
  
  #Find street
  find_street <- df %>%
    subset(name !="other") %>%
    subset(landuse !="unknown") %>% 
    dplyr::select(name, ECO2_gmh, ECO2_micro, EC_gmh, cars, speed, length, fclass, lanes,
                  maxspeed, amenity, leisure, landuse, natural, shop) %>%
    head(100)
  file <- paste0(folder,"find_street.csv")
  write_csv(find_street, file)
  
  #Hour
  hour <- df %>%
    group_by(date_hour) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"hour.csv")
  write_csv(hour, file)
  
  #Week
  week <- df %>%
    group_by(date_wday) %>%
    summarise(ktCO2 = round(sum(ECO2_gmh)/1000/1000, digits= 2))
  file <- paste0(folder,"week.csv")
  write_csv(week, file)
  
  #iDistrict map
  district <- osmdata::getbb(city) %>% opq() %>% add_osm_feature(key = "admin_level", value = "9") %>% # level 9 is for districts
    osmdata_sf()
  district_osm <- district$osm_multipolygons %>%
    dplyr::select(osm_id, name, geometry) %>% rename(district = name) %>% 
    st_intersection(my_area) %>% 
    st_make_valid() 
  district_osm_type <- st_geometry_type(district_osm)
  district_osm <- district_osm[district_osm_type == "POLYGON",]
  rm(district, district_osm_type)
  iDistrict_map <- df %>%
    group_by(osm_id) %>%
    summarise(tCO2 = round(sum(ECO2_gmh)/1000, digits = 2)) %>%
    inner_join(road, by= "osm_id") %>%
    dplyr::select(tCO2, geometry) %>%
    st_as_sf() %>%
    st_transform(crs = 4326) %>%
    raster::aggregate(district_osm, FUN=sum) %>% 
    bind_cols(district_osm %>% as_tibble() %>% dplyr::select(-geometry)) %>%
    st_as_sf() %>%
    st_transform(crs = 4326)
  file <- paste0(folder,"iDistrict.shp")
  st_write(iDistrict_map, file, append=FALSE)
  
  #create a tibble for our line plot
  iday <- df %>% distinct(date_day, .keep_all = F)
  iday <- iday$date_day
  iDistrict_tibble <- pbapply::pblapply(1:length(iday), FUN = function(i)
    dplyr::select(df, osm_id, date_day,ECO2_gmh) %>% 
      dplyr::filter(date_day == paste0(iday[i])) %>% 
      group_by(osm_id, date_day) %>%
      summarise(tCO2 = round(sum(ECO2_gmh)/1000, digits = 2), .groups = "drop") %>%
      inner_join(road, by= "osm_id") %>%
      dplyr::select(tCO2, geometry) %>%
      st_as_sf() %>%
      st_transform(crs = 4326) %>%
      raster::aggregate(district_osm, FUN=sum) %>% as_tibble() %>% dplyr::select(-geometry) %>%
      mutate(day = paste0(iday[i])) %>% 
      bind_cols(district_osm %>% as_tibble() %>% dplyr::select(-geometry)) %>% 
      dplyr::select(-osm_id)
  ) 
  
  iDistrict_tibble <- do.call(rbind.data.frame, iDistrict_tibble) %>% 
    dplyr::select(-osm_id)
  file <- paste0(folder,"iDistrict_table.csv")
  write_csv(iDistrict_tibble, file)
  return()
}

#======================================================================
#Load data and define the path where are the files .rds

iNetRoad <- st_read("shps/iNetroad.shp")

#======================================================================
#Call the main function PrepDash

idash <- PrepDash(city = "Berlin",
                  ipath = "output_citystreet/",
                  road = iNetRoad)
