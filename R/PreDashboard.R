#Pre Dashaborad using outcomes of the ZCCM::traffic

setwd("myFolder") #sets the working directory to the specified path
source("R/ZCCM_functions.R") #runs the ZCCM_functions file, which contains all specific functions 

#======================================================================
#Load data and define the path where are the files .rds
#======================================================================
iNetRoad <- st_read("shps/iNetroad.shp")

#======================================================================
#Call the main function PrepDash
#======================================================================

idash <- PrepDash(city = "Berlin",
                  ipath = "output_citystreet/",
                  road = iNetRoad)
