
#### High spatio-temporal resolution model ########

#=================================================================
# CO2 Emissions calculations
#=================================================================

#Emission factor
coeffs <- list(
  # emission factor AVgSpeed < 50
  coeff1 = 0.0928
  , coeff2 = 2
  , coeff3 = 9.2601
  , coeff4 = 358.7
  # emission factor AVgSpeed >= 80
  , coeff5 = 0.0165
  , coeff6 = 2
  , coeff7 = 2.3481
  , coeff8 = 211.68
  # emission factor AVgSpeed >= else
  , coeff9 = 130
  # Conversion factor micro mol
  , coeff10 = 0.1/100/0.1584
  # vehic flux factor
  , flux_veihc = 3.1293
)

#Formula to calculate traffic CO2 emissions
ECO2traffic <- function(x, coeffs) {
  x <- mutate(x, emiss_factor = ifelse(speed < 50, coeffs[["coeff1"]]*(speed^coeffs[["coeff2"]]) - coeffs[["coeff3"]]*speed + coeffs[["coeff4"]],
                                       ifelse(speed >= 80,coeffs[["coeff5"]]*(speed^coeffs[["coeff6"]]) - coeffs[["coeff7"]]*speed + coeffs[["coeff8"]],
                                              coeffs[["coeff9"]])),
              ECO2_gmh = (length/1000 * cars * emiss_factor) / 1000, #CO2 emissions [k[g per 100 meters],
              ECO2_micro = ECO2_gmh/0.1584,# CO2 emissions [micro mole per meters per second]
              EC_gmh = ECO2_gmh/0.0432) #CO2 emissions [metric tons per 100 meters])
  return(x)
}

#Formula to calculate building CO2 emissions
ECO2build <- function(x, coeffs) {
  x <- mutate(x, ECO2_micro = as.numeric(ifelse(hour == 0, abs(-1.57*max(0, 9.9 - airT)-3.6/volume), ifelse(hour == 1, abs(-1.73*max(0, 12.8 - airT)- 4.2/volume),ifelse(hour == 2, abs(-1.73*max(0,12 - airT)-4.64/volume),
                                    ifelse(hour == 3, abs(-1.5*max(0, 13.1 - airT)-3.44/volume), ifelse(hour == 4, abs(-1.38*max(0, 12.4 - airT)-3.86/volume),ifelse(hour == 5, abs(-1.7*max(0, 12.9 - airT)-2.46/volume),
                                    ifelse(hour == 6, abs(-2.55*max(0, 12.7 - airT)-0.77/volume), ifelse(hour == 7, abs(-2.29*max(0, 10.2 - airT)-2.18/volume), ifelse(hour == 8, abs(-2.88*max(0, 12.7 - airT)- (-1.5)/volume),
                                    ifelse(hour == 9, abs(-2.97*max(0, 12.3 - airT)-(-1.8)/volume), ifelse(hour == 10, abs(-1.27*max(0, 13.3 - airT)-0.59/volume), ifelse(hour == 11, abs(-1.45*max(0, 15.3 - airT)-0.57/volume),
                                    ifelse(hour == 12, abs(-1.48*max(0, 15 - airT)-1.1/volume), ifelse(hour == 13, abs(-1.45*max(0, 13.7 - airT)-1.7/volume), ifelse(hour == 14, abs(-1.34*max(0, 13.2 - airT)-2.11/volume),
                                    ifelse(hour == 15, abs(-1.35*max(0, 13.5 - airT)-3.22/volume), ifelse(hour == 16, abs(-1.36*max(0, 13 - airT)-4.39/volume), ifelse(hour == 17, abs(-1.29*max(0, 12 - airT)-5.4/volume),
                                    ifelse(hour == 18, abs(-1.44*max(0, 10.6 - airT)-7.3/volume), ifelse(hour == 19, abs(-1.44*max(0, 7 - airT)-9.77/volume), ifelse(hour == 20, abs(-2.7*max(0, 6 - airT)-6.87/volume),
                                    ifelse(hour == 21, abs(-1.24*max(0, 8 - airT)-4.29/volume), ifelse(hour == 22, abs(-0.48*max(0, 10.3 - airT)-3.68/volume),
                                    ifelse(hour == 23, abs(-1.69*max(0, 12.4 - airT)-3.6/volume), ""))))))))))))))))))))))))))

  return(x)
}

#Formula to calculate building CO2 areal interpolation emission with volume of building (CITATION...)
ECO2buildAreal <- function(x, coeffs) {
  x <- mutate(x, ECO2_micro = as.numeric(ifelse(hour == "0", abs(newvolume/volume*(-1.57* pmax(0, 9.9 - airT)-3.6/volume)),
                                       ifelse(hour == "1", abs(newvolume/volume*(-1.73*pmax(0, 12.8 - airT)- 4.2/volume)),
                                       ifelse(hour == "2", abs(newvolume/volume*(-1.73*pmax(0,12 - airT)-4.64/volume)),
                                       ifelse(hour == "3", abs(newvolume/volume*(-1.5*pmax(0, 13.1 - airT)-3.44/volume)),
                                       ifelse(hour == "4", abs(newvolume/volume*(-1.38*pmax(0, 12.4 - airT)-3.86/volume)),
                                       ifelse(hour == "5", abs(newvolume/volume*(-1.7*pmax(0, 12.9 - airT)-2.46/volume)),
                                       ifelse(hour == "6", abs(newvolume/volume*(-2.55*pmax(0, 12.7 - airT)-0.77/volume)),
                                       ifelse(hour == "7", abs(newvolume/volume*(-2.29*pmax(0, 10.2 - airT)-2.18/volume)),
                                       ifelse(hour == "8", abs(newvolume/volume*(-2.88*pmax(0, 12.7 - airT)- (-1.5)/volume)),
                                       ifelse(hour == "9", abs(newvolume/volume*(-2.97*pmax(0, 12.3 - airT)-(-1.8)/volume)),
                                       ifelse(hour == "10", abs(newvolume/volume*(-1.27*pmax(0, 13.3 - airT)-0.59/volume)),
                                       ifelse(hour == "11", abs(newvolume/volume*(-1.45*pmax(0, 15.3 - airT)-0.57/volume)),
                                       ifelse(hour == "12", abs(newvolume/volume*(-1.48*pmax(0, 15 - airT)-1.1/volume)),
                                       ifelse(hour == "13", abs(newvolume/volume*(-1.45*pmax(0, 13.7 - airT)-1.7/volume)),
                                       ifelse(hour == "14", abs(newvolume/volume*(-1.34*pmax(0, 13.2 - airT)-2.11/volume)),
                                       ifelse(hour == "15", abs(newvolume/volume*(-1.35*pmax(0, 13.5 - airT)-3.22/volume)),
                                       ifelse(hour == "16", abs(newvolume/volume*(-1.36*pmax(0, 13 - airT)-4.39/volume)),
                                       ifelse(hour == "17", abs(newvolume/volume*(-1.29*pmax(0, 12 - airT)-5.4/volume)),
                                       ifelse(hour == "18", abs(newvolume/volume*(-1.44*pmax(0, 10.6 - airT)-7.3/volume)),
                                       ifelse(hour == "19", abs(newvolume/volume*(-1.44*pmax(0, 7 - airT)-9.77/volume)),
                                       ifelse(hour == "20", abs(newvolume/volume*(-2.7*pmax(0, 6 - airT)-6.87/volume)),
                                       ifelse(hour == "21", abs(newvolume/volume*(-1.24*pmax(0, 8 - airT)-4.29/volume)),
                                       ifelse(hour == "22", abs(newvolume/volume*(-0.48*pmax(0, 10.3 - airT)-3.68/volume)),
                                       ifelse(hour == "23", abs(newvolume/volume*(-1.69*pmax(0, 12.4 - airT)-3.6/volume)),
                                              ""))))))))))))))))))))))))))

  return(x)
}

#Formula to calculate NEE of CO2 fluxes (Modified1)
ECO2NEE <- function(x, coeffs) {
  x <- mutate(x, NEE =ifelse(veg_fraction>=20 & veg_fraction<50, 4.473-1/2*0.98*(0.009*(0.46*Rg)+8.106 - ((0.009*(0.46*Rg)+8.106)^2 -4*0.009*8.106*0.98*(0.46*Rg))^0.5),
                      ifelse(veg_fraction>=50 & veg_fraction<60, 1.748-1/2*0.871*(0.016*(0.46*Rg)+8.838 - ((0.016*(0.46*Rg)+8.838)^2 -4*0.016*8.838*0.871*(0.46*Rg))^0.5),
                      ifelse(veg_fraction>=60 & veg_fraction<70, 4.121-1/2*0.977*(0.014*(0.46*Rg)+16.567 - ((0.014*(0.46*Rg)+16.567)^2 -4*0.014*16.567*0.977*(0.46*Rg))^0.5),
                      ifelse(veg_fraction>=70 & veg_fraction<80, 1.74-1/2*0.972*(0.013*(0.46*Rg)+6.814 - ((0.013*(0.46*Rg)+6.814)^2 -4*0.013*6.814*0.972*(0.46*Rg))^0.5),
                      ifelse(veg_fraction>=80 & veg_fraction<=100, 2.95-1/2*0.951*(0.022*(0.46*Rg)+27.588 - ((0.022*(0.46*Rg)+27.588)^2 -4*0.022*27.588*0.951*(0.46*Rg))^0.5),0))))))
  return(x)
}

#Formula to calculate NEE of CO2 fluxes (Modified2)
ECO2NEE_2 <- function(x, coeffs) {
  x <- mutate(x, B = 27.588
              ,a = 0.0002*veg_fraction + 0.0052
              ,yc = 1.43
              ,o =0.96
              ,PAR = 0.46*Rg
              ,NEE = yc - 1/2*o*(a*PAR+B-((a*PAR+B)^2 -4*a*B*o*PAR)^0.5))
  return(x)
}

#Formula to calculate NEE of CO2 fluxes (Modified2)
ECO2NEE_local <- function(x) {
  x <- mutate(x, B = -8.25 + 0.35*veg_fraction
              ,a = 0.0002*veg_fraction + 0.0052
              ,eco = if_else(Rg<5, 3.23 * exp(0.03 * airT), 0)
              ,o =0.96
              ,PAR = 0.505*Rg
              ,NEE = eco - 1/2*o*(a*PAR+B-((a*PAR+B)^2 -4*a*B*o*PAR)^0.5))
  return(x)
}


#Formula to calculate NEE of CO2 fluxes (Modified2)
NEEboost <- function(x) {
  x <- x %>% #filter(veg_fraction > 15) %>% 
    mutate(B = abs(-8.25 + 0.35*veg_fraction)
              ,a = 0.0002*veg_fraction + 0.0052 # 0.005 + 0.016*veg_fraction
              #,yc = if_else(Rg < 20, REddyProc::fLloydTaylor(10, 330, airT + 273.15)[1], 0)
              #,Reco_night = ifelse(Rg <20, 1.3 * (exp(230 * (1/(15 - -46.02) - 1/(airT - -46.02))))*(veg_fraction/100), 0) 
              ,Reco = 1.3 * (exp(230 * (1/(15 - -46.02) - 1/(airT - -46.02))))*(veg_fraction/100)   
              #,yc = 1.3 * (exp(230 * (1/(15 - -46.02) - 1/(airT - -46.02))))*(veg_fraction/100)
              ,o =0.96
              ,PAR = 0.505*Rg
              ,NEE = Reco - 1/2*o*(a*PAR+B-((a*PAR+B)^2 -4*a*B*o*PAR)^0.5)
              #,GPP1 = - 1/2*o*(a*PAR+B-((a*PAR+B)^2 -4*a*B*o*PAR)^0.5)
              #,GPP = ifelse(GPP1>0, 0, GPP1)
              #,NEE = Reco+GPP)
    )
  return(x)
}

importGRmax <- function(file = myfilename){
  gr_df <- janitor::clean_names(file)
  gr_df <- str_split_fixed(file$time, ":", 2)
  gr_df <-  tibble("date" = unlist(gr_df[,1]),
                   "hour" = unlist(gr_df[,2])) %>%
    mutate(date=as.Date(anytime::anydate(date), format="%Y-%m-%d")) %>%
    mutate(date= as.POSIXct(paste(date, hour),format="%Y-%m-%d %H"))
  my_gr <- bind_cols(file, gr_df) %>% janitor::clean_names() %>%
    rename(Rg = g_i, sun = h_sun, airT = t2m, ws = ws10m) %>%
    dplyr::select(date, Rg, sun, airT, ws)

  return(my_gr)
}

NEEcityMax <- function(imput, veg_df = myfcover, rad = myGR, citycount = FALSE, citymap = TRUE) {

  imonth <- imput[1]
  iyear <- imput[2]

  rad <- rad %>%
    selectByDate(year = iyear, month = imonth, hour= 0:23) %>%
    mutate(Rg = ifelse(global_rad < 5, 0, global_rad)) #Global solar radiation, Rg > 5 Wm2

  veg_month <- veg_df %>% as_tibble() %>%
    mutate(date = ymd(gsub("2020", paste0(iyear), date))) %>%
    selectByDate(month = imonth) %>%
    filter(veg_fraction >30) %>%
    mutate(FID=row_number())

  #Calculate by day
  iday <- rad %>%
    mutate(day = day(date)) %>%
    distinct(day, .keep_all = FALSE) %>%
    expand.grid()

  if(citycount == TRUE) {

    myNEEday <- function(iday) {

      myday <- iday[1]

      rad_day <- rad %>%
        mutate(day = day(date)) %>%
        openair::selectByDate(day = myday,  hour = 0:23)

      #Downscale to hour
      ihour <- rad_day %>%
        mutate(ihour = hour(date)) %>%
        distinct(ihour, .keep_all = FALSE) %>%
        expand.grid()

      model_hour <- function(ihour) {

        myhour <- ihour[1]

        rad_model <- rad_day %>%
          mutate(hour = lubridate::hour(date)) %>%
          openair::selectByDate(hour = myhour)

        date_cross <-  crossing(stuff = veg_month$FID, rad_model$date) %>%
          set_names("FID", "date")
        setDT(date_cross)
        date_cross[,FID:=as.numeric(FID)]
        setDT(veg_month)
        veg_month[,FID:=as.numeric(FID)]

        fcover_model <- inner_join(date_cross, veg_month %>% dplyr::select(-date), by="FID", all.x = TRUE, all.y = FALSE)

        fcover_model <- inner_join(fcover_model, rad_model %>% dplyr::select(-day), by="date", all.x = TRUE, all.y = FALSE)

        #Calculate the NEE CO2 fluxes
        NEE_data <- ECO2NEE_2(fcover_model)
        NEE_sum <- as_tibble(sum(NEE_data$NEE)) %>% set_names("NEE")
        respiration <- NEE_data %>% filter(NEE > 0)
        respiration <- as_tibble(sum(respiration$NEE)) %>% set_names("respiration")
        uptake <- NEE_data %>% filter(NEE <0)
        uptake <- as_tibble(sum(uptake$NEE)) %>% set_names("uptake")
        NEE_table <- bind_cols(rad_model %>% distinct(date, .keep_all = FALSE),
                               NEE_sum, respiration, uptake) %>%
          mutate(NEE = NEE*0.1585,
                 respiration = respiration*0.1585,
                 uptake = uptake*0.1585)

        return(NEE_table)

      }

      MapHour <-  apply(ihour, 1, model_hour)
      NEE_hour <- do.call(rbind.data.frame, MapHour)
      return(NEE_hour)

    }

    NEEdays <-  pbapply::pbapply(iday, 1, myNEEday)
    NEE_month <- do.call(rbind.data.frame, NEEdays)
    write.csv(NEE_month, paste0("Vegetation/output/table/",iyear,imonth,"NEE.csv"))
    return(NEE_month)

  }

  if(citymap == TRUE) {

    myNEEday <- function(iday) {

      myday <- iday[1]

      rad_day <- rad %>%
        mutate(day = day(date)) %>%
        openair::selectByDate(day = myday,  hour = 0:23)

      #Downscale to hour
      ihour <- rad_day %>%
        mutate(ihour = hour(date)) %>%
        distinct(ihour, .keep_all = FALSE) %>%
        expand.grid()

      model_hour <- function(ihour) {

        myhour <- ihour[1]

        rad_model <- rad_day %>%
          mutate(hour = lubridate::hour(date)) %>%
          openair::selectByDate(hour = myhour)

        date_cross <-  crossing(stuff = veg_month$FID, rad_model$date) %>%
          set_names("FID", "date")
        setDT(date_cross)
        date_cross[,FID:=as.numeric(FID)]
        setDT(veg_month)
        veg_month[,FID:=as.numeric(FID)]

        fcover_model <- inner_join(date_cross, veg_month %>% dplyr::select(-date), by="FID", all.x = TRUE, all.y = FALSE)

        fcover_model <- inner_join(fcover_model, rad_model %>% dplyr::select(-day), by="date", all.x = TRUE, all.y = FALSE)

        #Calculate the NEE CO2 fluxes
        NEE_data <- ECO2NEE_2(fcover_model)

        NEE_ras <- NEE_data %>% dplyr::select(x, y, NEE)
        NEE_ras = raster::rasterFromXYZ(xyz = NEE_ras,crs = "+proj=longlat +datum=WGS84 +no_defs")
        NEE_ras = NEE_ras * 0.1585
        mydate <- rad_model %>% distinct(date, .keep_all = FALSE) %>% as.data.frame()
        mydate <- gsub("[: -]", "" , mydate$date, perl=TRUE)
        names(NEE_ras) <- paste0("NEE_", mydate)
        raster::writeRaster(NEE_ras, paste0("Vegetation/output/maps/", mydate, "_NEE.TIF"), format="GTiff", overwrite = TRUE)
        return(NEE_ras)

      }

      MapHour <-  apply(ihour, 1, model_hour)
      return(MapHour)

    }

    NEEdays <-  pbapply::pbapply(iday, 1, myNEEday)
    return(NEEdays)

  }

}

getOSMfeatures <- function(city = NULL, road_class = NULL, city_area = NULL, ishp = TRUE, iplot = TRUE) {

  httr::set_config(config(ssl_verifypeer = FALSE))
  sf_use_s2(FALSE)
  tmap_mode("plot")

  # Create a folder name using paste0
  folder <- paste0("output_OSMfeatures/")

  # Check if the folder exists
  if (!dir.exists(folder)) {
    # Create the folder if it does not exist
    dir.create(folder)
  }

  #Get features
  road <- getbb(city) %>% opq() %>% add_osm_feature(key = "highway", value = road_class) %>% osmdata_sf()
  road_osm  <- road$osm_lines %>% dplyr::select(osm_id, name, highway, lanes, maxspeed, geometry) %>%
    #dplyr::filter(highway %in% c(road_class)) %>%
    rename(fclass = highway) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    dplyr::filter(sf::st_is(., "LINESTRING"))
  rm(road)

  if(ishp == TRUE) {
    file <- paste0(folder,city, "_road_osm.shp")
    st_write(road_osm, file, append=FALSE)
  }

  if(iplot == TRUE) {
    road_map <- road_osm %>% tm_shape() +
      tm_lines("fclass",lwd = 1.5, title.col = "") +
      tm_layout( bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white") +
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city, "_road_osm.png")
    tmap_save(road_map, file, asp = 0)
  }

  leisure <- getbb(city) %>% opq()%>% add_osm_feature(key = "leisure") %>% osmdata_sf()
  leisure_osm  <- leisure$osm_polygons %>% dplyr::select(leisure, geometry) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(leisure)

  if(ishp == TRUE) {
    file <- paste0(folder,city, "_leisure_osm.shp")
    st_write(leisure_osm, file, append=FALSE)
  }

  if(iplot == TRUE) {
    leisure_map <- leisure_osm %>%
      tm_shape() +
      tm_polygons("leisure", n = 10, border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6", legend.outside = TRUE,
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white") +
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_leisure_osm.png")
    tmap_save(leisure_map, file, asp = 0)

  }

  landuse <- getbb(city) %>% opq() %>% add_osm_feature(key = "landuse") %>% osmdata_sf()
  landuse_osm  <- landuse$osm_polygons %>% dplyr::select(landuse, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(my_area) %>%
    st_collection_extract("POLYGON")
  rm(landuse)

  if(ishp == TRUE) {
    file <- paste0(folder,city,"_landuse_osm.shp")
    st_write(landuse_osm, file, append=FALSE)
  }

  if(iplot == TRUE) {
    landuse_map <- landuse_osm %>%
      tm_shape() +
      tm_polygons("landuse", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_landuse_osm.png")
    tmap_save(landuse_map, file, asp = 0)
  }

  amenity <- getbb(city)  %>% opq() %>% add_osm_feature(key = "amenity") %>% osmdata_sf()
  amenity_osm  <- amenity$osm_polygons %>% dplyr::select(amenity, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(amenity)


  if(ishp == TRUE) {

    file <- paste0(folder,city,"_amenity_osm.shp")
    st_write(amenity_osm,file, append = FALSE)
  }

  if(iplot == TRUE) {
    amenity_map <- amenity_osm %>%
      tm_shape() +
      tm_polygons("amenity", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_amenity_osm.png")
    tmap_save(amenity_map, file, asp = 0)
  }

  building <- getbb(city) %>% opq() %>% add_osm_feature(key = "building") %>% osmdata_sf()
  building_osm  <- building$osm_polygons %>% dplyr::select(building, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(building)

  if(ishp == TRUE) {
    file <- paste0(folder,city,"_building_osm.shp")
    st_write(building_osm, file, append = FALSE)
  }

  if(iplot == TRUE) {
    building_map <- building_osm %>%
      tm_shape() +
      tm_polygons("building", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_building_osm.png")
    tmap_save(building_map, file, asp = 0)
  }

  place <- getbb(city)  %>% opq() %>% add_osm_feature(key = "place") %>% osmdata_sf()
  place_osm  <- place$osm_polygons %>% dplyr::select(place, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(place)

  if(ishp == TRUE) {
    file <- paste0(folder,city,"_place_osm.shp")
    st_write(place_osm, file, append = FALSE)
  }

  if(iplot == TRUE) {
    place_map <- place_osm %>%
      tm_shape() +
      tm_polygons("place", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_place_osm.png")
    tmap_save(place_map, file, asp = 0)
  }

  shop <- getbb(city) %>% opq() %>% add_osm_feature(key = "shop") %>% osmdata_sf()
  shop_osm  <- shop$osm_polygons %>% dplyr::select(shop, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(shop)

  if(ishp == TRUE) {

    file <- paste0(folder,city,"_shop_osm.shp")
    st_write(shop_osm, file, append = FALSE)
  }

  if(iplot == TRUE) {
    shop_map <- shop_osm %>%
      tm_shape() +
      tm_polygons("shop", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_shop_osm.png")
    tmap_save(shop_map, file, asp = 0)
  }
  natural <- getbb(city) %>% opq() %>% add_osm_feature(key = "natural") %>% osmdata_sf()
  natural_osm  <- natural$osm_polygons %>% dplyr::select(natural, geometry) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    sf::st_intersection(city_area) %>%
    st_collection_extract("POLYGON")
  rm(natural)

  if(ishp == TRUE) {

    file <- paste0(folder,city,"_natural_osm.shp")
    st_write(natural_osm, file, append = FALSE)
  }

  if(iplot == TRUE) {
    natural_map <- natural_osm %>%
      tm_shape() +
      tm_polygons("natural", border.col = NULL) +
      tm_layout(bg.color = "#F6F6F6" #legend.outside.position = "bottom",
      ) +
      tm_legend(title = city) +
      tm_credits("Source: ©ZoomCityCarbonModel, https://github.com/ByMaxAnjos\nData: OpenStreetMap contributors, 2017", size = 0.7, position=c("left","bottom"))+
      tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 1) +
      tm_scale_bar(lwd = 1, color.dark = "black", color.light = "white")+
      tm_graticules(lwd = .1) +
      qtm(city_area, fill = NULL)

    file <- paste0(folder,city,"_natural_osm.png")
    tmap_save(natural_map, file, asp = 0)
  }

  # Join OSM features based on raod_osm at first.
  join_features <- st_join(road_osm, amenity_osm, st_nearest_feature, st_is_within_distance, dist = 0.1) #Start with road_osm
  join_features <- st_join(join_features, building_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  join_features <- st_join(join_features, leisure_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  join_features <- st_join(join_features, landuse_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  join_features <- st_join(join_features, place_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  join_features <- st_join(join_features, shop_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  join_features <- st_join(join_features, natural_osm, st_nearest_feature, st_is_within_distance, dist = 0.1)
  
  road_feat_OSM <- join_features %>%
    mutate(length = round(as.numeric(st_length(geometry)), digits = 0)) %>%
    #dplyr::select(osm_id, name, fclass, lanes, maxspeed, amenity, leisure, landuse, place, natural, shop, length,geometry) %>% #Whithout building feature
    dplyr::select(osm_id, name, fclass, lanes, maxspeed, amenity, leisure, landuse, building, place, natural, shop, length, geometry) %>% 
    mutate_if(is.integer, as.factor) %>% st_as_sf() %>% st_transform(crs = 4326)

  if(ishp == TRUE) {
    file <- paste0(folder,city,"road_features_osm.shp")
    st_write(road_feat_OSM, file)
  }
  return(road_feat_OSM)

}


#Train the ML models for selected period
DeployMLtraffic <- function(city="Berlin",input,
                        traffic_data = traffic,
                        stations_data = stations,
                        weather_data = weather,
                        road_data = iNetRoad,
                        n.trees = 100,
                        cityStreet = TRUE,
                        cityCount = TRUE,
                        cityMap = TRUE,
                        spatRes = 100,
                        tempRes = "hour",
                        iunit = "grams",
                        ista = "sum") {

  month <- input[1]
  year <- input[2]

  traffic_mod <- openair::selectByDate(traffic_data, year = iyear, month = imonth)

  #Join point counting stations
  road_sampled <- st_join(stations_data, road_data, join = st_is_within_distance, dist = 20, left = FALSE) %>%
    mutate(category = "sampled") %>% st_as_sf() %>% st_transform(crs = 4326)
  road_nonsampled <- road_data[!road_data$osm_id%in%road_sampled$osm_id,]
  road_nonsampled <- mutate(road_nonsampled, category = "nonsampled")

  #Join traffic timeseries to stations by using id
  traffic_mod$id <- as.character(traffic_mod$id)
  road_sampled$id <- as.character(road_sampled$id)

  traffic_join <- inner_join(traffic_mod, road_sampled, by ="id") # merge with stations

  features <- traffic_join %>%
    group_by(date, osm_id) %>%
    summarise(mean_cars = mean(icars),
              mean_speed= mean(ispeed), .groups = "drop") %>%
    filter(mean_cars>1, mean_speed >1) %>%
    inner_join(road_sampled, by= "osm_id") %>%
    inner_join(weather_data, by= "date") %>% #Join weather data
    as_tibble() %>% dplyr::select(-id, -name, -osm_id, -geometry) %>% #Select features
    mutate_if(is.character, as.factor)

  # Timeseries for ML using the idx_date and signature
  receipe_steps <-
    recipe(mean_cars + mean_speed ~., data = features) %>% # Depend variable selected
    step_rm(category) %>%
    step_ts_impute(all_numeric()) %>% #Impute values for numeric predictors and outcomes
    step_impute_mode(lanes, maxspeed) %>% #Impute values for nominal/categorical variables_cars
    step_unknown(all_nominal_predictors()) %>%
    step_other(all_nominal_predictors(), -lanes, -maxspeed) %>%
    step_timeseries_signature(date) %>% # creating indexes from date-time
    step_rm(date, contains("index.num"), contains("iso"), contains("xts"))

  train_recipe <- receipe_steps %>%
    prep(features) %>%
    bake(features)

  #select CARS for prediction
  train_processed <- train_recipe %>%
    dplyr::select(-mean_speed)

  #ML-ranger
  MLcars <- ranger(mean_cars ~ ., data = train_processed, num.trees =  n.trees)

  #ranger speed
  train_processed <- train_recipe %>%
    dplyr::select(-mean_cars)

  MLspeed <- ranger(mean_speed ~ ., data = train_processed, num.trees =  n.trees)

  #Daily-Hourly basis
  iday <- traffic_mod %>%
    mutate(day = lubridate::day(date)) %>%
    distinct(day, .keep_all = FALSE) %>%
    expand.grid()

  myCO2day <- function(iday) {

    myday <- iday[1]

    traffic_day <- traffic_mod %>%
      mutate(day = lubridate::day(date)) %>%
      openair::selectByDate(day = myday)
    traffic_day$id <- as.factor(traffic_day$id)

    #Deploying to Non-sampled roads
    date_cross <-  crossing(stuff = road_nonsampled$osm_id, traffic_day$date) %>%
      set_names("osm_id", "date")
    setDT(date_cross)

    model_join_nonsampled <- inner_join(date_cross, road_nonsampled, by="osm_id", all.x = TRUE, all.y = FALSE)
    model_join_nonsampled <- inner_join(model_join_nonsampled, weather_data, by="date", all.x = TRUE, all.y = FALSE)
    rm(date_cross)
    features_nonsampled <- model_join_nonsampled %>%
      dplyr::select(-geometry) %>%
      mutate_if(is.character, as.factor)

    receipe_steps <-
      recipe(date ~., data = features_nonsampled) %>% # Depend variable selected
      step_rm(osm_id, name, category) %>%
      step_ts_impute(all_numeric()) %>% #Impute values for numeric predictors and outcomes
      step_impute_mode(lanes, maxspeed) %>% #Impute values for nominal/categorical variables_cars
      step_unknown(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), -lanes, -maxspeed) %>%
      step_timeseries_signature(date) %>% # creating indexes from date-time
      step_rm(date, contains("index.num"), contains("iso"), contains("xts"))
    dataset_processed <- receipe_steps %>%
      prep(features_nonsampled) %>%
      bake(features_nonsampled)

    options(digits = 1)
    model_pred_cars <- predict(MLcars, data=dataset_processed) #Predicting the cars
    cars_pred <- model_pred_cars$predictions %>%
      as_tibble() %>% bind_cols(model_join_nonsampled %>% dplyr::select(date, osm_id, name, category)) %>%
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
    traffic_day$id <- as.character(traffic_day$id)
    road_sampled$id <- as.character(road_sampled$id)

    model_join_sampled <- inner_join(traffic_day, road_sampled, by = "id")

    features_sampled <- model_join_sampled %>%
      group_by(date, osm_id) %>%
      summarise(cars = mean(icars),
                speed = mean(ispeed), .groups = "drop") %>%
      inner_join(road_sampled, by= "osm_id") %>%
      distinct(osm_id, cars, .keep_all = TRUE) %>%
      inner_join(weather_data, by= "date") %>%
      as_tibble() %>% dplyr::select(-geometry) %>%
      mutate_if(is.character, as.factor)

    receipe_steps <-
      recipe(date ~., data = features_sampled) %>% # Depend variable selected
      step_rm(osm_id, name, category) %>%
      step_ts_impute(all_numeric()) %>% #Impute values for numeric predictors and outcomes
      step_impute_mode(lanes, maxspeed) %>% #Impute values for nominal/categorical variables_cars
      step_unknown(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), -lanes, -maxspeed) %>%
      step_timeseries_signature(date) %>% # creating indexes from date-time
      step_rm(date, contains("index.num"), contains("iso"), contains("xts"))

    dataset_processed <- receipe_steps %>%
      prep(features_sampled) %>%
      bake(features_sampled)

    traffic_CO2_sampled <- ECO2traffic(dataset_processed, coeffs) %>%
      bind_cols(features_sampled %>% dplyr::select(osm_id, date, name, category)) %>%
      mutate(cars_sampled = cars,
             speed_sampled = speed,
             ECO2_gmh_sampled = ECO2_gmh,
             ECO2_micro_sampled = ECO2_micro,
             EC_gmh_sampled = EC_gmh)
    traffic_CO2_sampled[, order(names(traffic_CO2_sampled))]

    #Join CO2 emissions raod_sampled and raod_nonsampled
    traffic_CO2 <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled)
    setDT(traffic_CO2)
    
    if (cityStreet == TRUE) {
      
      # Create a folder name using paste0
      folder <- paste0("output_citystreet/")
      
      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        dir.create(folder)
      }
      
      # Create a file name using paste0
      file <- paste0(folder,iyear,imonth,myday,"CO2street.Rds")
      
      # Write the data frame to the file
      saveRDS(traffic_CO2, file)
      
      return(traffic_CO2)
      
    }
    
    if (cityCount == TRUE) {
      #Merge CO2 sampled, nonsampled
      CO2_sampled <-  dplyr::select(traffic_CO2_sampled,  date, starts_with(c("cars_", "ECO2_gmh_", "ECO2_micro_", "EC_gmh_"))) %>%
        openair::timeAverage(avg.time = tempRes, statistic = ista)
      CO2_nonsampled <-  dplyr::select(traffic_CO2_nonsampled, date, starts_with(c("cars_", "ECO2_gmh_", "ECO2_micro_", "EC_gmh_"))) %>%
        openair::timeAverage(avg.time = tempRes, statistic = ista)

      options(digits = 0)
      trafficCO2cityCount <- CO2_sampled %>%
        inner_join(CO2_nonsampled, by= "date") %>%
        mutate(cars_total = cars_sampled + cars_nonsampled,
               ECO2_gmh_total = ECO2_gmh_sampled + ECO2_gmh_nonsampled,
               ECO2_micro_total = ECO2_micro_sampled + ECO2_micro_nonsampled,
               EC_gmh_total = EC_gmh_sampled + EC_gmh_nonsampled)

      # Create a folder name using paste0
      folder <- paste0("output_citycount/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        dir.create(folder)
      }

      # Create a file name using paste0
      file <- paste0(folder,iyear,imonth,myday,"CO2count.csv")

      # Write the data frame to the file
      write_csv(trafficCO2cityCount, file)

      #write.csv(trafficCO2cityCount, paste0("Transport/output/table/",year,month,myday,"ETCO2.csv"))

      return(trafficCO2cityCount)

    }
    
    if (cityMap == TRUE) {

      #Unit for calculations
      if (iunit == "grams") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled) %>%
          dplyr::select(osm_id, date, ECO2_gmh) %>%
          group_by(osm_id, date) %>%
          summarise(ECO2_gmh = sum(ECO2_gmh), .groups = "drop") %>%
          pivot_wider(id_cols = osm_id, names_from = date, values_from = ECO2_gmh)
      }

      if (iunit == "micro") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled) %>%
          dplyr::select(osm_id, date, ECO2_micro) %>%
          group_by(osm_id, date) %>%
          summarise(ECO2_micro = sum(ECO2_micro), .groups = "drop") %>%
          pivot_wider(id_cols = osm_id, names_from = date, values_from = ECO2_micro)
      }

      if (iunit == "gramsCarbon") {

        pivot_col <- bind_rows(traffic_CO2_sampled, traffic_CO2_nonsampled) %>%
          dplyr::select(osm_id, date, EC_gmh) %>%
          group_by(osm_id, date) %>%
          summarise(EC_gmh = sum(EC_gmh), .groups = "drop") %>%
          pivot_wider(id_cols = osm_id, names_from = date, values_from = EC_gmh)
      }

      getgeometry <- bind_rows(road_sampled %>% dplyr::select(osm_id, name, geometry)) %>%
        bind_rows(road_nonsampled %>% dplyr::select(osm_id, name, geometry))

      ECO2T_cal <- pivot_col %>%
        merge(getgeometry, by= "osm_id") %>%
        #dplyr::select(-name) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = 4326)


      # Create a folder name using paste0
      folder1 <- paste0("output_citymap/")

      # Check if the folder exists
      if (!dir.exists(folder1)) {
        # Create the folder if it does not exist
        dir.create(folder1)
      }

      # Create a file name using paste0
      file1 <- paste0(folder1,iyear,imonth,myday,iunit,"CO2map.gpkg")

      # Write the data frame to the file
      st_write(ECO2T_cal, file1, driver = "GPKG", append=FALSE)

      # #Rasterize as stars
      ECO2T_ras <- lapply(2:(ncol(ECO2T_cal)-2), function(i)
        stars::st_rasterize(ECO2T_cal [, i]))

      #Convert to data.frame
      ECO2T_ras <- lapply(1:length(ECO2T_ras), function(i)
        as.data.frame(ECO2T_ras [[i]], xyz = TRUE))
      #Convert to raster format
      ECO2T_ras <-  sapply(1:length(ECO2T_ras), FUN=function(i)
        raster::rasterFromXYZ(xyz = ECO2T_ras[[i]], crs = "+proj=longlat +datum=WGS84 +no_defs"))
      trafficCO2Map <- raster::stack(ECO2T_ras)
      file2 <- paste0(folder1,iyear,imonth,myday,iunit,"CO2ras.TIF")

      raster::writeRaster(trafficCO2Map,file2, format="GTiff", overwrite = TRUE)

      #raster::writeRaster(trafficCO2Map,paste0("Components/Transport/output/maps/",year,month,myday, "ETCO2.TIF"), format="GTiff", overwrite = TRUE)

      return(ECO2T_cal)
    }

  }

  mydays <- pbapply::pbapply(iday, 1, myCO2day)
  CO2Traffic <- do.call(rbind.data.frame, mydays)
  #write.csv(CO2Traffic_sum, paste0("output/transportation/reports",year,month,"ECO2_Tsum.csv"))
  return(CO2Traffic)

}


#PreDashboard
PrepDash <- function(city = "Berlin", ipath = "output_citystreet/", road = iNetRoad){
  
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

#Interpolate Air temperature

getLCZparameters <- function(lcz_map = lcz_map) {
  #lcz.id <- c(seq(1, 10, 1), seq(101, 107))
  lcz <- c(seq(1, 10, 1), seq(11, 17))
  lcz.code <- c(seq(1, 10, 1), "A", "B", "C", "D", "E", "F", "G")
  # lcz.name <- c('compact_high-rise', 'compact_midrise', 'compact_low-rise',
  #               'open_high-rise', 'open_midrise', 'open_low-rise',
  #               'lightweight_low-rise', 'large_low-rise', 'sparsely_built',
  #               'heavy_industry', 'dense_trees', 'scattered_trees', 'bush_scrub',
  #               'low_plants', 'bare_rock_paved', 'bare_soil_sand', 'water')
  
  lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water")
  lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
               "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
               "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA")
  
  #LCZ parameters
  SVF.min <- c(0.2, 0.3, 0.2, 0.5, 0.5, 0.6, 0.2, 0.75, 0.85, 0.6, 0.35, 0.5, 0.7, rep(0.9, 4))
  SVF.max <- c(0.4, 0.6, 0.6, 0.7, 0.8, 0.9, 0.5, 0.75, 0.85, 0.9, 0.35, 0.8, 0.9, rep(0.9, 4))
  aspect.ratio.min <- c(3, 0.75, 0.75, 0.75, 0.3, 0.3, 1, 0.1, 0.1, 0.2, 1.5, 0.25, 0.25, rep(0.1, 4))
  aspect.ratio.max <- c(3, 2, 1.5, 1.25, 0.75, 0.75, 2, 0.3, 0.25, 0.5, 1.5, 0.75, 1.0, rep(0.1, 4))
  build.frac.min <- c(40, 40, 40, rep(20,3), 60, 30, 10, 20, rep(9, 7))
  build.frac.max <- c(60, 70, 70, rep(40,3), 90, 50, 20, 30, rep(9, 7))
  imp.frac.min <- c(40, 40, 40, rep(20, 3), 60, 30, 10, 20, rep(0, 7))
  imp.frac.max <- c(60, 70, 70, rep(40, 3), 90, 50, 20, 30, rep(10, 7))
  veg.frac.max <- c(10, 20, 30, 40, 40, 60, 30, 20, 80, 50, rep(100, 4), 10, 100, 100)
  veg.frac.min <- c(0, 0, 0, 30, 20, 30, 0, 0, 60, 40, 90, 90, 90, 90, 0, 90, 90)
  tree.frac.min <- c(rep(0, 10), 90, 90, rep(0, 5))
  tree.frac.max <- c(rep(0, 10), 100, 100, rep(0, 5))
  height.roug.min <- c(26, 10, 3, 26, 10, 3, 2, 3, 3, 5, 3, 3, 2.9, 0.9, 0.24, 0.23,  0)
  height.roug.max <- c(26, 25, 10, 26, 25, 10, 4, 10, 10, 15, 30, 15, 2.9, 0.9, 0.24, 0.23, 0)
  terra.roug.min <- c(8, 6, 6, 7, 5, 5, 4, 5, 5, 5, 8, 5, 4, 3, 1, 1, 1)
  terra.roug.max <- c(8, 7, 6, 8, 6, 6, 5, 5, 6, 6, 8, 6, 5, 4, 2, 2, 1)
  surf.admit.min <- c(1.500, 1.500, 1.200, 1.400, 1.400, 1.200, 800, 1.200, 1.000, 1.000, 0, 1.000, 700, 1.200, 1.200, 600, 1.500)
  surf.admit.max <- c(1.800, 2.000, 1.800, 1.800, 2.000, 1.800, 1.500, 1.800, 1.800, 2.5000, 0, 1.800, 1.500, 1.600, 2.500, 1.400, 1.500)
  surf.albedo.min <- c(rep(0.10, 3), rep(0.12, 3), rep(0.15, 2), rep(0.12, 2), 0.10, rep(0.15, 4), 0.20, 0.02)
  surf.albedo.max <- c(rep(0.20, 3), rep(0.25, 3), 0.35, 0.25, 0.25, 0.20, 0.20, 0.25, 0.30, 0.25, 0.30, 0.35, 0.10)
  antrop.heat.min <- c(50, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7))
  antrop.heat.max <- c(300, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7))
  
  # lcz.col <- c('#8c0000', '#d10000', '#ff0100', '#be4d01', '#ff6602', '#ff9955',
  #              '#faee05', '#bcbcbc', '#ffccaa', '#555555', '#006a01', '#01aa00',
  #              '#648526', '#b9db79', '#000000', '#fbf7ae', '#6a6aff')
  lcz.df <- data.frame(lcz, lcz.name, lcz.code, lcz.col, SVF.min, SVF.max, aspect.ratio.min, aspect.ratio.max, build.frac.min, build.frac.max,
                       imp.frac.min, imp.frac.max, veg.frac.min, veg.frac.max, tree.frac.min, tree.frac.max,
                       height.roug.min, height.roug.max, terra.roug.min, terra.roug.max, surf.admit.min, surf.admit.max, surf.albedo.min, surf.albedo.max,
                       antrop.heat.min, antrop.heat.max,
                       stringsAsFactors = F) %>%
    mutate(z0 = ifelse(lcz.code %in% c("G"), 0.0002, #Get z0
                       ifelse(lcz.code %in% c("E", "F"), 0.0005,
                              ifelse(lcz.code=="D", 0.03,
                                     ifelse(lcz.code %in% c(7, "C"), 0.10,
                                            ifelse(lcz.code %in% c(8, "B"), 0.25,
                                                   ifelse(lcz.code %in% c(2, 3, 5, 6, 9, 10), 0.5,
                                                          ifelse(lcz.code %in% c(2, 4), 1.0,
                                                                 ifelse(lcz.code %in% c(1, "A"), 2, ""))))))))) %>%
    mutate(SVF.mean = round((SVF.min + SVF.max)/2, digits = 2),
           aspect.ratio.mean = (aspect.ratio.min + aspect.ratio.max)/2,
           build.frac.mea = (build.frac.min + build.frac.max)/2,
           imp.frac.mean = (imp.frac.min + imp.frac.max)/2,
           veg.frac.mean = (veg.frac.min + veg.frac.max)/2,
           tree.frac.mean = (tree.frac.min +tree.frac.max)/2,
           height.roug.mean = (height.roug.min + height.roug.max)/2,
           terra.roug.mean = (terra.roug.min + terra.roug.max)/2,
           surf.admit.mean = (surf.admit.min + surf.admit.max)/2,
           surf.albedo.mean = (surf.albedo.min + surf.albedo.max)/2,
           antrop.heat.mean = (antrop.heat.min + antrop.heat.max)/2
    )
  #Preprocessing raster
  names(lcz_map) <- "lcz"
  lcz_shp <- terra::as.polygons(rast(lcz_map)) %>% st_as_sf()
  lcz_result <- inner_join(lcz_shp, lcz.df, by="lcz")
  
  return(lcz_result)
}
