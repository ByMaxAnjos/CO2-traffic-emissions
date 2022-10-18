
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






