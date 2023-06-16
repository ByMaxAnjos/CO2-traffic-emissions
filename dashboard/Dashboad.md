# Emission Geographic Information platform::dashboard

<img width="1439" alt="Screenshot 2023-06-16 at 12 41 44" src="https://github.com/ByMaxAnjos/CO2-traffic-emissions/assets/94705218/649c85fc-eecb-4b81-b382-6a451b92c168">


## Pre-processing dashboard data 

The **PrepDash** has three arguments, including:

-   **city**: name of your city or area.

-   **ipath**:path, "output_citystreet/", where is the output from *DeployMLtraffic* function (the argument cityStreet=TRUE).

-   **road**: a shapefile that describes the road segments with OSM features, which is named *iNetRoad* or *GIS_road*.


```{r}

iNetRoad <- st_read("shps/iNetroad.shp")

#Call the main function PrepDash
idash <- PrepDash(city = "Berlin",
                  ipath = "output_citystreet/",
                  road = iNetRoad)


```
## Generate Dashboard

After run *PrepDash* function, use the **Dashboard_generate.Rmd** function to generate the dashboard based on your predictions. It is gonna be okay, it is expected it works automatically in generating the dashboard in format .html, clicking on *Knit*. 

## Structure of dashboard

The dashboard consists of six sections:

-   **iMap**: text...

-   **iDistrict**: text...

-   **Summary Stats**: text...

-   **iStreet**: text..

-   **Temporal Behaviour**: text..

-   **About**: text..


