library(tidyverse)
library(datasets)
library(viridis)
library(maps)
library(readr)
library(leaflet)
library(ggplot2)
library(dplyr)

world_map <- map_data(map = "world"
                      , region = ".")


leaflet(data = usa_states_leaflet) %>% 
  addTiles() %>%
  setView(-72.5, 42.4, zoom = 3) %>%
  addMarkers(lat=ac_lat, lng=ac_long, popup="Amherst College") %>%
  # base state's fill color on proportion planning for in-person learning
  addPolygons(fillColor = ~mypal(prop_inperson)
              , color = "#b2aeae"  #color needs to be in hex format
              , fillOpacity = 0.7
              , stroke = FALSE
              , popup = paste0("State: ", usa_states_leaflet$names, "<br>"
                               , "Number of schools reporting: "
                               , round(usa_states_leaflet$n_schools,0), "<br>"
                               , "Number of schools planning for in-person learning: "
                               , round(usa_states_leaflet$n_inperson,0), "<br>"
                               , "Proportion planning for in-person learning: "
                               , round(usa_states_leaflet$prop_inperson,2))) %>%
  # add legend explaining fill color
  addLegend(pal = mypal, 
            values = usa_states_leaflet$prop_inperson, 
            position = "bottomright", 
            title = "Proportion planning for<br>in-person learning<br>as of July 2019")