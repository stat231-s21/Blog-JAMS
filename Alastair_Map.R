library(tidyverse)
library(datasets)
library(viridis)
library(maps)
library(readr)
library(leaflet)
library(ggplot2)
library(dplyr)

###############################################################
#Final wrangling that combines the "blank map" with water data
###############################################################

#Creates "blank map" of world countries
countries <- map("world", fill = TRUE, plot = FALSE)

#Takes all country names and puts them in a datafram
countries_leaflet <- as.data.frame(countries$names) %>%
#Renames variable for clarity
  rename(country = `countries$names`) %>%
#Isolates the country name and discards information regarding region within country
#Because water and death data is on a country-wide level
  separate(col = country, into = c("country2", "extra"), sep = ":", remove = FALSE) %>%
  mutate(country2 = case_when(str_detect(country, "Virgin Islands") ~ "Virgin Islands",
                              TRUE ~ country2))

#Combines "blank map" with data on water access and deaths
countries_water <- countries_leaflet %>%
  left_join(water_access_deaths1, by= c("country2" = "country"))

#Checks for differences (country names that appear here aren't in the "blank map")
missing <- water_access_deaths1 %>%
  anti_join(countries_leaflet, by=c("country" = "country2"))
#The "blank map" does not include the island nations of Tokelau and Tuvalu
  
#########################
#Creating the Actual Map
#########################

#Creates palette and metric for which the map will be filled
mypal <- colorNumeric(
  palette = "YlGnBu",
  domain = countries_water$prop_deaths)

#Creates the interactive map
leaflet(data = countries) %>% 
  addTiles() %>%
  setView(0, 0, zoom = 1) %>%
#Base country's fill color on percent of deaths due to unsafe water
  addPolygons(fillColor = ~mypal(countries_water$prop_deaths)
              , color = "#b2aeae"  #color needs to be in hex format
              , fillOpacity = 0.7
              , stroke = FALSE
              , popup = paste0("Country: ", countries_water$country2, "<br>"
                               , "Number of Deaths: ", countries_water$num_deaths, "<br>"
                               , "Percent of Country Relying on Water Service Level:", "<br>",
                               "Surface Water: "
                               , round(countries_water$surface_water,2), "<br>"
                               , "Unimproved Water Service: "
                               , round(countries_water$unimproved,2), "<br>"
                               , "Limited Water Service: "
                               , round(countries_water$limited_service,2), "<br>"
                               , "Basic Water Service: "
                               , round(countries_water$basic_service,2), "<br>"
                               , "Safely Managed Water Service: "
                               , round(countries_water$safely_managed_service,2))) %>%
#Add legend explaining fill color
  addLegend(pal = mypal, 
            values = countries_water$prop_deaths, 
            position = "bottomright", 
            title = "Percent of deaths caused<br>by unsafe drinking water<br>in 2017")