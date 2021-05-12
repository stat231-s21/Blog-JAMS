library(tidyverse)
library(datasets)
library(viridis)
library(maps)
library(readr)
library(leaflet)
library(ggplot2)
library(dplyr)

##################################################################
#Data wrangling for unsafe drinking water related deaths by country
##################################################################

#Note path as this may have to change in final product
path_in <- "~/Desktop/Blog-JAMS/Data wrangling/"

water_deaths <- read_csv(paste0(path_in,"Deaths Due to Unsafe Water by Country.csv")) %>%
#Selects only the observations that record the number of deaths and percent of deaths
  filter(metric %in% c("Number", "Percent")) %>%
  select(location, metric, val) %>%
#Creates two distinct columns per country
#One column has the actual number of deaths in a country
#The second column has the percent of deaths due to unsafe drinking water
  pivot_wider(names_from = metric, values_from = val) %>%
#Renames variables for clarity  
  rename(country = location, num_deaths = Number, prop_deaths = Percent) %>%
#Removes scientific notation and rounds to nearest integer-- number of deaths should be a number
  mutate(num_deaths = round(num_deaths, digits = 0))

###################################################
#Data wrangling for drinking water access by country
###################################################

#Note path as this may have to change in final product
path_in <- "~/Desktop/Blog-JAMS/Data wrangling/"

water_access <- read_csv(paste0(path_in,"Drinking Water Access by Country.csv")) %>%
  select(Country, Coverage, `Service level`) %>%
  pivot_wider(names_from = `Service level`, values_from = Coverage)

#Turn all the na's to 0 (necessary before proceeding so that the two "basic" columns can be added)
water_access[is.na(water_access)] = 0

water_access <- water_access %>%
#Renaming to get rid of the spaces in the variable names  
  rename(country = `Country`, limited_service = `Limited service`, surface_water = `Surface water`,
         unimproved = `Unimproved`, safely_managed_service = `Safely managed service`) %>%
#Combining the two basic services into one observational category  
  mutate(basic_service = `At least basic` + `Basic service`) %>%
#Putting the columns in order from unsafe to safe
  select(country, surface_water, unimproved, limited_service, basic_service,
         safely_managed_service)

#######################
#Merge the two datasets
#######################

water_access_deaths <-
  inner_join(water_access, water_deaths, by = "country")

#############################################
#Figure out what is missing in order to map
#############################################

#Shows which countries are missing from data the water_access data set
#Needed to hard code a couple of countries in excel file
diff <- water_deaths %>%
  anti_join(water_access_deaths, by= "country")
#The countries for which there is no available data on water access and related deaths
#Taiwan, Argentina, Dominica, Palestine, 
#Eritrea, Central African Republic, Saint Kitts and Nevis

#Shows which countries don't appear in the world map
world_map <- map_data(map = "world"
                      , region = ".")

missing_world_map <- water_access_deaths %>%
  anti_join(world_map, by=c("country" = "region"))

#Corrects this in my data set
water_access_deaths1 <- water_access_deaths %>%
  mutate(country = case_when(country == "Antigua and Barbuda" ~ "Antigua"
                            , country == "Bolivia (Plurinational State of)" ~ "Bolivia"
                            , country == "Brunei Darussalam" ~ "Brunei"
                            , country == "Côte d'Ivoire" ~ "Ivory Coast"
                            , country == "Congo" ~ "Democratic Republic of the Congo"
                            , country == "Cabo Verde" ~ "Cape Verde"
                            , country == "Czechia" ~ "Czech Republic"
                            , country == "Micronesia (Federated States of)" ~ "Micronesia"
                            , country == "United Kingdom" ~ "UK"
                            , country == "Iran (Islamic Republic of)" ~ "Iran"
                            , country == "Republic of Korea" ~ "South Korea"
                            , country == "Lao People's Democratic Republic" ~ "Laos"
                            , str_detect(country, "Republic of Moldova") ~ "Moldova"
                            , country == "North Macedonia" ~ "Macedonia"
                            , country == "Democratic People's Republic of Korea" ~ "North Korea"
                            , country == "Russian Federation" ~ "Russia"
                            , country == "Eswatini" ~ "Swaziland"
                            , country == "Syrian Arab Republic" ~ "Syria"
                            , country == "Trinidad and Tobago" ~ "Tobago"
                            , country == "United Republic of Tanzania" ~ "Tanzania"
                            , country == "United States of America" ~ "USA"
                            , country == "Saint Vincent and the Grenadines" ~ "Grenadines"
                            , country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela"
                            , country == "United States Virgin Islands" ~ "Virgin Islands"
                            , country == "Viet Nam" ~ "Vietnam"
                            , TRUE ~ country)) %>%
  add_row(filter(water_access_deaths, country == "Trinidad and Tobago") %>%
            mutate(country = "Trinidad")) %>%
  add_row(filter(water_access_deaths, country == "Saint Vincent and the Grenadines") %>%
            mutate(country = "Saint Vincent"))
