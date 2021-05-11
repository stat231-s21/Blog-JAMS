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
#Selects only the observations that record the number of deaths  
  filter(metric %in% "Number") %>%
  select(location, val) %>%
#Renames variables for clarity  
  rename(country = location, deaths = val) %>%
#Removes scientific notation and rounds to nearest integer-- number of deaths should be a number
  mutate(deaths = round(deaths, digits = 0))

  
  

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

water_acess_deaths <-
  inner_join(water_access, water_deaths, by = "country")


#Shows which countries are missing from data the water_access data set
#Needed to hard code a couple of countries
diff <- water_deaths %>%
  anti_join(water_acess_deaths, by= "country")


