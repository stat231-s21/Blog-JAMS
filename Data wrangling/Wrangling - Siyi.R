library(tidyverse)
library(dplyr)

# Import the dataset of water service by urban/rural region 
water_urban_rural <- read_csv("Data wrangling/WaterServiceUrbanRuralInequality.csv") %>%
  select(Region, 'Residence Type', Year, Coverage, 'Service Type', 'Service level') 

# Rename columns
colnames(water_urban_rural) <- tolower(colnames(water_urban_rural))
names(water_urban_rural) <- str_replace_all(names(water_urban_rural), " ", "_") 

# Filter the dataset 
water_urban_rural <- water_urban_rural %>%
  filter(service_level == "Safely managed service"|service_level == "Basic service") %>%
  filter(service_type != "Hygiene") 
# %>% pivot_wider(names_from = region, values_from = coverage) 
# %>% pivot_wider(names_from = service_level, values_from =  coverage)

# Write the dataset
write_csv(x = water_urban_rural, "Data wrangling/water_urban_rural.csv")

# Import the dataset of water resources by income
water_income <- read_csv("Data wrangling/improved-water-sources-vs-gdp-per-capita.csv") 

# Rename columns
colnames(water_income) <- tolower(colnames(water_income))
names(water_income) <- str_replace_all(names(water_income), "\\$|%| |\\(|\\)", "_") 
water_income <- water_income %>%
  mutate(country = country_name, year = time, gini = gini_index__world_bank_estimate_, basic = people_using_at_least_basic_drinking_water_services____of_population_, safe = people_using_safely_managed_drinking_water_services____of_population_, gdp = gdp_per_capita__current_us__) %>%
  select(country, year, gini, gdp, basic, safe)

# Round the digits
water_income <- water_income %>% 
  mutate(across(where(is.numeric), ~ round(., 1)))

# Import SDG regions
SDG <- read_csv("Data wrangling/SDG.csv") 

# Join SDG and water_income
water_income <- water_income %>%
  left_join(SDG, by = c("country" = "country"))

# Remove NA
water_income <- na.omit(water_income)

# Write the dataset
write_csv(x = water_income, "Data wrangling/water_income.csv")