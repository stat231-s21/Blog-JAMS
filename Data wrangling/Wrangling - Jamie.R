library(tidyverse)
library(dplyr)

# Import the dataset for water access in schools in 2019
school_water_data_by_country <- read_csv("Data Wrangling/basic water access in schools by country.csv") %>%
  filter(ServiceLevel == "Basic service") %>%
  select(Country, SchoolType, Coverage) %>%
  rename(country = `Country`, schoolType = `SchoolType`, coverage = `Coverage`) 
  #pivot_wider(names_from = schoolType, values_from = coverage) %>%
  #rename(basicWaterCoveragePrimary = `primary`, basicWaterCoverageSecondary = `secondary`) 

# Import the dataset for primary school enrollment rates
primary_enrollment_rates <- read_csv("Data Wrangling/gross-enrollment-ratio-in-primary-education.csv") %>%
  group_by(Entity) %>%
  slice(which.max(Year)) %>%
  select(Entity, GrossEnrolmentRatio) %>%
  rename(primary = `GrossEnrolmentRatio`)

# Import the dataset for secondary school enrollment rates
secondary_enrollment_rates <- read_csv("Data Wrangling/gross-enrollment-ratio-in-secondary-education.csv") %>%
  group_by(Entity) %>%
  slice(which.max(Year)) %>%
  select(Entity, GrossEnrolmentRatio) %>%
  rename(secondary = `GrossEnrolmentRatio`)

# Joining the primary and secondary enrollment rates datasets
school_enrollment_rates <- primary_enrollment_rates %>%
  left_join(secondary_enrollment_rates, by = c("Entity" = "Entity")) %>%
  rename(country = `Entity`) %>%
  pivot_longer(!country, names_to = "schoolType", values_to = "grossSchoolEnrollmentRatio")

# Joining the enrollment rates and water access datasets
scatterplot_data <- merge(x = school_enrollment_rates, y = school_water_data_by_country, by = "country", all = FALSE)

# Only keep rows that exclusively account for primary school or exclusively account for secondary schools
final_scatter_data <- subset(scatterplot_data, (schoolType.x == "primary" & schoolType.y == "primary") | 
                              (schoolType.x == "secondary" & schoolType.y == "secondary")) %>%
                      select(country, schoolType.x, grossSchoolEnrollmentRatio, coverage) %>%
                      rename(schoolType = `schoolType.x`)

# Writing csv file
write_csv(final_scatter_data, "Data Wrangling/school-enrollment-and-water-access.csv")

# Import the dataset for water access in schools by region
regional_water_data <- read_csv("Data Wrangling/Water in schools by region.csv") %>%
    rename(schoolType = `Residence / School Type`, region = `Region`, coverage = `Coverage`, serviceLevel = `Service level`, year = `Year`)

# Writing csv file
write_csv(regional_water_data, "Data Wrangling/school-water-access-regional.csv")
