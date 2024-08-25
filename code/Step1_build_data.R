
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/gfdx/processed"
plotdir <- "figures"
outdir <- "output"

# Read data
supply_orig <- readRDS(file.path(datadir, "GFDX_1990_2022_vehicle_supply_and_intake.Rds"))
fortified_orig <- readRDS(file.path(datadir, "GFDX_2004_2024_food_fortification_percents.Rds"))
processed_orig <- readRDS(file.path(datadir, "GFDX_2008_2024_industrially_processed_percents.Rds"))
nutrients_orig <- readRDS(file.path(datadir, "GFDX_1960_2022_nutrient_content_in_foods.Rds"))


# Prepare data
################################################################################

# Format nutrient content
nutrients <- nutrients_orig %>% 
  # Rename
  mutate(fort_status=legislation_status) %>% 
  # Simplify
  select(country, iso3, food_vehicle, fort_status, nutrient_type, nutrient, standard_mg_kg, year) %>% 
  filter(!is.na(standard_mg_kg)) %>% 
  # Reduce to most recent year
  arrange(country, iso3, food_vehicle, nutrient, desc(year)) %>% 
  group_by(country, iso3, food_vehicle, nutrient) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename year
  rename(year_nutrients=year) %>% 
  # Format nutrient
  mutate(nutrient=sub("\\s*\\([^\\)]*\\)$", "", nutrient)) %>% 
  # Format status
  mutate(fort_status=gsub(" fortification", "", fort_status))

# Inspect
freeR::complete(nutrients)
table(nutrients$food_vehicle)
table(nutrients$nutrient)
table(nutrients$fort_status)


# Format fortification compliance
fortified <- fortified_orig %>% 
  # Simplify
  select(country, iso3, food_vehicle, year, fortified_prop) %>% 
  filter(!is.na(fortified_prop)) %>% 
  # Reduce to most recent year
  arrange(country, iso3, food_vehicle, desc(year)) %>% 
  group_by(country, iso3, food_vehicle) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename year
  rename(year_pfort=year)
  
# Format supply data
supply <- supply_orig  %>% 
  # Simplify
  select(country, iso3, food_vehicle, legislation_status, year, daily_intake_g) %>% 
  filter(!is.na(daily_intake_g)) %>% 
  # Reduce to most recent year
  arrange(country, iso3, food_vehicle, desc(year)) %>% 
  group_by(country, iso3, food_vehicle) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename year
  rename(year_supply=year) %>% 
  # Format status
  rename(fort_status=legislation_status) %>% 
  mutate(fort_status=recode(fort_status,
                            "Neither mandatory nor voluntary fortification"="None",
                            "Mandatory fortification"="Mandatory",
                            "Voluntary fortification"="Voluntary"))

# Inspect
freeR::complete(supply)
table(supply$year_supply)
table(supply$fort_status)

# Format percent industrially processed
processed <- processed_orig %>% 
  # Simplifyx
  select(country, iso3, food_vehicle, year, processed_prop) %>% 
  filter(!is.na(processed_prop)) %>% 
  # Reduce to most recent year
  arrange(country, iso3, food_vehicle, desc(year)) %>% 
  group_by(country, iso3, food_vehicle) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename year
  rename(year_processed=year)
  

# Merge data
################################################################################

# Build data
data <- supply %>% 
  # Add percent processed
  left_join(processed) %>% 
  # Add percent fortified
  left_join(fortified)
  
# Inspect
freeR::complete(data)

# Export data
saveRDS(data, file=file.path(outdir, "gfdx_data.Rds"))
saveRDS(nutrients, file=file.path(outdir, "gfdx_nutrient_data.Rds"))


