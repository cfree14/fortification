
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
data_orig <- readRDS(file.path(outdir, "gfdx_data.Rds"))
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))

# Download country data from the World Bank, including income classifications
wb_data <- wbstats::wb_countries() %>%
  select(iso3c, income_level_iso3c) %>% 
  rename(income=income_level_iso3c)

# To-do list
# Record style of imputation


# Read data
################################################################################

# Build data
data <- data_orig %>%
  # Remove original region (b/c not verified)
  select(-c(region, income)) %>% 
  # Add World Bank continent and region
  mutate(continent=countrycode(country, "country.name", "continent"),
         region=countrycode(country, "country.name", "region")) %>% 
  # Add income
  left_join(wb_data, by=c("iso3"="iso3c")) %>% 
  mutate(income=ifelse(country=="Vatican City", "HIC", income)) %>% 
  # Add processing prop source (reported, imputed, non)
  mutate(processed_prop_type=case_when(!is.na(processed_prop) ~ "Reported", 
                                       is.na(processed_prop) & fort_status!="None" ~ "Imputed",
                                       fort_status=="None" ~ "None",
                                       T ~ NA)) %>% 
  # Add fortified prop source (reported, imputed, non)
  mutate(fortified_prop_type=case_when(!is.na(fortified_prop) ~ "Reported", 
                                       is.na(fortified_prop) & fort_status!="None" ~ "Imputed",
                                       fort_status=="None" ~ "None",
                                       T ~ NA)) %>% 
  # Calculate average % processed by region
  group_by(region, food_vehicle) %>% 
  mutate(processed_prop_region=mean(processed_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate average % processed by continent
  group_by(continent, food_vehicle) %>% 
  mutate(processed_prop_continent=mean(processed_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Pick best % processed based on hierarchy
  mutate(processed_prop_use=case_when(processed_prop_type == "Imputed" & !is.na(processed_prop_region) ~ processed_prop_region,  
                                      processed_prop_type == "Imputed" & is.na(processed_prop_region) ~ processed_prop_continent, 
                                      T ~ processed_prop)) %>% 
  # Impute salt processing prop
  # Low income = 95, High income = 100
  mutate(processed_prop_use=case_when(processed_prop_type=="Imputed" & food_vehicle=="Salt" & income %in% c("LIC", "LMC", "INX") ~ 0.95,
                                      processed_prop_type=="Imputed" & food_vehicle=="Salt" & income %in% c("HIC", "UMC") ~ 1,
                                      T ~ processed_prop_use)) %>% 
  ## PERCENT FORTIFIED
  # Calculate % fortified by region/legal status
  group_by(region, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_region=mean(fortified_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate % fortified by continent/legal status
  group_by(continent, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_continent=mean(fortified_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate % fortified by income/legal status
  group_by(income, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_income=mean(fortified_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Pick best % fortified based on hierarchy
  mutate(fortified_prop_use=case_when(fortified_prop_type == "Imputed" & !is.na(fortified_prop_region) ~ fortified_prop_region,  
                                      fortified_prop_type == "Imputed" & is.na(fortified_prop_region) & !is.na(fortified_prop_continent) ~ fortified_prop_continent, 
                                      fortified_prop_type == "Imputed" & is.na(fortified_prop_region) & is.na(fortified_prop_continent) ~ fortified_prop_income, 
                                      T ~ fortified_prop)) %>% 
  # If voluntary and not provided, assume 0
  mutate(fortified_prop_use=ifelse(fort_status=="Voluntary" & is.na(fortified_prop), 0, fortified_prop_use)) %>% 
  # Arrange
  select(continent, region, income, country, iso3,
         food_vehicle, fort_status,
         year_supply, daily_intake_g, 
         processed_prop_type, year_processed, 
         processed_prop_use, processed_prop, processed_prop_region, processed_prop_continent, 
         fortified_prop_type, year_pfort, 
         fortified_prop_use, fortified_prop, fortified_prop_region, fortified_prop_continent, fortified_prop_income,
         everything())

# Inspect
freeR::complete(data)
table(data$processed_prop_type)


# Subset to rows with fort programs
data_fort <- data %>% 
  filter(fort_status!="None")

# Check - these should all be complete for important values
freeR::complete(data_fort)

# Simple data
################################################################################

# Simple data
data_simple <- data %>% 
  # Simplify
  select(-c(year_supply,
            year_processed , processed_prop, processed_prop_region, processed_prop_continent, 
            year_pfort, fortified_prop, fortified_prop_region, fortified_prop_continent, fortified_prop_income)) %>% 
  # Rename
  rename(processed_prop=processed_prop_use,
         fortified_prop=fortified_prop_use) 

# Inspect
str(data_simple)
freeR::complete(data_simple)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "gfdx_data_imputed_full.Rds"))
saveRDS(data_simple, file=file.path(outdir, "gfdx_data_imputed_simple.Rds"))
