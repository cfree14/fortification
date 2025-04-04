
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

# Read global intake inadequacies
sevs_orig <- readRDS("data/global_intake_inadequacies/2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds")

# Download country data from the World Bank, including income classifications
wb_data <- wbstats::wb_countries() %>%
  select(iso3c, income_level_iso3c) %>% 
  rename(income=income_level_iso3c)

# To-do list
# Record style of imputation


# Read data
################################################################################

# Country key
cntry_key <- data_orig %>% 
  # Contries
  select(country, iso3) %>% 
  unique() %>% 
  # Add World Bank continent and region
  mutate(continent=countrycode(country, "country.name", "continent"),
         region=countrycode(country, "country.name", "region")) %>% 
  # Add income
  left_join(wb_data, by=c("iso3"="iso3c")) %>% 
  mutate(income=ifelse(country=="Vatican City", "HIC", income)) 

# ISO3-FV grid
iso3_fv_grid <- expand.grid(iso3=sort(unique(data_orig$iso3)),
                            food_vehicle=sort(unique(data_orig$food_vehicle)))

# Full country / food vehicle grid
data <-  iso3_fv_grid %>% 
  # Add country info
  left_join(cntry_key, by="iso3") %>% 
  # Add food info
  left_join(data_orig %>% select(-c(region, income, country)), by=c("iso3", "food_vehicle")) %>% 
  # Assume no fortification if blank
  mutate(fort_status=ifelse(is.na(fort_status), "None", fort_status)) %>% 
  # RECORD IMPUTATION TYPE
  # Daily intake
  mutate(daily_intake_type=case_when(!is.na(daily_intake_g) ~ "Reported", 
                                     is.na(daily_intake_g) ~ "Imputed",
                                     T ~ NA)) %>% 
  # Add processing prop source (reported, imputed, non)
  mutate(processed_prop_type=case_when(!is.na(processed_prop) ~ "Reported", 
                                       is.na(processed_prop) ~ "Imputed",
                                       T ~ NA)) %>% 
  # Add fortified prop source (reported, imputed, non)
  mutate(fortified_prop_type=case_when(!is.na(fortified_prop) ~ "Reported", 
                                       is.na(fortified_prop) & fort_status!="None" ~ "Imputed",
                                       fort_status=="None" ~ "None",
                                       T ~ NA)) %>% 
  # Erase fortififed props from proxies
  mutate(fortified_prop_no_proxies=ifelse(grepl("Proxy", fortified_prop_type), NA, fortified_prop)) %>% 
  # CALCULATE AVERAGES
  # Calculate average % processed by region
  group_by(region, food_vehicle) %>% 
  mutate(daily_intake_g_region=mean(daily_intake_g, na.rm=T),
         processed_prop_region=mean(processed_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate average % processed by continent
  group_by(continent, food_vehicle) %>% 
  mutate(daily_intake_g_continent=mean(daily_intake_g, na.rm=T),
         processed_prop_continent=mean(processed_prop, na.rm=T)) %>% 
  ungroup() %>% 
  # SELCECT BEST
  # Pick best daily intake based on hierarchy
  mutate(daily_intake_g_use=ifelse(!is.na(daily_intake_g), daily_intake_g, daily_intake_g_region)) %>% 
  # Pick best % processed based on hierarchy
  mutate(processed_prop_use=case_when(processed_prop_type == "Imputed" & !is.na(processed_prop_region) ~ processed_prop_region,  
                                      processed_prop_type == "Imputed" & is.na(processed_prop_region) ~ processed_prop_continent, 
                                      T ~ processed_prop)) %>% 
  # Impute salt processing prop
  # Low income = 95, High income = 100
  mutate(processed_prop_use=case_when(processed_prop_type=="Imputed" & food_vehicle=="Salt" & income %in% c("LIC", "LMC", "INX") ~ 0.90,
                                      processed_prop_type=="Imputed" & food_vehicle=="Salt" & income %in% c("HIC", "UMC") ~ 0.95,
                                      T ~ processed_prop_use)) %>% 
  ## PERCENT FORTIFIED
  mutate(fortified_prop=ifelse(fort_status=="None", 0, fortified_prop)) %>% 
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
  ## PERCENT FORTIFIED (IGNORING PROXIES)
  mutate(fortified_prop_no_proxies_type=case_when(!is.na(fortified_prop_no_proxies) ~ "Reported", 
                                                  is.na(fortified_prop_no_proxies) & fort_status!="None" ~ "Imputed",
                                                  fort_status=="None" ~ "None",
                                                  T ~ NA)) %>% 
  mutate(fortified_prop_no_proxies=ifelse(fort_status=="None", 0, fortified_prop_no_proxies)) %>% 
  # Calculate % fortified by region/legal status
  group_by(region, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_no_proxies_region=mean(fortified_prop_no_proxies, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate % fortified by continent/legal status
  group_by(continent, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_no_proxies_continent=mean(fortified_prop_no_proxies, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate % fortified by income/legal status
  group_by(income, fort_status, food_vehicle) %>% 
  mutate(fortified_prop_no_proxies_income=mean(fortified_prop_no_proxies, na.rm=T)) %>% 
  ungroup() %>% 
  # Pick best % fortified based on hierarchy
  mutate(fortified_prop_no_proxies_use=case_when(fortified_prop_no_proxies_type == "Imputed" & !is.na(fortified_prop_no_proxies_region) ~ fortified_prop_no_proxies_region,  
                                                 fortified_prop_no_proxies_type == "Imputed" & is.na(fortified_prop_no_proxies_region) & !is.na(fortified_prop_no_proxies_continent) ~ fortified_prop_no_proxies_continent, 
                                                 fortified_prop_no_proxies_type == "Imputed" & is.na(fortified_prop_no_proxies_region) & is.na(fortified_prop_no_proxies_continent) ~ fortified_prop_no_proxies_income, 
                                                 T ~ fortified_prop_no_proxies)) %>% 
  # If voluntary and not provided, assume 0
  mutate(fortified_prop_no_proxies_use=ifelse(fort_status=="Voluntary" & is.na(fortified_prop_no_proxies), 0, fortified_prop_no_proxies_use)) %>% 
  # Arrange
  select(continent, region, income, country, iso3,
         food_vehicle, fort_status,
         year_supply, 
         daily_intake_type, daily_intake_g_use, daily_intake_g, daily_intake_g_region, daily_intake_g_continent, 
         processed_prop_type, year_processed, 
         processed_prop_use, processed_prop, processed_prop_region, processed_prop_continent, 
         fortified_prop_type, year_pfort, 
         fortified_prop_use, fortified_prop, fortified_prop_region, fortified_prop_continent, fortified_prop_income,
         fortified_prop_no_proxies_type, fortified_prop_no_proxies_use, fortified_prop_no_proxies, 
         fortified_prop_no_proxies_region, fortified_prop_no_proxies_continent, fortified_prop_no_proxies_income,
         everything())

# Inspect
freeR::complete(data)


# Simplify data
################################################################################

# Simple data
data_simple <- data %>% 
  # Simplify
  select(continent, region, income, country, iso3,
         food_vehicle, fort_status, 
         daily_intake_type, daily_intake_g_use,
         processed_prop_type, processed_prop_use,
         fortified_prop_type, fortified_prop_use, fortified_prop_no_proxies_use) %>% 
  # Rename
  rename(daily_intake_g=daily_intake_g_use,
         processed_prop=processed_prop_use,
         fortified_prop=fortified_prop_use,
         fortified_prop_no_proxies=fortified_prop_no_proxies_use) 

# Inspect
str(data_simple)
freeR::complete(data_simple)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "gfdx_data_imputed_full.Rds"))
saveRDS(data_simple, file=file.path(outdir, "gfdx_data_imputed_simple.Rds"))
write.csv(data_simple, file=file.path(outdir, "gfdx_data_imputed_simple.csv"), row.names=F)

