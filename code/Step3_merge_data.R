
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
food_orig <- readRDS(file.path(outdir, "gfdx_data_imputed_simple.Rds"))
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))
kcal_orig <- readRDS("data/intake_proxy/processed/intake_data_imputed.Rds")

# Source function
source("code/calc_aligned_standard.R")


# Prepare data
################################################################################

# Prepare kcalorie data
# Scalar for what each group is relative to mean
kcal <- kcal_orig %>% 
  group_by(iso3) %>% 
  mutate(kcal_scalar=kcalories/mean(kcalories)) %>% 
  ungroup()

# Prepare foood data
food <- food_orig %>%
  # Simplify
  select(iso3, food_vehicle, daily_intake_g, fort_status, processed_prop, fortified_prop)

# Prepare nutrient standard
nutrients <- nutrients_orig %>%
  # Simplify
  select(iso3, food_vehicle, nutrient, standard_mg_kg)


# Build master data
################################################################################

# Sex
sexes <- c("Males", 'Females')
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
isos <- freeR::uniq(food$iso3)
fvs <- freeR::uniq(food$food_vehicle)

# Build master data
data <- expand.grid(iso3=isos,
                  food_vehicle=fvs,
                  sex=sexes, 
                  age_group=age_groups) %>% 
  # Factor age group
  mutate(age_group=factor(age_group)) %>% 
  # Add calorie age group
  mutate(age_group_kcal=case_when(age_group %in% c("0-4", "5-9") ~ "0-9",
                                  age_group %in% c("10-14", "15-19") ~ "10-19",
                                  age_group %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
                                  age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64") ~ "40-64",
                                  age_group %in% c("65-69", "70-74", "75-79", "80+") ~ "65+",
                                  T ~ NA)) %>% 
  # Add kilocalories
  left_join(kcal %>% select(iso3, sex, age, kcalories, kcal_scalar), by=c("iso3", "sex", "age_group_kcal"="age")) %>% 
  # Arrange
  arrange(iso3, food_vehicle, sex, age_group) %>% 
  # Add food vehicle stats
  left_join(food, by=c("iso3", "food_vehicle")) %>% 
  # Reduce to only fortified food vehicles
  filter(!is.na(daily_intake_g)) %>% 
  # Calculate country-fv-sex-age scaled intakes based on calories
  rename(daily_intake_g_avg=daily_intake_g) %>% 
  mutate(daily_intake_g=daily_intake_g_avg*kcal_scalar) %>% 
  relocate(daily_intake_g, .after=daily_intake_g_avg) %>% 
  # Daily intake in kg
  mutate(daily_intake_kg=daily_intake_g/1000) %>% 
  relocate(daily_intake_kg, .after=daily_intake_g)
  

# Inspect 
freeR::complete(data)

# Add nutrient standards
data1 <- data %>% 
  # Add nutrient standard
  inner_join(nutrients, by=c("iso3", "food_vehicle"), relationship = "many-to-many") %>% 
  # Calculate aligned standard
  rowwise() %>% 
  mutate(aligned_standard_mg_kg=calc_aligned_standard(fv=food_vehicle,
                                                      nutr=nutrient,
                                                      intake_g_d=daily_intake_g_avg)) %>% 
  ungroup() %>% 
  # Use current standard when there is no aligned standard
  mutate(aligned_standard_mg_kg=ifelse(is.na(aligned_standard_mg_kg), standard_mg_kg, aligned_standard_mg_kg))

# Inspect
freeR::complete(data1)

# Export data
saveRDS(data1, file=file.path(outdir, "fortification_scenario_data.Rds"))




