

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
sevs_orig <- readRDS("data/global_intake_inadequacies/2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds")

# Source function
source("code/calc_aligned_standard.R")


# Calculate SEV by country and nutrient
################################################################################

# Format SEVs
sevs <- sevs_orig %>% 
  group_by(nutrient, iso3, iron_abs) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T),
            sev_avg=mean(sev, na.rm = T)/100) %>% 
  ungroup() %>% 
  mutate(sev=ndeficient/npeople) %>% 
  mutate(sev=ifelse(!is.na(sev), sev, sev_avg)) %>% 
  mutate(nutrient=recode(nutrient,
                         "Vitamin A (RAE)"="Vitamin A"))

# Iron key
iron_key <- sevs_orig %>% 
  select(iso3, iron_abs) %>% 
  unique() %>% 
  mutate(iron_abs=iron_abs/100)


# Build standards
################################################################################

# Nutrients
nutrs <- sort(unique(nutrients_orig$nutrient))

# Simplify nutrients data
nutrients <- nutrients_orig %>% 
  select(iso3, food_vehicle, nutrient_type, nutrient, fort_status, standard_mg_kg) %>% 
  rename(fort_status_nutr=fort_status)


# ISO3-FV-nutrient grid
iso3_fv_nutr_grid <- expand.grid(iso3=sort(unique(food_orig$iso3)),
                                 food_vehicle=sort(unique(food_orig$food_vehicle)),
                                 nutrient=nutrs) %>% 
  arrange(iso3, food_vehicle, nutrient) 

# Merge FV info and standards
data1 <- iso3_fv_nutr_grid %>% 
  # Add FV data
  left_join(food_orig, by=c("iso3", "food_vehicle")) %>% 
  # Add nutrient
  left_join(nutrients, by=c("iso3", "food_vehicle", "nutrient")) %>%
  # Fill in nutrient type
  group_by(nutrient) %>% 
  fill(nutrient_type, .direction = "downup") %>% 
  # Add missing standards
  mutate(fort_status_nutr=ifelse(is.na(fort_status_nutr), "None", fort_status_nutr),
         standard_mg_kg=ifelse(is.na(standard_mg_kg), 0, standard_mg_kg)) %>% 
  # Calculate aligned standard
  rowwise() %>% 
  mutate(aligned_standard_mg_kg=calc_aligned_standard(fv=food_vehicle,
                                                      nutr=nutrient,
                                                      intake_g_d=daily_intake_g)) %>% 
  ungroup() %>% 
  # Use current standard when there is no aligned standard
  mutate(aligned_standard_mg_kg=ifelse(is.na(aligned_standard_mg_kg), standard_mg_kg, aligned_standard_mg_kg)) %>% 
  # Add current SEV for nutrient
  left_join(sevs %>% select(iso3, nutrient, sev), by=c("iso3", "nutrient")) %>% 
  # Add iron absorption
  left_join(iron_key, by="iso3") %>% 
  # Arrange
  select(continent, region, income, country, iso3, iron_abs,
         food_vehicle, fort_status, daily_intake_type:fortified_prop_no_proxies, 
         nutrient_type, nutrient, fort_status_nutr, standard_mg_kg, aligned_standard_mg_kg, sev)

# Inspect
freeR::complete(data1)

# Check nutrient key
data1 %>% 
  count(nutrient_type, nutrient)




# Prepare data
################################################################################

# Prepare kcalorie data
# Scalar for what each group is relative to mean
kcal <- kcal_orig %>% 
  group_by(iso3) %>% 
  mutate(kcal_scalar=kcalories/mean(kcalories)) %>% 
  ungroup()

# # Prepare foood data
# food <- food_orig %>%
#   # Simplify
#   select(iso3, food_vehicle, daily_intake_g, fort_status, processed_prop, fortified_prop)
# 
# # Prepare nutrient standard
# nutrients <- nutrients_orig %>%
#   # Simplify
#   select(iso3, food_vehicle, nutrient, standard_mg_kg)


# Build master data
################################################################################

# Sex
sexes <- c("Males", 'Females')
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
isos <- freeR::uniq(food_orig$iso3)
fvs <- freeR::uniq(food_orig$food_vehicle)

# Build master data
data2 <- expand.grid(iso3=isos,
                  sex=sexes, 
                  age_group=age_groups,
                  food_vehicle=fvs,
                  nutrient=nutrs) %>% 
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
  left_join(data1) %>% 
  # Calculate country-fv-sex-age scaled intakes based on calories
  rename(daily_intake_g_avg=daily_intake_g) %>% 
  mutate(daily_intake_g=daily_intake_g_avg*kcal_scalar) %>% 
  relocate(daily_intake_g, .after=daily_intake_g_avg) %>% 
  # Daily intake in kg
  mutate(daily_intake_kg=daily_intake_g/1000) %>% 
  relocate(daily_intake_kg, .after=daily_intake_g) %>% 
  # Remove a few
  filter(nutrient!="Vitamin D") %>% 
  filter(iso3!="VAT")


# Inspect 
freeR::complete(data2)
n_distinct(data2$iso3)



# Export data
saveRDS(data2, file=file.path(outdir, "fortification_scenario_data.Rds"))




