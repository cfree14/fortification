
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
supply_orig <- readRDS(file.path(outdir, "gfdx_data_imputed_simple.Rds"))
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))
kcal_orig <- readRDS("data/intake_proxy/processed/intake_data_imputed.Rds")


# Read data
################################################################################

# Prepare kcalorie data
# Scalar for what each group is relative to mean
kcal <- kcal_orig %>% 
  group_by(iso3) %>% 
  mutate(kcal_scalar=kcalories/mean(kcalories)) %>% 
  ungroup()

# Prepare supply data
supply <- supply_orig %>% 
  # Simplify
  select(iso3, food_vehicle, daily_intake_g, processed_prop, fortified_prop)

# Merge data
data <- nutrients_orig %>% 
  # Simplify
  select(-year_nutrients) %>% 
  # Add supply info
  left_join(supply)

# Eliminate data still missing assuming that these are fake programs
data1 <- na.omit(data)


# Build master data
################################################################################

# Sex
sexes <- c("Males", 'Females')
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-39", 
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
isos <- freeR::uniq(data1$iso3)
fvs <- freeR::uniq(supply$food_vehicle)

# Build master data
df <- expand.grid(iso3=isos,
                  food_vehicle=fvs,
                  sex=sexes, 
                  age_group=age_groups) %>% 
  # Factor age group
  mutate(age_group=factor(age_group)) %>% 
  # Add calorie age group
  mutate(age_group_kcal=case_when(age_group %in% c("0-4", "5-9") ~ "0-9",
                                  age_group %in% c("10-14", "15-19") ~ "10-19",
                                  age_group %in% c("20-24", "25-29", "30-39") ~ "20-39",
                                  age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64") ~ "40-64",
                                  age_group %in% c("65-69", "70-74", "75-79", "80+") ~ "65+",
                                  T ~ NA)) %>% 
  # Add kilocalories
  left_join(kcal %>% select(iso3, sex, age, kcalories, kcal_scalar), by=c("iso3", "sex", "age_group_kcal"="age")) %>% 
  # Arrange
  arrange(iso3, food_vehicle, sex, age_group) %>% 
  # Add food vehicle stats
  left_join(supply) %>% 
  # Reduce to only fortified food vehicles
  filter(!is.na(daily_intake_g)) %>% 
  # Calculate country-fv-sex-age scaled intakes based on calories
  rename(daily_intake_g_avg=daily_intake_g) %>% 
  mutate(daily_intake_g=daily_intake_g_avg*kcal_scalar) %>% 
  relocate(daily_intake_g, .after=daily_intake_g_avg)

# Inspect 
freeR::complete(df)

# Add standards and then you are ready!






