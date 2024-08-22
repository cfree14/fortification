
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
indir <- "data/intake_proxy/raw"
outdir <- "data/intake_proxy/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "food_proxy_FBS_age_sex_040124.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(iso3=region) %>% 
  # Add country
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  # Format food
  mutate(food_group=gsub("_", " ", food_group) %>% stringr::str_to_sentence(),
         food_group=recode(food_group,
                           "All-fg"="All food groups",
                           "Fat ani"="Animal fat",
                           "Fish demrs"='Fish, demersal',
                           "Fish freshw"="Fish, freshwater",
                           "Fish other"="Fish, other",
                           "Fish pelag"="Fish, pelagic",
                           "Fruits starch"="Fruits, starch",
                           "Fruits temp"="Fruits, temperate",
                           "Fruits trop"="Fruits, tropical",
                           "Oil palm"="Oil, palm",
                           "Oil veg"="Oil, vegetable",
                           "Othr grains"="Other grains",
                           "Othr meat"="Other meat")) %>% 
  # Format sex
  mutate(sex=recode(sex,
                    "BTH"="both",
                    "FML"="female",
                    "MLE"="male")) %>% 
  # Format age
  mutate(age=recode(age,
                    "43739"="10-19", 
                    "all-a"="all ages"),
         age=factor(age, levels=c("0-9", "10-19", "20-39", "40-64", "65+", "all ages"))) %>% 
  # Format unit
  mutate(unit=gsub("_w", "", unit)) %>% 
  # Arrange
  select(iso3, country, food_group, unit, sex, age, year, everything()) %>% 
  arrange(iso3, food_group, sex, age, year)

# Inspect 
str(data)

table(data$food_group)
table(data$unit)
table(data$sex)
table(data$year)
table(data$age)

# Country key
cntry_key <- data %>% 
  count(iso3, country)

# Food key
food_key <- data %>% 
  count(food_group, unit)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "intake_data.Rds"))
