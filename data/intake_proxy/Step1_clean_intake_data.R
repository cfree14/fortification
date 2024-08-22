
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

# WHO
world <- "WLD"
wb_regions <- c("HIC", "UMC", "LMC", "LIC")
who_regions <- c("NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF")

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(iso3=region) %>% 
  # Add country
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  # Add region type
  mutate(region_type=case_when(!is.na(country) ~ "country", 
                               iso3 %in% wb_regions ~ "WB income",
                               iso3 %in% who_regions ~ "WHO region",
                               iso3 == "WLD" ~ "world",
                               T ~ NA)) %>% 
  # Fill in missing countries
  mutate(country=ifelse(!is.na(country), country, iso3),
         country=recode(country,
                        "HIC"="High income country", 
                        "UMC"="Upper middle income country", 
                        "LMC"="Lower middle income country", 
                        "LIC"="Low income country",
                        # WHO regions
                        "NAC"="North America", 
                        "LCN"="Latin America and the Caribbean", 
                        "ECS"="Europe and Central Asia",
                        "MEA"="Middle East and North Africa",
                        "SAS"="South Asia",
                        "EAS"="East Asia and the Pacific", 
                        "SSF"="Sub-Saharan Africa",
                        "WLD"="World")) %>% 
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
  select(region_type, iso3, country, food_group, unit, sex, age, year, everything()) %>% 
  arrange(region_type, iso3, food_group, sex, age, year)

# Inspect 
str(data)
freeR::complete(data)

table(data$food_group)
table(data$unit)
table(data$sex)
table(data$year)
table(data$age)

# Country key
cntry_key <- data %>% 
  count(region_type, iso3, country)

# Food key
food_key <- data %>% 
  count(food_group, unit)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "intake_data.Rds"))
