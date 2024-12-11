

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/global_intake_inadequacies/output/2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds")

# Contry list
key <- data_orig %>% 
  filter(!is.na(ndeficient)) %>% 
  count(continent, region, country, iso3)
