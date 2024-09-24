
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
supply_orig <- readRDS(file.path(outdir, "gfdx_data_imputed_full.Rds"))
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))


# Read data
################################################################################

# Prepare supply data
supply <- supply_orig %>% 
  # Simplify
  select(iso3, food_vehicle, daily_intake_g, processed_prop, fortified_prop)

# Build data
data <- nutrients_orig %>% 
  select(-year_nutrients) %>% 
  left_join(supply)

# Inspect
freeR::complete(data)




