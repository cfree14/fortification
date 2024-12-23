
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# NRVs 
nrvs_orig <- nutriR::nrvs

# ULs
uls <- nrvs_orig %>% 
  filter(nrv_type=="Upper limit")

# UL source
uls %>% 
  count(nutrient, source)

# Build data
data <- nrvs_orig %>% 
  # Filter
  filter(nrv_type %in% c("Upper limit", "Average requirement")) %>% 
  filter(stage %in% c("Children", "Males", "Females")) %>% 
  # Simplify
  select(nutrient, units, sex, stage, age_group, nrv_type, nrv) %>% 
  mutate(nrv_type=recode(nrv_type,
                         "Upper limit"="ul",
                         "Average requirement"="ar")) %>% 
  # Spread
  spread(key="nrv_type", value="nrv")


# Read data
################################################################################








