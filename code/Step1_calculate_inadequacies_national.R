
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

# Read global intake inadequacies
data_orig <- readRDS("data/global_intake_inadequacies/2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds")


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  group_by(nutrient, country, iso3) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(pdeficient=ndeficient/npeople)

# Export
write.csv(data, file=file.path("data/global_intake_inadequacies/2018_national_nutrient_intake_inadequacy.csv"), row.names=F)
