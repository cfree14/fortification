
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
data_all <- readRDS(file=file.path(outdir, "fortification_scenario_output.Rds"))
data_iron <-  readRDS(file=file.path(outdir, "fortification_scenario_output_iron.Rds"))


# Build data
################################################################################

# Reproductive ages
repro_age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

# Reduce iron
data_iron_use <- data_iron %>% 
  filter(sex=="Females" & age_group %in% repro_age_groups)

# Reduce all
data_all_use <- data_all %>% 
  filter(! (nutrient=="Iron" & sex=="Females" & age_group %in% repro_age_groups) )

# Merge
data <- bind_rows(data_all_use, data_iron_use) %>% 
  arrange(nutrient, iso3, sex, age_group )

nrow(data) == nrow(data_all)


# Export data
################################################################################


# Export data
saveRDS(data, file=file.path(outdir, "fortification_scenario_output_final.Rds"))
write.csv(data, file=file.path(outdir, "fortification_scenario_output_final.csv"), row.names=F)


