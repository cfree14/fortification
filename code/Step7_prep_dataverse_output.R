
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
dvdir <- "output/dataverse"

# Read data
data_full <- readRDS(file=file.path(outdir, "fortification_scenario_output_final.Rds"))


# Export full
################################################################################

saveRDS(data_full, file=file.path(dvdir, "fortification_full.Rds"))
write.csv(data_full, file=file.path(dvdir, "fortification_full.csv"), row.names = F)
openxlsx::write.xlsx(data_full, file=file.path(dvdir, "fortification_full.xlsx"))


# Reduce
################################################################################

data_simp <- data_full %>% 
  select(nutrient_type, nutrient, units,
         ar_source, ar, ar_cv,
         ul_source, ul,
         country, iso3, sex, age_group, npeople,
         subsidy1, subsidy2, subsidy3, subsidy4, subsidy5,
         intake0, intake1, intake2, intake3, intake4, intake5,
         sev0, sev1, sev2, sev3, sev4, sev5,
         ndeficient0, ndeficient1, ndeficient2, ndeficient3, ndeficient4, ndeficient5,
         ul0, ul1, ul2, ul3, ul4, ul5)

saveRDS(data_simp, file=file.path(dvdir, "fortification_reduced.Rds"))
write.csv(data_simp, file=file.path(dvdir, "fortification_reduced.csv"), row.names = F)
openxlsx::write.xlsx(data_simp, file=file.path(dvdir, "fortification_reduced.xlsx"))

