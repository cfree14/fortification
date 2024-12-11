
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
datadir1 <- "data/intake_proxy/processed"
datadir2 <- "output"
plotdir <- "figures"

# Read data
intake <- readRDS(file=file.path(datadir1, "intake_data_use.Rds"))
gfdx <- readRDS(data, file=file.path(datadir2, "gfdx_data_imputed_full.Rds"))


# Build data
################################################################################

# ISOs to fill
isos_in_gfdx <- freeR::uniq(gfdx$iso3)
isos_in_cal <- freeR::uniq(intake$iso3)
isos_missing <- isos_in_gfdx[!isos_in_gfdx%in%isos_in_cal]

# Read key 
key <- readxl::read_excel("tables/TableX_intake_proxy_key.xlsx", skip=1) %>% 
  setNames(c("iso_without", "country_without", "iso_with", "country_with"))

# Build data for countries w/ intakes 
intake_with <- intake %>% 
  # Rename
  rename(kcalories=value) %>% 
  # Supplement
  mutate(source="Provided",
         source_iso3=iso3,
         source_country=country) %>% 
  # Simplify
  select(region, iso3, country, sex, age, value, source, source_iso3, source_country) 

# Build data for countries w/out intakes 
x <- 1
intake_without <- purrr::map_df(1:nrow(key), function(x){
  
  # ISOs
  iso_with <- key$iso_with[x]
  iso_without <- key$iso_without[x]
  cntry_without <- key$country_without[x]
  
  # Subset relevant data
  sdata <- intake_with %>% 
    filter(iso3==iso_with) %>% 
    mutate(source="Borrowed",
           iso3=iso_without,
           country=cntry_without)
  
  
})

# Merge
intake_out <- bind_rows(intake_with, intake_without)

# Check
isos_in_out <- freeR::uniq(intake_out$iso3)
isos_in_gfdx[!isos_in_gfdx%in%isos_in_out]

# Export
saveRDS(intake_out, file=file.path(datadir1, "intake_data_imputed.Rds"))






