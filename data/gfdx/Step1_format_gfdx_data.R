
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/gfdx/raw"
outdir <- "data/gfdx/processed"

# Read nutrient key
nutr_key <- readxl::read_excel("data/nutrient_key.xlsx")


# Format data
################################################################################

# Sheets
sheets <- c("Salt", "Maize flour", "Oil", "Rice", "Wheat flour")

# Merge data
data_orig <- purrr::map_df(sheets, function(x){
  
  # Read data
  fdata_orig <- readxl::read_excel(file.path(indir, "Food Intake and Availability_20240730.xlsx"), sheet=x)
  
})

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(income=income_status,
         year=data_year, 
         daily_intake_g=daily_food_intake_availability_g_c_d,
         supply_mt=total_food_available_mt,
         supply_comment=intake_availability_comment,
         supply_source=intake_availability_source) %>% 
  # Add ISO3
  mutate(country=recode(country, 
                        "Micronesia"="Federated States of Micronesia"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  # Arrange
  select(country, iso3, everything()) %>% 
  arrange(country, food_vehicle, year)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$income)
table(data$food_vehicle)
table(data$legislation_status)
range(data$year)
table(data$data_type)
freeR::uniq(data$supply_source)
freeR::uniq(data$supply_comment)

# Country key
country_key <- data %>% 
  count(country, iso3)

# Export
saveRDS(data, file=file.path(outdir, "GFDX_1990_2022_vehicle_supply_and_intake.Rds"))


# Format data
################################################################################

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Number of Food Vehicles with Standards_20240729.xlsx"), na="(Blanks)")

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(income=income_status,
         year=standard_year,
         standard_mg_kg=nutrient_level_in_standard_mg_kg) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c"),
         iso3=case_when(country=="Kosovo" ~ "XKX", T ~ iso3)) %>% 
  # Add nutrient type
  left_join(nutr_key %>% select(nutrient_orig, nutrient_type), by=c("nutrient"="nutrient_orig")) %>% 
  # Arrange
  select(country, iso3, everything()) %>% 
  relocate(nutrient_type, .before=nutrient) %>% 
  arrange(country, food_vehicle, nutrient, year)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$income)
table(data$region)
table(data$food_vehicle)
table(data$legislation_status)
table(data$nutrient)
table(data$year)

# Country key
cntry_key <- data %>% 
  count(country, iso3, region, income)
freeR::which_duplicated(cntry_key$country)
freeR::which_duplicated(cntry_key$iso3)


# Export
saveRDS(data, file=file.path(outdir, "GFDX_1960_2022_nutrient_content_in_foods.Rds"))


# Format data
################################################################################

# Sheets
sheets <- c("Salt", "Maize flour", "Oil", "Rice", "Wheat flour")

# Merge data
data_orig <- purrr::map_df(sheets, function(x){
  
  # Read data
  fdata_orig <- readxl::read_excel(file.path(indir, "Proportion of Fortified Food Vehicle_20240730.xlsx"), sheet=x)
  
})

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(income=income_status,
         year=data_year,
         fortified_mt=quantity_fortified_mt,
         fortified_prop=proportion_fortified_percent,
         source=proportion_fortified_source,
         comment=proportion_fortified_comment) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c"),
         iso3=case_when(country=="Kosovo" ~ "XKX", T ~ iso3)) %>% 
  # Arrange
  select(country, iso3, everything()) #%>% 
  # arrange(country, food_vehicle, year)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
range(data$year)
table(data$income)
table(data$region)
table(data$food_vehicle)
table(data$legislation_status)
table(data$data_type)

# Export
saveRDS(data, file=file.path(outdir, "GFDX_2004_2024_food_fortification_percents.Rds"))


# Format data
################################################################################

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Proportion of Industrially Processed Food Vehicle_20240729.xlsx"))

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(income=income_status,
         year=data_year,
         processed_mt=quantity_industrially_processed_mt,
         processed_prop=proportion_industrially_processed_percent,
         source=industrially_processed_source,
         comment=industrially_processed_comment) %>%
  # Add ISO3
  mutate(country=recode(country, 
                        "Micronesia"="Federated States of Micronesia"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c"),
         iso3=case_when(country=="Kosovo" ~ "XKX", 
                        T ~ iso3)) %>% 
  # Arrange
  select(country, iso3, everything()) #%>% 
# arrange(country, food_vehicle, year)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
range(data$year)
table(data$income)
table(data$region)
table(data$food_vehicle)
table(data$legislation_status)
table(data$data_type)

# Export
saveRDS(data, file=file.path(outdir, "GFDX_2008_2024_industrially_processed_percents.Rds"))


# Format data
################################################################################

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Micronutrient Compound_20240722.xlsx"), col_types = "text")

# Format data
data <- data_orig %>% 
  # Remove garbage extra columns
  select(-c("...13", "...14")) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(income=income_status,
         nutrient_level=nutrient_level_from_fortification_standard,
         indicator_value=indicator_value22) %>% 
  # Convert to numeric
  mutate_at(vars(c(year)), as.numeric) %>% 
  mutate(nutrient_level=gsub(",", "", nutrient_level),
         nutrient_level_num=as.numeric(nutrient_level)) %>% 
  mutate(indicator_value=gsub(",", "", indicator_value),
         indicator_value_num=as.numeric(indicator_value)) %>%  
  # Format nutrient
  mutate(nutrient=gsub(" level", "", nutrient),
         nutrient=recode(nutrient, 
                         "B12"="Vitamin B12",
                         "B6"="Vitamin B6")) %>% 
  # Add ISO3
  mutate(country=recode(country, 
                        "Micronesia"="Federated States of Micronesia"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c"),
         iso3=case_when(country=="Kosovo" ~ "XKX", 
                        T ~ iso3)) %>% 
  # Arrange
  select(country, iso3, region, indicator, food_vehicle, 
         compound, nutrient, unit, year, 
         nutrient_level, nutrient_level_num, 
         indicator_value, indicator_value_num, source, everything())

# Inspect
str(data)
freeR::complete(data)

# Numeric
freeR::uniq(data$nutrient_level)
check1 <- data %>% 
  count(nutrient_level, nutrient_level_num)
check2 <- data %>% 
  count(indicator_value, indicator_value_num)

# More
range(data$year, na.rm=T)
table(data$income)
table(data$region)
table(data$unit)
table(data$nutrient)
table(data$compound)
table(data$year)

# Country key
cntry_key <- data %>% 
  count(country, iso3, region, income)
freeR::which_duplicated(cntry_key$country)
freeR::which_duplicated(cntry_key$iso3)


# Nutrient key (not sure what this tells me yet)
nutr_key1 <- data %>%
  count(nutrient, compound)
freeR::which_duplicated(nurt_key$compound)

# Export data
saveRDS(data, file=file.path(outdir, "GFDX_1940_2024_compound_levels.Rds"))

