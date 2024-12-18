
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
scen_data <- readRDS(file=file.path(outdir, "fortification_scenario_data.Rds"))

# Read global intake inadequacies
data_orig <- readRDS("data/global_intake_inadequacies/2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds")

# Check totals
check <- data_orig %>% 
  group_by(nutrient) %>% 
  summarize(ndeficient=sum(ndeficient, na.rm=T)/1e9) %>% 
  arrange(desc(ndeficient))


# Format data
################################################################################

# Steps
# 1. Format baseline data
# 2. Format fortification subsidies
# 3. Add subsidies to baseline
# 4. Split into ones that need to be updated and ones that dont
# 5. Split ones that need to be updated into gamma lognormal
# 6. Merge all

# Step 1. Format baseline data
#############################################

# Nutrients with fort
nutr_do <- freeR::uniq(scen_data$nutrient)

# Build base data
data0 <- data_orig %>% 
  # Format Vitamin A name
  mutate(nutrient=recode(nutrient,
                         "Vitamin A (RAE)"="Vitamin A")) %>% 
  # Reduce to fortified nutrients
  filter(nutrient %in% nutr_do) %>% 
  filter(!nutrient %in% c("Fluoride", "Vitamin D")) %>% 
  # Rename (Scenario 0 = original)
  rename(age_group=age_range,
         sev0=sev,
         ndeficient0=ndeficient,
         intake0=supply_med) %>% 
  # Build ISO3-nutrient combo
  mutate(iso3nutr=paste(iso3, nutrient, sep="-")) %>%
  # Clean up units for simplicity
  mutate(units=recode(units, 
                      "µg DFE"="ug",
                      "µg RAE"="ug"),
         ar_units=recode(ar_units, 
                         "µg DFE"="ug",
                         "µg RAE"="ug",
                         "mg a-tocopherol"="mg")) %>% 
  # Simplify
  select(iso3nutr, nutrient_type, nutrient, units, 
         ar_units, ar, ar_cv, 
         country, iso3, sex, age_group, npeople, 
         best_dist, g_rate, g_shape, ln_meanlog, ln_sdlog,
         intake0, sev0, ndeficient0) 

# Check
freeR::complete(data0)
table(data0$units)
table(data0$ar_units)


# Step 2. Format fortification subsides
#############################################

# Calculate subsidies resulting from scenarios
subs <- scen_data %>%
  # Calculate Scenario 1 subsidy
  mutate(subsidy_mg1=daily_intake_kg*processed_prop*fortified_prop*standard_mg_kg) %>% 
  # Calcultae Scenario 2 subsidy
  mutate(fortified_prop2=pmax(fortified_prop, 0.9)) %>% 
  mutate(subsidy_mg2=daily_intake_kg*processed_prop*fortified_prop2*standard_mg_kg) %>% 
  # Summarize subsidies across fortified food vehicles
  group_by(iso3, sex, age_group, nutrient) %>% 
  summarize(subsidy_mg1=sum(subsidy_mg1),
            subsidy_mg2=sum(subsidy_mg2)) %>% 
  ungroup() %>% 
  # Build ISO3-nutrient combo
  mutate(iso3nutr=paste(iso3, nutrient, sep="-")) %>% 
  # Remove fluoride (not analyzed b/c not in Passerelli)
  filter(nutrient!="Fluoride")

freeR::complete(subs)

# Confirm that original data exists for all fortification programs
iso3nutr_og <- freeR::uniq(data0$iso3nutr)
iso3nutr_subs <- freeR::uniq(subs$iso3nutr)
iso3nutr_subs[!iso3nutr_subs %in% iso3nutr_og]


# Step 3. Add subsidies to baseline
#############################################

# Add subsidies to base data
data1 <- data0 %>% 
  # Add subsidies
  left_join(subs, by=c("iso3nutr", "nutrient", "iso3", "sex", "age_group")) %>% 
  # Set missing subsidies to zero
  mutate(subsidy_mg1=ifelse(is.na(subsidy_mg1), 0, subsidy_mg1),
         subsidy_mg2=ifelse(is.na(subsidy_mg2), 0, subsidy_mg2)) %>% 
  # Convert subsidies (mg) to target units
  mutate(subsidy1=case_when(units=="mg" ~ subsidy_mg1,
                           units=="ug" ~ subsidy_mg1*1000, 
                           T ~ NA),
         subsidy2=case_when(units=="mg" ~ subsidy_mg2,
                            units=="ug" ~ subsidy_mg2*1000, 
                            T ~ NA)) %>% 
  # Add subsidy
  mutate(intake1=intake0+subsidy1,
         intake2=intake0+subsidy2) %>% 
  # Record whether there is any fortification (update with each scenario added)
  mutate(fortification_yn=ifelse(subsidy1 > 0 | subsidy2 > 0, "yes", "no"))
  
freeR::complete(data1)


# Step 4. Split into fortified/unfortified
#############################################

# Unfortified
data1_unfort <- data1 %>% 
  filter(fortification_yn=="no")

# Fortified
data1_fort <- data1 %>% 
  filter(fortification_yn=="yes")

# Unfortified
data1_unfort_expanded <- data1_unfort %>% 
  mutate(sev1=sev0,
         sev2=sev0,
         ndeficient1=ndeficient0,
         ndeficient2=ndeficient0)
  

# Step 5. Split ones needing new calculations and calculate SEVs 
#############################################

# Break into gamma and lognormal then remerge
data1_fort_ln <- data1_fort %>% 
  # Log-normal
  filter(best_dist=="log-normal") %>% 
  # Shift parameters
  rowwise() %>%
  mutate(ln_meanlog1=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake1, plot=F)$meanlog,
         ln_sdlog1=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake1, plot=F)$sdlog,
         ln_meanlog2=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake2, plot=F)$meanlog,
         ln_sdlog2=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake2, plot=F)$sdlog) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev1=nutriR::sev(ear = ar, cv = ar_cv, meanlog=ln_meanlog1, sdlog=ln_sdlog1, plot=F),
         sev2=nutriR::sev(ear = ar, cv = ar_cv, meanlog=ln_meanlog2, sdlog=ln_sdlog2, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadeuate intakes
  mutate(ndeficient1=npeople*sev1/100,
         ndeficient2=npeople*sev2/100)

# Break into gamma and lognormal then remerge
data1_fort_g <- data1_fort %>% 
  # Log-normal
  filter(best_dist=="gamma") %>% 
  # Shift parameters
  rowwise() %>%
  mutate(g_shape1=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake1, plot=F)$shape,
         g_rate1=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake1, plot=F)$rate,
         g_shape2=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake2, plot=F)$shape,
         g_rate2=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake2, plot=F)$rate) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev1=nutriR::sev(ear = ar, cv = ar_cv, shape=g_shape1, rate=g_rate1, plot=F),
         sev2=nutriR::sev(ear = ar, cv = ar_cv, shape=g_shape2, rate=g_rate2, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadeuate intakes
  mutate(ndeficient1=npeople*sev1/100,
         ndeficient2=npeople*sev2/100)


# Step 6. Merge
#############################################

# Merge
data2 <- bind_rows(data1_unfort_expanded, data1_fort_g, data1_fort_ln) %>% 
  # Arrange
  arrange(nutrient_type, nutrient, iso3, sex, age_group)

# Inspect
freeR::complete(data2)

# SOME DAY FIGURE OUT WHY SOME SEV CALCS DON'T WORK


# Export
#############################################

# Export data
saveRDS(data2, file=file.path(outdir, "fortification_scenario_output.Rds"))

