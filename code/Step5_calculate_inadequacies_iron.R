
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


# Build upper limits key
################################################################################

# Get upper limits
nrvs_orig <- nutriR::nrvs

# ULs
uls <- nrvs_orig %>% 
  # ULs
  filter(nrv_type=="Upper limit") %>% 
  # Format nutrient
  mutate(nutrient=recode(nutrient, 
                         "Vitamin B-6"="Vitamin B6")) %>% 
  # Nutrients of interest
  filter(nutrient %in% scen_data$nutrient) %>% 
  # Filter to relevant stage
  filter(!stage %in% c("Lactation", "Pregnancy")) %>% 
  # Rename
  rename(ul=nrv,
         ul_units=units,
         ul_source=source,) %>% 
  # Simplify
  select(-c(nrv_type, nrv_note)) %>% 
  # Average "4-6 y"   "51-70 y" "7-10 y" 
  mutate(age_group=recode(age_group,
                          "4-6 y"="4-10 yr",
                          "7-10 y"="4-10 yr")) %>% 
  group_by(nutrient, ul_source, ul_units, sex, stage, age_group) %>% 
  summarize(ul=mean(ul, na.rm = T)) %>% 
  ungroup() %>% 
  # Format units
  mutate(ul_units=recode(ul_units,
                         "mg a-tocopherol"="mg",
                         "ug retinol"="ug")) %>%
  # SImplify
  select(-stage) %>% 
  # Remove Vitamin A b/c UL in retinal and AR in RAE
  filter(nutrient!="Vitamin A")

# Inspect
freeR::uniq(uls$nutrient)
freeR::uniq(uls$age_group)
freeR::uniq(uls$ul_units)


# Format data
################################################################################

# Steps
# 1. Format intake distributions
# 2. Format fortification subsidies
# 3. Add subsidies to intake distributions
# 4. Split into intakes that are and are not fortified
# 5. For unfortified, calculate P(>UL) for gamma/log-normal
# 5. For fortified, shift distributions, calculate SEVs and P(>UL) for g/ln
# 6. Merge all

# Step 1. Format intake distributions
#############################################

# Nutrients with fort
nutr_do <- freeR::uniq(scen_data$nutrient)

# Build base data
data0 <- data_orig %>% 
  # Reduce to Iron
  filter(nutrient %in% "Iron") %>% 
  # Overwrite AR CV
  mutate(ar_cv=0.44) %>% 
  # Rename (Scenario 0 = original)
  rename(age_group=age_range,
         sev0=sev,
         ndeficient0=ndeficient,
         intake0=supply_med,
         g_rate0=g_rate_shift,
         g_shape0=g_shape_shift, 
         ln_meanlog0=ln_meanlog_shift,
         ln_sdlog0=ln_sdlog_shift) %>% 
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
  # Add UL
  mutate(age_group_ul=case_when(age_group %in% c("0-4") ~ "1-3 y", 
                                age_group %in% c("5-9") ~ "4-10 yr", # average "4-6 y", "7-10 y",
                                age_group %in% c("10-14") ~ "11-14 y", 
                                age_group %in% c("15-19") ~ "15-17 y", 
                                age_group %in% c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49") ~ "18-50 y", 
                                age_group %in% c("50-54", "55-59", "60-64", "65-69") ~ "51-70 y", 
                                age_group %in% c("70-74", "75-79", "80+") ~ ">70 y",
                                T ~ NA),
         sex_ul=ifelse(age_group %in% c("0-4", "5-9"), "Children", sex)) %>%
  left_join( uls, by=c("nutrient", "sex_ul"="sex", "age_group_ul"="age_group")) %>% 
  # Simplify
  select(iso3nutr, nutrient_type, nutrient, units, 
         ar_source, ar_units, ar, ar_cv, 
         ul_source, ul_units, ul,
         country, iso3, sex, age_group, npeople, 
         best_dist, g_rate, g_shape, ln_meanlog, ln_sdlog,
         g_rate0, g_shape0, ln_meanlog0, ln_sdlog0,
         intake0, sev0, ndeficient0) 

# Check
freeR::complete(data0)
table(data0$units)
table(data0$ar_units)
freeR::uniq(data0$age_group)


# AR and UL units same? Must be 0
sum(data0$ar_units!=data0$ul_units, na.rm=T)


# Step 2. Format fortification subsidies
#############################################

# Calculate subsidies resulting from scenarios
subs <- scen_data %>%
  # Mark whether program is on now or when expanded
  mutate(now_on=ifelse(fort_status_nutr!="None", 1, 0),
         enough_consumed_yn = case_when(food_vehicle=="Salt" & daily_intake_g_avg>=3 ~ T, 
                                         food_vehicle %in% c("Wheat flour", "Maize flour", "Rice") & daily_intake_g_avg>=50 ~ T,
                                         food_vehicle=="Oil" & daily_intake_g_avg>=10 ~ T,
                                         T ~ F), 
         future_on=ifelse(now_on==1 | (sev>=0.20 & enough_consumed_yn & processed_prop >=0.50), 1, 0)) %>% 
  # Mark whether subsidy is reduced because of bioavailability
  mutate(bioavail_scalar=ifelse(nutrient!="Iron", 1, iron_abs/0.1)) %>% 
  # Calculate Scenario 1 (currnet) subsidy
  mutate(subsidy_mg1=now_on*daily_intake_kg*processed_prop*fortified_prop*standard_mg_kg/bioavail_scalar) %>% 
  # Calculate Scenario 2 (improved compliance) subsidy
  mutate(fortified_prop2=pmax(fortified_prop, 0.9)) %>% 
  mutate(subsidy_mg2=now_on*daily_intake_kg*processed_prop*fortified_prop2*standard_mg_kg/bioavail_scalar) %>% 
  # Calculate aligned standard to use
  mutate(aligned_standard_mg_kg_use=pmax(aligned_standard_mg_kg, standard_mg_kg)) %>% 
  # Calculate Scenario 3 (aligned std, current compliance) subsidy
  mutate(subsidy_mg3=now_on*daily_intake_kg*processed_prop*fortified_prop*aligned_standard_mg_kg_use/bioavail_scalar) %>% 
  # Calculate Scenario 4 (aligned std, improved compliance) subsidy
  mutate(subsidy_mg4=now_on*daily_intake_kg*processed_prop*fortified_prop2*aligned_standard_mg_kg_use/bioavail_scalar) %>% 
  # Calculate Scenario 5 (aligned std, improved compliance) subsidy for more countries
  mutate(subsidy_mg5=future_on*daily_intake_kg*processed_prop*fortified_prop2*aligned_standard_mg_kg_use/bioavail_scalar) %>% 
  # Summarize subsidies across fortified food vehicles
  group_by(iso3, sex, age_group, nutrient) %>% 
  summarize(subsidy_mg1=sum(subsidy_mg1),
            subsidy_mg2=sum(subsidy_mg2),
            subsidy_mg3=sum(subsidy_mg3),
            subsidy_mg4=sum(subsidy_mg4),
            subsidy_mg5=sum(subsidy_mg5)) %>% 
  ungroup() %>% 
  # Build ISO3-nutrient combo
  mutate(iso3nutr=paste(iso3, nutrient, sep="-")) %>% 
  # Remove fluoride (not analyzed b/c not in Passerelli)
  filter(nutrient!="Fluoride")

# Inspect
freeR::complete(subs)

# Confirm that original data exists for all fortification programs
iso3nutr_og <- freeR::uniq(data0$iso3nutr)
iso3nutr_subs <- freeR::uniq(subs$iso3nutr)
iso3nutr_subs[!iso3nutr_subs %in% iso3nutr_og]


# Step 3. Add subsidies to intake distributions
#############################################

# Add subsidies to base data
data1 <- data0 %>% 
  # Add subsidies
  left_join(subs, by=c("iso3nutr", "nutrient", "iso3", "sex", "age_group")) %>% 
  # Set missing subsidies to zero
  mutate(subsidy_mg1=ifelse(is.na(subsidy_mg1), 0, subsidy_mg1),
         subsidy_mg2=ifelse(is.na(subsidy_mg2), 0, subsidy_mg2),
         subsidy_mg3=ifelse(is.na(subsidy_mg3), 0, subsidy_mg3),
         subsidy_mg4=ifelse(is.na(subsidy_mg4), 0, subsidy_mg4),
         subsidy_mg5=ifelse(is.na(subsidy_mg5), 0, subsidy_mg5)) %>% 
  # Convert subsidies (mg) to target units
  mutate(subsidy1=case_when(units=="mg" ~ subsidy_mg1,
                           units=="ug" ~ subsidy_mg1*1000, 
                           T ~ NA),
         subsidy2=case_when(units=="mg" ~ subsidy_mg2,
                            units=="ug" ~ subsidy_mg2*1000, 
                            T ~ NA),
         subsidy3=case_when(units=="mg" ~ subsidy_mg3,
                            units=="ug" ~ subsidy_mg3*1000, 
                            T ~ NA),
         subsidy4=case_when(units=="mg" ~ subsidy_mg4,
                            units=="ug" ~ subsidy_mg4*1000, 
                            T ~ NA),
         subsidy5=case_when(units=="mg" ~ subsidy_mg5,
                            units=="ug" ~ subsidy_mg5*1000, 
                            T ~ NA)) %>% 
  # Add subsidy
  mutate(intake1=intake0+subsidy1,
         intake2=intake0+subsidy2,
         intake3=intake0+subsidy3,
         intake4=intake0+subsidy4,
         intake5=intake0+subsidy5) %>% 
  # Record whether there is any fortification (update with each scenario added)
  mutate(fortification_yn=ifelse(subsidy1 > 0 | subsidy2 > 0 | subsidy3 > 0 | subsidy4 > 0 | subsidy5 > 0, "yes", "no")) %>% 
  # Record original SEV
  mutate(sev0_norm=sev0)

# Inspect
freeR::complete(data1)


# Step 4. Split into fortified/unfortified and process unfortified
#############################################

# Fortified
data1_fort <- data1 %>% 
  filter(fortification_yn=="yes")

# Unfortified
data1_unfort <- data1 %>% 
  filter(fortification_yn=="no")

# Unfortified-gamma
data1_unfort_g <- data1_unfort %>% 
  # Filter
  filter(best_dist=="gamma") %>% 
  # Calculate P(>UL)
  rowwise() %>%
  mutate(ul0=nutriR::above_ul(ul = ul, shape = g_shape0, rate=g_rate0)) %>%
  ungroup() %>% 
  mutate(ul1=ul0,
         ul2=ul0,
         ul3=ul0,
         ul4=ul0,
         ul5=ul0) %>% 
  # Recalculate SEV
  rowwise() %>% 
  mutate(sev0=nutriR::sev(ear = ar, 
                          cv = ar_cv,
                          ear_log=T,
                          shape = g_shape0, 
                          rate=g_rate0,
                          plot=F),
         ndeficient0=npeople*sev0/100) %>% 
  ungroup()

# Unfortified-lognormal
data1_unfort_ln <- data1_unfort %>% 
  # Filter
  filter(best_dist=="log-normal") %>% 
  # Calculate P(>UL)
  rowwise() %>%
  mutate(ul0=nutriR::above_ul(ul = ul, meanlog = ln_meanlog0, sdlog=ln_sdlog0)) %>%
  ungroup() %>% 
  mutate(ul1=ul0,
         ul2=ul0,
         ul3=ul0,
         ul4=ul0,
         ul5=ul0) %>% 
  # Recalculate SEV without fort
  rowwise() %>% 
  mutate(sev0=nutriR::sev(ear = ar, 
                          cv = ar_cv,
                          ear_log=T,
                          meanlog = ln_meanlog0, 
                          sdlog=ln_sdlog0,
                          plot=F),
         ndeficient0=npeople*sev0/100) %>% 
  ungroup()

# Unfortified
data1_unfort_expanded <- bind_rows(data1_unfort_g, data1_unfort_ln) %>% 
  mutate(sev1=sev0,
         sev2=sev0,
         sev3=sev0,
         sev4=sev0,
         sev5=sev0,
         ndeficient1=ndeficient0,
         ndeficient2=ndeficient0,
         ndeficient3=ndeficient0,
         ndeficient4=ndeficient0,
         ndeficient5=ndeficient0)


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
         # Scenario 2
         ln_meanlog2=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake2, plot=F)$meanlog,
         ln_sdlog2=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake2, plot=F)$sdlog,
         # Scenario 3
         ln_meanlog3=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake3, plot=F)$meanlog,
         ln_sdlog3=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake3, plot=F)$sdlog,
         # Scenario 4
         ln_meanlog4=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake4, plot=F)$meanlog,
         ln_sdlog4=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake4, plot=F)$sdlog,
         # Scenario 5
         ln_meanlog5=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake5, plot=F)$meanlog,
         ln_sdlog5=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=intake5, plot=F)$sdlog) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev0=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog0, sdlog=ln_sdlog0, plot=F),
         sev1=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog1, sdlog=ln_sdlog1, plot=F),
         sev2=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog2, sdlog=ln_sdlog2, plot=F),
         sev3=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog3, sdlog=ln_sdlog3, plot=F),
         sev4=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog4, sdlog=ln_sdlog4, plot=F),
         sev5=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, meanlog=ln_meanlog5, sdlog=ln_sdlog5, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadequate intakes
  mutate(ndeficient0=npeople*sev0/100,
         ndeficient1=npeople*sev1/100,
         ndeficient2=npeople*sev2/100,
         ndeficient3=npeople*sev3/100,
         ndeficient4=npeople*sev4/100,
         ndeficient5=npeople*sev5/100) %>% 
  # Calculate UL
  rowwise() %>%
  mutate(ul0=nutriR::above_ul(ul = ul, meanlog = ln_meanlog0, sdlog=ln_sdlog0),
         ul1=nutriR::above_ul(ul = ul, meanlog = ln_meanlog1, sdlog=ln_sdlog1),
         ul2=nutriR::above_ul(ul = ul, meanlog = ln_meanlog2, sdlog=ln_sdlog2),
         ul3=nutriR::above_ul(ul = ul, meanlog = ln_meanlog3, sdlog=ln_sdlog3),
         ul4=nutriR::above_ul(ul = ul, meanlog = ln_meanlog4, sdlog=ln_sdlog4),
         ul5=nutriR::above_ul(ul = ul, meanlog = ln_meanlog5, sdlog=ln_sdlog5)) %>%
  ungroup()

# Break into gamma and lognormal then remerge
data1_fort_g <- data1_fort %>% 
  # Log-normal
  filter(best_dist=="gamma") %>% 
  # Shift parameters
  rowwise() %>%
  mutate(g_shape1=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake1, plot=F)$shape,
         g_rate1=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake1, plot=F)$rate,
         # Scenario 2
         g_shape2=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake2, plot=F)$shape,
         g_rate2=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake2, plot=F)$rate,
         # Scenario 3
         g_shape3=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake3, plot=F)$shape,
         g_rate3=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake3, plot=F)$rate,
         # Scenario 4
         g_shape4=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake4, plot=F)$shape,
         g_rate4=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake4, plot=F)$rate,
         # Scenario 5
         g_shape5=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake5, plot=F)$shape,
         g_rate5=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=intake5, plot=F)$rate) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev0=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape0, rate=g_rate0, plot=F),
         sev1=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape1, rate=g_rate1, plot=F),
         sev2=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape2, rate=g_rate2, plot=F),
         sev3=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape3, rate=g_rate3, plot=F),
         sev4=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape4, rate=g_rate4, plot=F),
         sev5=nutriR::sev(ear = ar, cv = ar_cv, ear_log=T, shape=g_shape5, rate=g_rate5, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadeuate intakes
  mutate(ndeficient0=npeople*sev0/100,
         ndeficient1=npeople*sev1/100,
         ndeficient2=npeople*sev2/100,
         ndeficient3=npeople*sev3/100,
         ndeficient4=npeople*sev4/100,
         ndeficient5=npeople*sev5/100) %>% 
  # Calculate UL
  rowwise() %>%
  mutate(ul0=nutriR::above_ul(ul = ul, shape = g_shape0, rate=g_rate0),
         ul1=nutriR::above_ul(ul = ul, shape = g_shape1, rate=g_rate1),
         ul2=nutriR::above_ul(ul = ul, shape = g_shape2, rate=g_rate2),
         ul3=nutriR::above_ul(ul = ul, shape = g_shape3, rate=g_rate3),
         ul4=nutriR::above_ul(ul = ul, shape = g_shape4, rate=g_rate4),
         ul5=nutriR::above_ul(ul = ul, shape = g_shape5, rate=g_rate5)) %>%
  ungroup()


# Step 6. Merge
#############################################

# Merge
data2 <- bind_rows(data1_unfort_expanded, data1_fort_g, data1_fort_ln) %>% 
  # Arrange
  arrange(nutrient_type, nutrient, iso3, sex, age_group)
  

# Inspect
freeR::complete(data2)

# SOME DAY FIGURE OUT WHY SOME SEV CALCS DON'T WORK


# Check new SEVs (with lognormal AR) against old SEVs (with normal AR)
ggplot(data2, aes(x=sev0_norm, y=sev0, color=fortification_yn) ) +
  geom_point() +
  geom_abline(slope=1)

age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
ggplot(data2 %>% filter(sex=="Females" & age_group %in% age_groups), aes(x=sev0_norm, y=sev0, color=fortification_yn) ) +
  geom_point() +
  geom_abline(slope=1)


# Export
#############################################

# Export data
saveRDS(data2, file=file.path(outdir, "fortification_scenario_output_iron.Rds"))

