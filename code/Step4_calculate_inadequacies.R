
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


# Format data
################################################################################

# Build base data
data0 <- data_orig %>% 
  # Rename (Scenario 0 = original)
  rename(age_group=age_range,
         sev0=sev,
         ndeficient0=ndeficient,
         intake0=supply_med) %>% 
  # Format Vitamin A name
  mutate(nutrient=recode(nutrient,
                         "Vitamin A (RAE)"="Vitamin A")) %>% 
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

# Add subsidies to base data
data1 <- data0 %>% 
  # Reduce to fortification programs
  filter(iso3nutr %in% iso3nutr_subs) %>% 
  # Add subsidies
  left_join(subs, by=c("iso3nutr", "nutrient", "iso3", "sex", "age_group")) %>% 
  # Convert subsidies (mg) to target units
  mutate(subsidy1=case_when(units=="mg" ~ subsidy_mg1,
                           units=="ug" ~ subsidy_mg1*1000, 
                           T ~ NA),
         subsidy2=case_when(units=="mg" ~ subsidy_mg2,
                            units=="ug" ~ subsidy_mg2*1000, 
                            T ~ NA)) %>% 
  # Add subsidy
  mutate(intake1=intake0+subsidy1,
         intake2=intake0+subsidy2)
  
freeR::complete(data1)
  

# Shift distributions and calculate SEVs
################################################################################

# Break into gamma and lognormal then remerge
data1_ln <- data1 %>% 
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
data1_g <- data1 %>% 
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

# Merge
data2 <- bind_rows(data1_g, data1_ln) %>% 
  arrange(nutrient_type, nutrient, iso3, sex, age_group)



# Shift distributions and calculate SEVs
################################################################################

# Stats
stats <- data2 %>% 
  # Number deficient by country-nutrient in each scenario
  group_by(nutrient_type, nutrient, iso3, country) %>% 
  summarize(npeople=sum(npeople),
            ndeficient0=sum(ndeficient0), 
            ndeficient1=sum(ndeficient1),
            ndeficient2=sum(ndeficient2)) %>% 
  ungroup() %>% 
  # Percent deficient by country-nutrient in each scenario
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople) %>% 
  # Spread
  select(nutrient_type, nutrient, iso3, country, npeople, pdeficient0, pdeficient1, pdeficient2) %>% 
  gather(6:ncol(.), key="scenario", value="pdeficient") %>% 
  mutate(scenario=recode_factor(scenario,
                               "pdeficient0"="No fortification", 
                               "pdeficient1"="Current fortification",
                               "pdeficient2"="Improved fortification")) %>% 
  # Add number deficient
  mutate(ndeficient=npeople*pdeficient)

freeR::complete(stats)


# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot minerals
g <- ggplot(stats, 
       aes(x=pdeficient, 
           y=tidytext::reorder_within(country, pdeficient, nutrient),
           color=scenario)) +
  facet_grid(nutrient~., space="free_y", scales="free_y") +
  # geom_path(aes(x=pdeficient, y=tidytext::reorder_within(country, pdeficient, nutrient)), col="grey80", lwd=0.2) +
  geom_point() +
  # Labels
  labs(x="% deficient", y="") +
  # Scales
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="Scenario") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename="~/Desktop/fortification_results.png", 
       width=7.5, height=12.5, units="in", dpi=600)





# Total people
################################################################################

# Overall stats
gstats <- stats %>% 
  # Calculate number
  group_by(nutrient, scenario) %>% 
  summarize(ncountries=n_distinct(iso3),
            npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate percent
  mutate(pdeficienct=ndeficient/npeople) %>% 
  # Calculate number prevented
  group_by(nutrient) %>% 
  mutate(prevented=ndeficient[scenario=="No fortification"]-ndeficient) %>% 
  ungroup() %>% 
  # Benefits of improved fortification
  group_by(nutrient) %>% 
  mutate(benefits=ndeficient[scenario=="Current fortification"]-ndeficient) %>% 
  ungroup() %>% 
  # Add label
  mutate(nutrient_label=paste0(nutrient, " (", ncountries, ")"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.margin = margin(t=-2, b=-2),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Order
nutr_order1 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(ndeficient)

# Total
g1 <- ggplot(gstats, aes(y=factor(nutrient_label, levels=nutr_order1$nutrient_label), 
                   x=ndeficient/1e9, 
                   color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Billions of people\nwith inadequate intakes", y="", tag="A") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

# Order
nutr_order2 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(prevented)

# Prevented
g2 <- ggplot(gstats %>% filter(scenario!="No fortification") , 
       aes(y=factor(nutrient_label, levels=nutr_order2$nutrient_label),
           x=prevented/1e9,
           color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Billions of people with\nprevented inadequate intakes", y="", tag="B") +
  scale_color_discrete(name="", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")

# Order
nutr_order3 <- gstats %>% 
  filter(scenario=="Improved fortification") %>% 
  arrange(benefits)

# Prevented
g3 <- ggplot(gstats %>% filter(scenario=="Improved fortification") , 
             aes(y=factor(nutrient_label, levels=nutr_order3$nutrient_label),
                 x=benefits/1e6,
                 color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Millions of inadequate intakes\nprevented by improved fortification", y="", tag="C") +
  scale_color_discrete(name="", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(0.38, 0.31, 0.31))

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_global_results.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



