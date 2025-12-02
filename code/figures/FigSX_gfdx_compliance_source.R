

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
gisdir <- "data/world/processed"
plotdir <- "figures"
outdir <- "output"
datadir <- "data/gfdx/processed"

# Read data
data_orig <- readRDS(file.path(outdir, "gfdx_data.Rds"))


# Build data
################################################################################

# Build data 
type_levels <- c("Industry compliance by production volumes",
                 "Industry compliance by facilities/samples monitored",
                 "Proxy of industry compliance by market share",
                 "Proxy of fortification quality by market/household samples",
                 "Expert opinion",
                 "Proxy of estimated fortification quality")
data <- data_orig %>% 
  # Simpliy
  filter(fort_status!="None" & !is.na(fortified_prop)) %>% 
  # Count
  count(food_vehicle, fort_status, fortified_prop_type) %>% 
  # Add percent
  group_by(fort_status, food_vehicle) %>% 
  mutate(perc=n/sum(n)) %>% 
  # Apply levels
  mutate(fortified_prop_type=factor(fortified_prop_type,
                               levels=type_levels))

# Plot data
ggplot(data, aes(y=food_vehicle, x=perc, fill=fortified_prop_type)) +
  facet_wrap(~fort_status) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of programs", y="") +
  # Legend
  scale_fill_ordinal(name="% compliance source") +
  sc
  # Theme
  theme_bw()

