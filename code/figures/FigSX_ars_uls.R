

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
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Build data
  count(nutrient, sex, age_group, ar_source, ar_units, ar, ul_source, ul_units, ul) %>% 
  # Check units
  mutate(check=ar_units==ul_units) %>% 
  # Remove pesky nutrients (TEMPORARY)
  filter(!nutrient %in% c("Zinc", "Iron"))

# Data
data_long <- data %>% 
  # Simplify
  rename(units=ar_units) %>% 
  select(-c(ar_source, ul_source, ul_units, check, n)) %>% 
  # Gather
  gather(key="nrv", value="value", 5:ncol(.)) %>% 
  # Add group
  mutate(group=paste(sex, nrv, sep="-")) %>% 
  # Add nutrient label
  mutate(nutrient_label=paste0(nutrient, " (", units, ")")) %>% 
  # Format NRV
  mutate(nrv=recode(nrv,
                    "ar"="Average requirement",
                    "ul"="Upper limit"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(t=-2, b=-4),
                   legend.position="top",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_long, aes(x=age_group, y=value, color=sex, linetype=nrv, group=group)) +
  facet_wrap(~nutrient_label, scales="free_y", ncol=3) +
  geom_line() +
  # Limits
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Age group (yrs)", y="Nutrient reference value (NRV)") +
  # Legend
  scale_color_discrete(name="Sex") +
  scale_linetype_discrete(name="NRV") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_ars_uls_most.png"), 
       width=6.5, height=5, units="in", dpi=600)



# Build iron/zinc data
################################################################################

# Build data
data <- data_orig %>% 
  # Filter
  filter(nutrient %in% c("Zinc", "Iron")) %>% 
  # Build data
  count(nutrient, iso3, sex, age_group, ar_source, ar_units, ar, ul_source, ul_units, ul) %>% 
  # Check units
  mutate(check=ar_units==ul_units)

# Data
data_long <- data %>% 
  # Simplify
  rename(units=ar_units) %>% 
  select(-c(ar_source, ul_source, ul_units, check, n)) %>% 
  # Gather
  gather(key="nrv", value="value", 6:ncol(.)) %>% 
  # Add group
  mutate(group=paste(iso3, sex, nrv, sep="-")) %>% 
  # Add nutrient label
  mutate(nutrient_label=paste0(nutrient, " (", units, ")")) %>% 
  # Format NRV
  mutate(nrv=recode(nrv,
                    "ar"="Average requirement",
                    "ul"="Upper limit"))

g <- ggplot(data_long, aes(x=age_group, y=value, color=sex, linetype=nrv, group=group)) +
  facet_wrap(~nutrient_label, scales="free_y", ncol=3) +
  geom_line(lwd=0.2) +
  # Limits
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Age group (yrs)", y="Nutrient reference value (NRV)") +
  # Legend
  scale_color_discrete(name="Sex") +
  scale_linetype_discrete(name="NRV") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_ars_uls_iron_zinc.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


