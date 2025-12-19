

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
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(y=food_vehicle, x=perc, fill=fortified_prop_type)) +
  facet_wrap(~fort_status) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Percent of programs", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="% compliance source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="bottom", 
        legend.direction = "vertical",
        legend.margin = margin(t=-5))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_compliance_source.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

