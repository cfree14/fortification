

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
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))


# Format data
################################################################################

# Format data
nutrients <- nutrients_orig %>% 
  # Remove fluoride and vitamin D
  filter(!nutrient %in% c("Fluoride", "Vitamin D")) %>% 
  # Calculate median
  group_by(food_vehicle, nutrient) %>% 
  mutate(standard_mg_kg_med=median(standard_mg_kg, na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange and rank
  group_by(food_vehicle) %>% 
  arrange(standard_mg_kg_med) %>% 
  ungroup()

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot nutrient standards
g <- ggplot(nutrients, aes(y=tidytext::reorder_within(nutrient, desc(standard_mg_kg_med), food_vehicle), 
                                x=standard_mg_kg)) +
  facet_grid(food_vehicle~., scale="free_y", space="free_y") +
  geom_point(alpha=0.2) +
  # Axes
  tidytext::scale_y_reordered() +
  scale_x_continuous(trans="log10",
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  # Labels
  labs(x="Nutrient standard (mg/kg)", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_nutrient_standards.png"), 
       width=4.5, height=4.75, units="in", dpi=600)

