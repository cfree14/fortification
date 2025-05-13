

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

# Format current data
nutrients1 <- nutrients_orig %>% 
  # Remove fluoride and vitamin D
  filter(!nutrient %in% c("Fluoride", "Vitamin D")) %>% 
  # Calculate median
  group_by(food_vehicle, nutrient) %>% 
  mutate(standard_mg_kg_med=median(standard_mg_kg, na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange and rank
  group_by(food_vehicle) %>% 
  arrange(standard_mg_kg_med) %>% 
  ungroup() %>% 
  # Add
  mutate(scenario="Current") %>% 
  # Simplify
  select(scenario, food_vehicle, nutrient, iso3, standard_mg_kg, standard_mg_kg_med)

# Build aligned / new standards
nutrients2 <- nutrients1 %>% 
  group_by(food_vehicle, nutrient, standard_mg_kg_med) %>% 
  summarize(standard50=median(standard_mg_kg, na.rm=T ),
            standard75=quantile(standard_mg_kg, probs=0.9)) %>% 
  ungroup() %>% 
  gather(key="scenario", value="standard_mg_kg", 4:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "standard50"="Aligned", 
                         "standard75"="New")) %>% 
  # Simplify
  select(scenario, food_vehicle, nutrient, standard_mg_kg, standard_mg_kg_med)

# Merge scenarios
nutrients <- bind_rows(nutrients1, nutrients2) %>% 
  # Factor scenario
  mutate(scenario=factor(scenario, levels=c("Current", "Aligned", "New"))) 

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
                   legend.position = "top",
                   legend.margin = margin(t=-2, b=-6),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot nutrient standards
g <- ggplot(nutrients, aes(y=tidytext::reorder_within(nutrient, desc(standard_mg_kg_med), food_vehicle), 
                           x=standard_mg_kg, 
                           fill=scenario,
                           alpha=scenario)) +
  facet_grid(food_vehicle~., scale="free_y", space="free_y") +
  geom_point(pch=21) +
  # Axes
  tidytext::scale_y_reordered() +
  scale_x_continuous(trans="log10",
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  # Legend
  scale_fill_ordinal(name="Scenario") +
  scale_alpha_manual(name="Scenario", values=c(0.3, 1, 1)) +
  # Labels
  labs(x="Nutrient standard (mg/kg)", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_nutrient_standards.png"), 
       width=4.5, height=5, units="in", dpi=600)

