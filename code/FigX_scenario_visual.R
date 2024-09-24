

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
data_orig <- readRDS(file.path(outdir, "gfdx_data_imputed_simple.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  filter(fort_status!="None") %>% 
  select(country, iso3, food_vehicle, fort_status, fortified_prop) %>% 
  # Add scenario 2 
  mutate(fortified_prop2=pmax(90, fortified_prop)) %>% 
  # Divide by 100
  mutate(fortified_prop=fortified_prop/100,
         fortified_prop2=fortified_prop2/100)



# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=6),
                     strip.text=element_text(size=6),
                     # Facets
                     panel.spacing = unit(0, "lines"),
                     # Gridlines
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))



# Plot salt
g1 <- ggplot(data %>% filter(food_vehicle=="Salt"),
             aes(x=tidytext::reorder_within(x=iso3, by=fortified_prop, within=list(food_vehicle, fort_status)))) +
  facet_grid(.~food_vehicle+fort_status, space="free_x", scale="free_x") +
  # Reference line
  geom_hline(yintercept=0.9, color="grey60", linetype="dotted", lwd=0.5) +
  # Segment
  geom_segment(mapping=aes(y=fortified_prop, yend=fortified_prop2), 
               color="grey50", lwd=0.3) +
  # Points
  geom_point(mapping=aes(y=fortified_prop)) +
  geom_point(mapping=aes(y=fortified_prop2), color="red") +
  # Labels
  labs(x="", y="% fortified") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme 
g1

# Plot other
g2 <- ggplot(data %>% filter(food_vehicle!="Salt"),
             aes(x=tidytext::reorder_within(x=iso3, by=fortified_prop, within=list(food_vehicle, fort_status)))) +
  facet_grid(.~food_vehicle+fort_status, space="free_x", scale="free_x") +
  # Reference line
  geom_hline(yintercept=0.9, color="grey60", linetype="dotted", lwd=0.5) +
  # Segment
  geom_segment(mapping=aes(y=fortified_prop, yend=fortified_prop2), 
               color="grey50", lwd=0.3) +
  # Points
  geom_point(mapping=aes(y=fortified_prop)) +
  geom_point(mapping=aes(y=fortified_prop2), color="red") +
  # Labels
  labs(x="", y="% fortified") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme
g2

# Merge 
g <- gridExtra::grid.arrange(g1, g2, nrow=2)


# Export
ggsave(g, filename=file.path(plotdir, "FigX_scenario_visual.png"), 
       width=10, height=6.5, units="in", dpi=600)
