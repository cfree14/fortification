

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"

# Build data
################################################################################

# Generate country key
key <- wbstats::wb_countries() %>%
  # Remove
  filter(region!="Aggregates") %>% 
  # Reduce
  select(iso3c, income_level_iso3c) %>% 
  # Rename
  rename(iso3=iso3c, 
         income=income_level_iso3c) %>% 
  # Add World Bank continent and region
  mutate(country=countrycode(iso3, "iso3c", "country.name"), 
         continent=countrycode(iso3, "iso3c", "continent"),
         region=countrycode(iso3, "iso3c", "region"))


# World
world <- rnaturalearth::ne_countries("large", returnclass = "sf") %>% 
  select(adm0_a3) %>% 
  left_join(key, by=c("adm0_a3"="iso3"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Region
g1 <- ggplot(world, aes(fill=region)) +
  geom_sf() +
  # Legend
  scale_fill_discrete(name="Region", na.value="grey80") +
  # Crop
  coord_sf(ylim=c(-60,90)) +
  # Theme
  theme_bw() + my_theme
#g1

# Continent
g2 <- ggplot(world, aes(fill=continent)) +
  geom_sf() +
  # Legend
  scale_fill_discrete(name="Continent              ", na.value="grey80") +
  # Crop
  coord_sf(ylim=c(-60,90)) +
  # Theme
  theme_bw() + my_theme
#g2

# Income group
g3 <- ggplot(world, aes(fill=income)) +
  geom_sf() +
  # Legend
  scale_fill_discrete(name="Income group            ", na.value="grey80") +
  # Crop
  coord_sf(ylim=c(-60,90)) +
  # Theme
  theme_bw() + my_theme
#g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3)

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_region_map.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

