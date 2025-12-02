

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "fortification_scenario_output_final.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Nutrients of interest
sort(unique(data_orig$nutrient))
fish_nutrients <- c("Vitamin A", "Vitamin B12", "Calcium", "Iodine", "Iron", "Zinc", "Selenium")

# Build data
data <- data_orig %>%
  # Reduce to nutrients of interest
  filter(nutrient %in% fish_nutrients) %>% 
  # Calculate stats
  group_by(nutrient, iso3, country) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient1, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup() %>% 
  # Eliminate NAs
  filter(!is.na(pdeficient)) %>% 
  # Mean inadequate intakes by country
  group_by(iso3, country) %>% 
  summarize(pdeficient=mean(pdeficient)) %>% 
  ungroup()

# Spatialize
data_sf <- world_sm %>% 
  left_join(data %>% select(-country), by="iso3")

# Spatialize small countries
data_pts <- world_centers %>%
  left_join(data %>% select(-country), by="iso3") %>% 
  # Filter to tiny places with data
    filter(area_sqkm<=25000 & !is.na(pdeficient))


# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
                axis.title=element_blank(),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                strip.text=element_text(size=6, hjust=0), # face="bold"
                # Borders/axes
                strip.background=element_blank(),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                panel.border = element_blank(),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.position = c(0.6, 0.1),
                legend.direction = "horizontal",
                legend.key.size = unit(0.4, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot data
  geom_sf(data=data_sf, mapping=aes(fill=pdeficient), lwd=0.1) +
  geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient), pch=21, size=0.9, inherit.aes = F, stroke=0.2) +
  # Legend
  scale_fill_gradientn(name="% inadequate",
                       labels=scales::percent,
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5, frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + theme1 +
  theme(panel.spacing = unit(-0.2, "lines"),
        legend.margin = margin(-8,0,-4,0))
g




