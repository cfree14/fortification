

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
data_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Calculate stats
  group_by(nutrient, iso3, country) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient0=sum(ndeficient0, na.rm=T),
            ndeficient1=sum(ndeficient1, na.rm=T),
            ndeficient2=sum(ndeficient2, na.rm=T),
            ndeficient3=sum(ndeficient3, na.rm=T),
            ndeficient4=sum(ndeficient4, na.rm=T),
            ndeficient5=sum(ndeficient5, na.rm=T)) %>%
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople,
         pdeficient3=ndeficient3/npeople,
         pdeficient4=ndeficient4/npeople,
         pdeficient5=ndeficient5/npeople) %>%
  ungroup() %>% 
  # Gather
  gather(key="metric_scen", value="value", 5:ncol(.)) %>% 
  # Add metric and scenario
  mutate(scenario=gsub("ndeficient|pdeficient", "", metric_scen),
         metric=gsub("0|1|2|3|4|5", "", metric_scen)) %>% 
  select(-metric_scen) %>% 
  spread(key="metric", value="value") %>% 
  # Rename scenarios
  mutate(scenario=recode_factor(scenario,
                                "0"="No fortification", 
                                "1"="Current fortification",
                                "2"="Improved compliance",
                                "3"="Aligned standards",
                                "4"="Aligned and improved",
                                "5"="Aligned, improved, expanded")) %>% 
  # Average across nutrients
  group_by(country, iso3, scenario) %>% 
  summarise(pdeficient_avg=mean(pdeficient, na.rm=T)) %>% 
  ungroup() %>% 
  # Compute difference relative to no fortification
  group_by(country, iso3) %>% 
  mutate(pdeficient_avg_diff=pdeficient_avg[scenario=="No fortification"]-pdeficient_avg) %>% 
  ungroup() %>% 
  # Filter to interest
  filter(scenario %in% c("No fortification", "Current fortification", "Improved compliance"))


# Add to spatial data
scenarios <- unique(data$scenario)
x <- scenarios[1]
data_sf <- purrr::map_df(scenarios, function(x){
  out <- world_sm %>%
    select(-country) %>%
    # Add nutrient info
    left_join(data %>% filter(scenario==x), by="iso3") %>% 
    # Fill in missing values
    mutate(scenario=na.omit(unique(scenario)))
})


# Create points for small countries
data_pts <- purrr::map_df(scenarios, function(x){
  out <- world_centers %>%
    select(-country) %>%
    left_join(data %>% filter(scenario==x), by="iso3") %>%
    # Fill in missing values
    mutate(scenario=na.omit(unique(scenario))) %>% 
  # Filter to tiny places with data
    filter(area_sqkm<=25000 & !is.na(pdeficient_avg))
})

freeR::complete(data_sf)

# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
                axis.title=element_blank(),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                strip.text=element_text(size=6, hjust=0), # face="bold"
                plot.tag = element_text(size=7, face="bold"),
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
                legend.position = "top",
                legend.direction = "horizontal",
                legend.key.size = unit(0.4, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Facet
  facet_wrap(~scenario, ncol=1) +
  # Labels
  labs(tag="A") +
  # Plot data
  geom_sf(data=data_sf, mapping=aes(fill=pdeficient_avg), lwd=0.1) +
  geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient_avg), 
             pch=21, size=0.9, inherit.aes = F, stroke=0.2) +
  # Legend
  scale_fill_gradientn(name="\nAverage % inadequate",
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
        legend.margin = margin(-4,0,-10,0))
g1

# Plot data
g2 <- ggplot() +
  # Facet
  facet_wrap(~scenario, ncol=1) +
  # Labels
  labs(tag="B") +
  # Plot data
  geom_sf(data=data_sf, mapping=aes(fill=pdeficient_avg_diff), lwd=0.1) +
  geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient_avg_diff), 
             pch=21, size=0.9, inherit.aes = F, stroke=0.2) +
  # Legend
  scale_fill_gradientn(name="Average % inadequate\nprevented through fortification",
                       labels=scales::percent,
                       colors=RColorBrewer::brewer.pal(9, "Blues"),
                       na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5, frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + theme1 +
  theme(panel.spacing = unit(-0.2, "lines"),
        legend.margin = margin(-4,0,-10,0))
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2)



# Export
ggsave(g, filename=file.path(plotdir, "Fig5_intake_inadequacy_maps_avg_pres.png"),
       width=4.5, height=3.5, units="in", dpi=600) # 6 in when double line legend



