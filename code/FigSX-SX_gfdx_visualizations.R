

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
nutrients_orig <- readRDS(file.path(outdir, "gfdx_nutrient_data.Rds"))

# Format data
data <- data_orig %>% 
  # Format status
  mutate(fort_status=ifelse(fort_status=="None", NA, fort_status)) %>% 
  # Add continent
  mutate(continent=countrycode::countrycode(iso3, "iso3c", "continent"))

# Format nutrients
food_vehicles <- c("Rice", "Wheat flour", "Maize flour", "Salt", "Oil")
nutrients <- nutrients_orig %>% 
  # Order foods
  mutate(food_vehicle=factor(food_vehicle, levels=food_vehicles))



# Themes
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
################################################################################

# Plot years
ystats <- data %>% 
  # Simplify
  select(iso3, food_vehicle, year_supply, year_processed, year_pfort) %>% 
  # Gather
  gather(key="data_type", value="year", 3:ncol(.)) %>% 
  mutate(data_type=recode_factor(data_type,
                                 "year_supply"="Daily intake",
                                 "year_processed"="% processed",
                                 "year_pfort"="% fortified"))

# Plot data
g <- ggplot(ystats, aes(x=year, y=food_vehicle,  fill=food_vehicle)) +
  facet_wrap(~data_type, scale="free_y", ncol=1) +
  geom_boxplot() +
  # Labels
  labs(x='Year', y="") +
  scale_x_continuous(breaks=seq(2008, 2024, 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g

ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_data_years.png"), 
        width=4.5, height=3.5, units="in", dpi=600)

# # Plot data
# g <- ggplot(ystats, aes(x=year, fill=food_vehicle)) +
#   facet_wrap(~data_type, scale="free_y") +
#   geom_density(alpha=0.3, lwd=0.2) +
#   # Labels
#   labs(x='Year', y="Density") +
#   scale_fill_discrete(name="Food vehicle") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(legend.key.size = unit(0.5, "cm"))
# g

# # Export data
# ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_data_years.png"), 
#        width=6.5, height=2.25, units="in", dpi=600)


# Plot data
################################################################################

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))

# Add to spatial data
foods <- sort(unique(data$food_vehicle))
x <- foods[1]
data_sf <- purrr::map_df(foods, function(x){
  out <- world_sm %>%
    # Add nutrient info
    left_join(data %>% filter(food_vehicle==x) %>% select(-country), by="iso3") %>%
    # Fill in missing values
    mutate(food_vehicle=x)
})

# Theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   axis.ticks = element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.title.position = "top",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g0 <- ggplot(data_sf, aes(fill=fort_status)) +
  facet_wrap(~food_vehicle, ncol=1, strip.position="left") +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_ordinal(name="\nFortification", na.value = "white",na.translate = F) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme
g0

# Plot data
g1 <- ggplot(data_sf, aes(fill=daily_intake_g)) +
  facet_wrap(~food_vehicle, ncol=1, strip.position="left") +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Daily intake (g)", na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
g1

# Plot data
g2 <- ggplot(data_sf, aes(fill=processed_prop)) +
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Industrially processed (%)", na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
g2

# Plot data
g3 <- ggplot(data_sf, aes(fill=fortified_prop)) +
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Fortification compiance (%)", na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.background = element_blank(),
        strip.text = element_blank())
g3

# Merge
w <- 0.275
g <- gridExtra::grid.arrange(g0, g1, g2, g3, 
                             widths=c(w, rep((1-w)/3, 3)), nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_data.png"), 
       width=6.5, height=3.8, units="in", dpi=600)



# Plot nutritional standards
################################################################################

# Spatialize
nutrients_sf <- world_sm %>% 
  left_join(nutrients %>% select(-country))

# Add to spatial data
nutr_key <- nutrients %>% 
  count(food_vehicle, nutrient, nutrient_type)
x <- 1
nutrients_sf <- purrr::map_df(1:nrow(nutr_key), function(x){
  out <- world_sm %>%
    # Add nutrient info
    left_join(nutrients %>% filter(food_vehicle==nutr_key$food_vehicle[x] & nutrient==nutr_key$nutrient[x]) %>% select(-country), by="iso3") %>%
    # Fill in missing values
    mutate(food_vehicle=nutr_key$food_vehicle[x],
           nutrient=nutr_key$nutrient[x],
           nutrient_type=nutr_key$nutrient_type[x])
})

# Plot
g1 <- ggplot(nutrients_sf %>% filter(nutrient_type=="Vitamin"), aes(fill=standard_mg_kg)) +
  facet_grid(nutrient ~ food_vehicle) +
  geom_sf() +
  # Legend
  scale_fill_gradientn(name="Standard (mg/kg)", na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigSX_gfdx_nutr_stds_vitamins.png"), 
       width=6.5, height=6, units="in", dpi=600)

# Plot
g2 <- ggplot(nutrients_sf %>% filter(nutrient_type=="Mineral"), aes(fill=standard_mg_kg)) +
  facet_grid(nutrient ~ food_vehicle) +
  geom_sf() +
  # Legend
  scale_fill_gradientn(name="Standard (mg/kg)", na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigSX_gfdx_nutr_stds_minerals.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



# Plot distributions of values
################################################################################

# Daily intake
g0 <- ggplot(data, aes(y=food_vehicle, x=daily_intake_g, fill=food_vehicle)) +
  geom_boxplot(size=0.2) +
  # Labels
  labs(x="Daily intake (g)", y="", tag="A") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g0

# Processed
g1 <- ggplot(data, aes(y=food_vehicle, x=processed_prop, fill=food_vehicle)) +
  geom_boxplot(size=0.2) +
  # Labels
  labs(x="% industrially processed", y="", tag="B") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Fortified
g2 <- ggplot(data, aes(y=food_vehicle, x=fortified_prop, fill=food_vehicle)) +
  geom_boxplot(size=0.2) +
  # Reference line
  geom_vline(xintercept = 90, linetype="dashed") +
  # Labels
  labs(x="% fortification compliance", y="", tag="C") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge 
g <- gridExtra::grid.arrange(g0, g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_intake_prop_dists.png"), 
       width=6.5, height=2, units="in", dpi=600)


# Standards distribution
################################################################################

# Plot data
g <- ggplot(nutrients, aes(y=food_vehicle, x=standard_mg_kg, fill=food_vehicle)) +
  facet_wrap(~nutrient, scales="free_x", ncol=5) +
  geom_boxplot(lwd=0.2) +
  # Labels
  labs(x="Fortification standard (mg/kg)", y="") +
  lims(x=c(0,NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_fort_standards_boxplots.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



# Plot data availability
################################################################################

# Prepare data
data1 <- data %>% 
  # Fortification programs
  filter(fort_status!="None") %>% 
  # Simplify 
  select(continent, iso3, food_vehicle, fort_status, processed_prop, fortified_prop) %>% 
  # Recode
  mutate(food_vehicle=recode(food_vehicle,
                             "Wheat flour"="Wheat\nflour",
                             "Maize flour"="Maize\nflour"),
         fort_status=recode(fort_status,
                            "Mandatory"="Mand.",
                            "Voluntary"="Vol."))

# Split
continents1 <- c("Africa", "Asia")

# Plot one
g1 <- ggplot(data1 %>% filter(continent %in% continents1), aes(x=food_vehicle, y=iso3, fill=processed_prop)) +
  facet_grid(continent+fort_status~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="Food vehicle", y="Country") +
  # Legend
  scale_fill_gradientn(name="% processed", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral"), 
                       na.value="grey70",
                       lim=c(0,100)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        panel.spacing = unit(0, "cm"))
g1

# Plot one
g2 <- ggplot(data1 %>% filter(!continent %in% continents1), aes(x=food_vehicle, y=iso3, fill=processed_prop)) +
  facet_grid(continent+fort_status~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="Food vehicle", y="Country") +
  # Legend
  scale_fill_gradientn(name="% processed", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral"), 
                       na.value="grey70",
                       lim=c(0,100)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        panel.spacing = unit(0, "cm"))
g2 

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.45, 0.55), nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_availability_processed_prop.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



# Plot data availability
################################################################################

# Plot one
g1 <- ggplot(data1 %>% filter(continent %in% continents1), aes(x=food_vehicle, y=iso3, fill=fortified_prop)) +
  facet_grid(continent+fort_status~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="Food vehicle", y="Country") +
  # Legend
  scale_fill_gradientn(name="% fortified", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral"), 
                       na.value="grey70",
                       lim=c(0,100)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        panel.spacing = unit(0, "cm"))
g1

# Plot one
g2 <- ggplot(data1 %>% filter(!continent %in% continents1), aes(x=food_vehicle, y=iso3, fill=fortified_prop)) +
  facet_grid(continent+fort_status~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="Food vehicle", y="Country") +
  # Legend
  scale_fill_gradientn(name="% fortified", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral"), 
                       na.value="grey70",
                       lim=c(0,100)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        panel.spacing = unit(0, "cm"))
g2 

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.45, 0.55), nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_availability_fortified_prop.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



# Nutrient standard data availability
################################################################################










