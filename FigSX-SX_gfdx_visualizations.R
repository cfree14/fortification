
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
g <- ggplot(ystats, aes(x=year, fill=food_vehicle)) +
  facet_wrap(~data_type, scale="free_y") +
  geom_density(alpha=0.3, lwd=0.2) +
  # Labels
  labs(x='Year', y="Density") +
  scale_fill_discrete(name="Food vehicle") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_data_years.png"), 
       width=6.5, height=2.25, units="in", dpi=600)


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
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_ordinal(name="Fortification", na.value = "grey90") +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme
g0

# Plot data
g1 <- ggplot(data_sf, aes(fill=daily_intake_g)) +
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Daily intake (g)", na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot(data_sf, aes(fill=processed_prop)) +
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Industrially processed (%)", na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot data
g3 <- ggplot(data_sf, aes(fill=fortified_prop)) +
  facet_wrap(~food_vehicle, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Legend
  scale_fill_gradientn(name="Fortification compiance (%)", na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g0, g1, g2, g3, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_data.png"), 
       width=6.5, height=4.75, units="in", dpi=600)



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



# Plot distributions of intakes
################################################################################

# Daily intake
g0 <- ggplot(data, aes(y=food_vehicle, x=daily_intake_g, fill=food_vehicle)) +
  geom_boxplot(alpha=0.4, size=0.2) +
  # Labels
  labs(x="Daily intake (g)", y="", tag="A") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g0

# Processed
g1 <- ggplot(data, aes(y=food_vehicle, x=processed_prop, fill=food_vehicle)) +
  geom_boxplot(alpha=0.4, size=0.2) +
  # Labels
  labs(x="% industrially processed", y="", tag="B") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Fortified
g2 <- ggplot(data, aes(y=food_vehicle, x=fortified_prop, fill=food_vehicle)) +
  geom_boxplot(alpha=0.4, size=0.2) +
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
       width=6.5, height=2.25, units="in", dpi=600)



# Plot data availabiliy
################################################################################

# 
stats <- data %>% 
  # Fortification programs
  filter(fort_status!="None") %>% 
  # Simplify 
  select(iso3, food_vehicle, fort_status, processed_prop, fortified_prop) %>% 
  # Gather
  gather(key="data_type", value="value", 4:ncol(.)) %>% 
  mutate(data_type=recode_factor(data_type,
                                 "processed_prop"="% processed",
                                 "fortified_prop"="% fortified")) %>% 
  # Format
  mutate(value=!is.na(value))



g1 <- ggplot(stats %>% filter(fort_status=="Mandatory" & food_vehicle=="Salt"), aes(y=iso3, x=data_type, fill=value)) +
  facet_grid(food_vehicle~., space="free", scales="free") +
  geom_tile() +
  # Labels
  labs(x="Data type", y="Country", title="Mandatory programs") +
  # Legend
  scale_fill_manual(values=c("white", "darkred")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title=element_text(size=8))
g1


g2 <- ggplot(stats %>% filter(fort_status=="Mandatory" & food_vehicle!="Salt"), aes(y=iso3, x=data_type, fill=value)) +
  facet_grid(food_vehicle~., space="free", scales="free") +
  geom_tile() +
  # Labels
  labs(x="Data type", y="Country", title=" ") +
  # Legend
  scale_fill_manual(values=c("white", "darkred")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title=element_text(size=8))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

ggsave(g, filename=file.path(plotdir, "FigSX_gfdx_prop_availability.png"), 
       width=4.5, height=12, units="in", dpi=600)
