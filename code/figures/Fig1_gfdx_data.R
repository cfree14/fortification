

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

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Format status
  mutate(fort_status=factor(fort_status, levels=c("Mandatory", "Voluntary", "None"))) %>% 
  # Add continent
  mutate(continent=countrycode::countrycode(iso3, "iso3c", "continent"))  %>% 
  # Arrange (this is for plotting - so mando on top and none on bottom)
  arrange(desc(fort_status))

# Stats
data %>% 
  filter(fort_status!="None") %>% 
  summarize(n=n(),
            n_low_compliance=sum(fortified_prop<0.9),
            p_low_compliance=n_low_compliance/n)

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

# Determine order
key <- data %>% 
  # Number of programs
  group_by(food_vehicle, fort_status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Spread
  mutate(fort_status=tolower(fort_status)) %>% 
  spread(key="fort_status", value="n") %>% 
  # Total
  mutate(total=mandatory+voluntary) %>% 
  arrange(desc(total)) %>% 
  # Order
  mutate(food_vehicle=factor(food_vehicle, levels=food_vehicle)) %>% 
  # Label
  mutate(label=paste0(total, " (", mandatory, "/", voluntary, ")"))

# Order data
data_sf_ordered <- data_sf %>% 
  mutate(food_vehicle=factor(food_vehicle, levels=key$food_vehicle)) 

# Themes
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=9),
                     strip.text=element_text(size=9),
                     plot.tag=element_text(size=9),
                     # Strip
                     strip.background = element_rect(colour="white", fill="white"),
                     panel.border = element_rect(colour = "white"),
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

# Program map
g1 <- ggplot(data_sf_ordered, aes(fill=fort_status)) +
  facet_wrap(~food_vehicle, ncol=1, strip.position="left") +
  geom_sf(color="grey30", lwd=0.1) +
  # Sample size
  geom_text(data=key, mapping=aes(x=-160, y=-30, label=label), hjust=0, inherit.aes=F, size=2.2) +
  # Labels
  labs(tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("purple", "gold", "white"), na.value = "white", na.translate = F) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top", 
        # legend.position = c(0.13, 0.87),
        legend.key.size = unit(0.3, "cm"),
        axis.line = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
g1


# % processed
key2 <- data %>% 
  group_by(food_vehicle) %>% 
  summarize(val=mean(daily_intake_g, na.rm=T)) %>% 
  arrange(val)
g2 <- ggplot(data, aes(y=factor(food_vehicle, key2$food_vehicle), x=daily_intake_g)) +
  geom_jitter(aes(fill=fort_status), pch=21, height = 0.2) +
  # geom_boxplot(size=0.2, fill=NA, outliers = F) +
  # Labels
  labs(x="Per capita intake (g/day)", y="", tag="B") +
  # Legend
  scale_fill_manual(name="", values=c("purple", "gold", "white"), na.value = "white", na.translate = F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# % processed
key3 <- data %>% 
  group_by(food_vehicle) %>% 
  summarize(val=mean(processed_prop, na.rm=T)) %>% 
  arrange(val)
g3 <- ggplot(data, aes(y=factor(food_vehicle, key3$food_vehicle), x=processed_prop)) +
  geom_jitter(aes(fill=fort_status), pch=21, height = 0.2) +
  # geom_boxplot(size=0.2, fill=NA, outliers = F) +
  # Labels
  labs(x="% industrially processed", y="", tag="C") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  # scale_fill_ordinal(name="\nFortification", na.value = "white", na.translate = F) +
  scale_fill_manual(name="", values=c("purple", "gold", "white"), na.value = "white", na.translate = F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g3

# % fortified
key4 <- data %>% 
  group_by(food_vehicle) %>% 
  summarize(val=mean(fortified_prop, na.rm=T)) %>% 
  arrange(val)
g4 <- ggplot(data, aes(y=factor(food_vehicle, key4$food_vehicle), x=fortified_prop)) +
  geom_jitter(aes(fill=fort_status), pch=21, height = 0.2) +
  # geom_boxplot(size=0.2, fill=NA, outliers = F) +
  # Reference line
  geom_vline(xintercept = 0.9, linetype="dashed", color="red") +
  # Labels
  labs(x="% fortification compliance", y="", tag="D") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  # scale_fill_ordinal(name="\nFortification", na.value = "white",na.translate = F) +
  scale_fill_manual(name="", values=c("purple", "gold", "white"), na.value = "white", na.translate = F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g4

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               1,3,
                               1,4), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_gfdx_data.png"), 
       width=6.5, height=6, units="in", dpi=600)
  
  
  

