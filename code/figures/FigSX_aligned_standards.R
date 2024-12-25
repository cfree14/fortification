

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
intake_orig <- readRDS("output/gfdx_data_imputed_simple.Rds")
nutrients_orig <- readRDS("output/gfdx_nutrient_data.Rds")

# Source function
source("code/calc_aligned_standard.R")


# Build data
################################################################################

# Format intakes
intake <- intake_orig %>% 
  select(iso3, food_vehicle, daily_intake_g)

# Build data
nutrients <- nutrients_orig %>% 
  select(iso3, food_vehicle, nutrient_type, nutrient, standard_mg_kg)

# Build data
data <- intake %>% 
  # Join
  inner_join(nutrients) %>% 
  # Calculate aligned stadard
  rowwise() %>% 
  mutate(aligned_standard_mg_kg=calc_aligned_standard(fv=food_vehicle, 
                                                      nutr=nutrient, 
                                                      intake_g_d=daily_intake_g)) %>% 
  ungroup() %>% 
  # Mark type
  mutate(type=case_when(standard_mg_kg > aligned_standard_mg_kg & !is.na(aligned_standard_mg_kg) ~ "Above aligned standard",
                        standard_mg_kg < aligned_standard_mg_kg & !is.na(aligned_standard_mg_kg) ~ "Below aligned standard",
                        T ~ "No aligned standard"))


# Build standards
################################################################################

# Build standards
programs <- data %>% 
  group_by(food_vehicle, nutrient_type, nutrient) %>% 
  summarize(daily_intake_g_max=max(daily_intake_g)) %>% 
  ungroup()

# Parameters
fvs <- freeR::uniq(programs$food_vehicle)
nutrs <- freeR::uniq(programs$nutrient)
intakes <- seq(0, 500, 1)

# Build standards (not salt)
standards1 <- expand.grid(food_vehicle=fvs, 
                    nutrient=nutrs, 
                    intake=intakes) %>% 
  # Arrange
  arrange(food_vehicle, nutrient, intake) %>% 
  # Calculate standard
  rowwise() %>% 
  mutate(standard=calc_aligned_standard(fv=food_vehicle, 
                                        nutr=nutrient, 
                                        intake_g_d=intake))

# Build standards (salt)
standards2 <- expand.grid(food_vehicle="Salt", 
                          nutrient="Iodine", 
                          intake=seq(0, 20, 0.05)) %>% 
  # Arrange
  arrange(food_vehicle, nutrient, intake) %>% 
  # Calculate standard
  rowwise() %>% 
  mutate(standard=calc_aligned_standard(fv=food_vehicle, 
                                        nutr=nutrient, 
                                        intake_g_d=intake))

# Format data
standards3 <- bind_rows(standards1, standards2) %>% 
  # Add meta-data
  left_join(programs) %>% 
  # Mutate
  mutate(intake_max=pmax(350, daily_intake_g_max)) %>% 
  # Reduce
  filter(intake <= daily_intake_g_max) %>% # intake_max
  # Arrange
  select(food_vehicle, nutrient_type, nutrient, everything()) 


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(t=-4, b=-4),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(standards3 %>% filter(nutrient_type=="Vitamin"), 
       aes(x=intake, y=standard)) +
  facet_grid(nutrient ~ food_vehicle, scale="free") +
  geom_line() +
  geom_point(data=data %>% filter(nutrient_type=="Vitamin"), 
             mapping=aes(x=daily_intake_g, y=standard_mg_kg, color=type),
             pch=21, alpha=0.5) +
  # Axis
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Per capita intake (g/day)", y="Nutrient standard (mg/kg)") +
  # Legend
  scale_color_manual(name="", values=c("blue", "red", "grey30")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

# Export plot
ggsave(g1, filename=file.path(plotdir, "FigSX_aligned_standards_vitamins.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

# Plot data
g2 <- ggplot(standards3 %>% filter(nutrient_type=="Mineral"), 
       aes(x=intake, y=standard)) +
  facet_grid(nutrient ~ food_vehicle, scale="free") +
  geom_line() +
  geom_point(data=data %>% filter(nutrient_type=="Mineral"), 
             mapping=aes(x=daily_intake_g, y=standard_mg_kg, color=type),
             pch=21, alpha=0.5) +
  # Axis
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Per capita intake (g/day)", y="Nutrient standard (mg/kg)") +
  # Legend
  scale_color_manual(name="", values=c("blue", "red", "grey30")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g2 

# Export plot
ggsave(g2, filename=file.path(plotdir, "FigSX_aligned_standards_minerals.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
