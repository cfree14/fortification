
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
indir <- "data/intake_proxy/raw"
outdir <- "data/intake_proxy/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "food_proxy_FBS_age_sex_040124.xlsx"))


# Build country key
################################################################################

# WHO
world <- "WLD"
wb_regions <- c("HIC", "UMC", "LMC", "LIC")
who_regions <- c("NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF")

# Country key
cntry_key_orig <- read.csv(file.path(indir, "map_iso2agg.csv")) 

# Build key %>% 
cntry_key <- cntry_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>%
  rename(iso3=country) %>% 
  # Spread
  mutate(id=rep(paste0("region", 1:3), 221)) %>% 
  spread(key="id", value="region") %>% 
  # Remove useless
  select(-region3) %>% 
  # Harmonize
  mutate(income_code=ifelse(region1 %in% wb_regions, region1, region2),
         region_code=ifelse(region1 %in% who_regions, region1, region2)) %>% 
  # Recode
  mutate(region=recode(region_code,
                       "EAS"="East Asia & Pacific",
                       "ECS"="Europe & Central Asia",
                       "LCN"="Latin America & Caribbean", 
                       "MEA"="Middle East & North Africa",
                       "NAC"="North America",
                       "SAS"="South Asia",
                       "SSF"="Sub-Saharan Africa"))

# World
world <- rnaturalearth::ne_countries("large", returnclass = "sf") %>% 
  left_join(cntry_key, by=c("adm0_a3"="iso3"))

# Plot
ggplot(world, aes(fill=region)) +
  geom_sf() +
  # Legend
  scale_fill_discrete(name="WHO region", na.value="grey80") +
  # Crop
  coord_sf(ylim=c(-60,90)) +
  # Theme
  theme_bw()


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(iso3=region) %>% 
  # Add country
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  # Add region type
  mutate(region_type=case_when(!is.na(country) ~ "country", 
                               iso3 %in% wb_regions ~ "WB income",
                               iso3 %in% who_regions ~ "WHO region",
                               iso3 == "WLD" ~ "world",
                               T ~ NA)) %>% 
  # Fill in missing countries
  mutate(country=ifelse(!is.na(country), country, iso3),
         country=recode(country,
                        "HIC"="High income country", 
                        "UMC"="Upper middle income country", 
                        "LMC"="Lower middle income country", 
                        "LIC"="Low income country",
                        # WHO regions
                        "NAC"="North America", 
                        "LCN"="Latin America and the Caribbean", 
                        "ECS"="Europe and Central Asia",
                        "MEA"="Middle East and North Africa",
                        "SAS"="South Asia",
                        "EAS"="East Asia and the Pacific", 
                        "SSF"="Sub-Saharan Africa",
                        "WLD"="World")) %>% 
  # Format food
  mutate(food_group=gsub("_", " ", food_group) %>% stringr::str_to_sentence(),
         food_group=recode(food_group,
                           "All-fg"="All food groups",
                           "Fat ani"="Animal fat",
                           "Fish demrs"='Fish, demersal',
                           "Fish freshw"="Fish, freshwater",
                           "Fish other"="Fish, other",
                           "Fish pelag"="Fish, pelagic",
                           "Fruits starch"="Fruits, starch",
                           "Fruits temp"="Fruits, temperate",
                           "Fruits trop"="Fruits, tropical",
                           "Oil palm"="Oil, palm",
                           "Oil veg"="Oil, vegetable",
                           "Othr grains"="Other grains",
                           "Othr meat"="Other meat")) %>% 
  # Format sex
  mutate(sex=recode(sex,
                    "BTH"="both",
                    "FML"="female",
                    "MLE"="male")) %>% 
  # Format age
  mutate(age=recode(age,
                    "43739"="10-19", 
                    "all-a"="all ages"),
         age=factor(age, levels=c("0-9", "10-19", "20-39", "40-64", "65+", "all ages"))) %>% 
  # Format unit
  mutate(unit=gsub("_w", "", unit)) %>% 
  # Arrange
  select(region_type, iso3, country, food_group, unit, sex, age, year, everything()) %>% 
  arrange(region_type, iso3, food_group, sex, age, year)

# Inspect 
str(data)
freeR::complete(data)

# Inspect more
table(data$food_group)
table(data$unit)
table(data$sex)
table(data$year)
table(data$age)

# Country key
cntry_key_check <- data %>% 
  count(region_type, iso3, country)

# Food key
food_key <- data %>% 
  count(food_group, unit)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "intake_data_full.Rds"))



# Summarize data
################################################################################

# Summarize data
stats <- data %>% 
  # Reduce to all food groups
  filter(food_group=="All food groups") %>% 
  # Reduce to country-sex-age-specific data
  filter(region_type=="country" & sex!="both" & age!="all ages") %>% 
  # Format sex
  mutate(sex=recode_factor(sex, 
                           "female"="Females",
                           "male"="Males")) %>% 
  # Add region info
  left_join(cntry_key %>% select(iso3, region), by="iso3")

saveRDS(stats, file=file.path(outdir, "intake_data_use.Rds"))



# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
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
g1 <- ggplot(stats, aes(x=age, y=value, group=iso3, color=region)) +
  facet_wrap(~sex) +
  geom_line(lwd=0.4) +
  # Labels
  labs(x="Age group (yr)", y="Daily calorie intake", tag="A") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Plot map
g2 <- ggplot(world, aes(fill=region)) +
  geom_sf() +
  # Labels
  labs(tag="B", y="") +
  # Legend
  scale_fill_discrete(name="WHO region", na.value="grey80") +
  # Crop
  coord_sf(ylim=c(-55,85)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text = element_text(color="white"),
        axis.title = element_text(color="white"),
        legend.key.size = unit(0.4, "cm"))
#g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.55, 0.45))


# Export plot
ggsave(g, filename=file.path(plotdir, "FigS12_calorie_intake_by_country_sex_age.png"), 
       width=6.5, height=5, units="in", dpi=600)





