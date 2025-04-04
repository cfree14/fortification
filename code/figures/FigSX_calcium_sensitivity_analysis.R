

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
tabledir <- "tables"

# Read data
data_base_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))
data_sens_orig <- readRDS(file.path(outdir, "fortification_scenario_output_calcium.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))



# Build global stats
################################################################################

gstats_sens <- data_sens_orig %>% 
  # Simplify
  select(npeople, ndeficient0, ndeficient1:ndeficient5) %>% 
  # Gather
  gather(key="scenario", value="ndeficient", 2:ncol(.)) %>% 
  # mutate(ndeficient=as.numeric(ndeficient)) %>% 
  # Summarize
  select(scenario, everything()) %>% 
  group_by(scenario) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient_sens=sum(ndeficient, na.rm=T)) %>% 
  ungroup()

gstats_base <- data_base_orig %>% 
  filter(nutrient=="Calcium") %>% 
  # Simplify
  select(npeople, ndeficient0, ndeficient1:ndeficient5) %>% 
  # Gather
  gather(key="scenario", value="ndeficient", 2:ncol(.)) %>% 
  # Summarize
  select(scenario, everything()) %>% 
  group_by(scenario) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient_base=sum(ndeficient, na.rm=T)) %>% 
  ungroup()

gstats <- gstats_base %>% 
  left_join(gstats_sens) %>% 
  mutate(ndeficient_delta=ndeficient_sens-ndeficient_base)

gstats1 <- gstats %>% 
  # Scnearios of interest
  filter(scenario %in% paste0("ndeficient", 3:5)) %>% 
  # Rename scenarios
  mutate(scenario=recode_factor(scenario, 
                                "ndeficient3"="Aligned standards",
                                "ndeficient4"="Aligned and improved",
                                "ndeficient5"="Aligned, improved, expanded")) %>% 
  mutate(label=round(ndeficient_delta/1e6, 1))


# Build country stats
################################################################################

# Country stats sensitivity
cstats_sens <- data_sens_orig %>% 
  # Calculate number deficient
  group_by(iso3, country) %>% 
  summarize(npeople=sum(npeople, na.rm = T),
            ndeficient0=sum(ndeficient0, na.rm=T),
            ndeficient1=sum(ndeficient1, na.rm=T),
            ndeficient2=sum(ndeficient2, na.rm=T),
            ndeficient3=sum(ndeficient3, na.rm=T),
            ndeficient4=sum(ndeficient4, na.rm=T),
            ndeficient5=sum(ndeficient5, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate percent deficient
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople,
         pdeficient3=ndeficient3/npeople,
         pdeficient4=ndeficient4/npeople,
         pdeficient5=ndeficient5/npeople) %>% 
  # Simplify
  select(iso3, country, pdeficient0:pdeficient5) %>% 
  # Gather
  gather(key="scenario", value="pdeficient_sens", 3:ncol(.))

# Country stats sensitivity
cstats_base <- data_base_orig %>% 
  # Calicum
  filter(nutrient=="Calcium") %>% 
  # Calculate number deficient
  group_by(iso3, country) %>% 
  summarize(npeople=sum(npeople, na.rm = T),
            ndeficient0=sum(ndeficient0, na.rm=T),
            ndeficient1=sum(ndeficient1, na.rm=T),
            ndeficient2=sum(ndeficient2, na.rm=T),
            ndeficient3=sum(ndeficient3, na.rm=T),
            ndeficient4=sum(ndeficient4, na.rm=T),
            ndeficient5=sum(ndeficient5, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate percent deficient
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople,
         pdeficient3=ndeficient3/npeople,
         pdeficient4=ndeficient4/npeople,
         pdeficient5=ndeficient5/npeople) %>% 
  # Simplify
  select(iso3, country, pdeficient0:pdeficient5) %>% 
  # Gather
  gather(key="scenario", value="pdeficient_base", 3:ncol(.))

# Build data
cstats <- cstats_base %>% 
  # Add sensitivity
  left_join(cstats_sens) %>% 
  # Calculate delta
  mutate(pdeficient_delta=pdeficient_sens-pdeficient_base) %>% 
  # Recode numeric errors as zeros (these values are all essentially same)
  mutate(pdeficient_delta=round(pdeficient_delta, 5)) %>% 
  # Not infinite
  filter(is.finite(pdeficient_delta))

# Confirm expectations
ggplot(cstats, aes(x=pdeficient_delta, fill=scenario)) +
  facet_wrap(~scenario, scales="free_y") +
  geom_histogram() +
  # Labels
  labs(y="Density", x="Change in % of people with inadequate intakes\nwhen calcium is not fortified") +
  # scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw()

# Format more
cstats1 <- cstats %>% 
  # Scnearios of interest
  filter(scenario %in% paste0("pdeficient", 3:5)) %>% 
  # Rename scenarios
  mutate(scenario=recode_factor(scenario, 
                                "pdeficient3"="Aligned standards",
                                "pdeficient4"="Aligned and improved",
                                "pdeficient5"="Aligned, improved, expanded"))

# Spatialize country stats
################################################################################


# Add to spatial data
scenarios <- sort(unique(cstats1$scenario))
x <- scenarios[1]
data_sf <- purrr::map_df(scenarios, function(x){
  out <- world_sm %>%
    # Add nutrient info
    left_join(cstats1 %>% filter(scenario==x) %>% select(-country), by="iso3") %>%
    # Fill in missing values
    mutate(scenario=x)
})


# Plot data
################################################################################

# BAse theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=6),
                    strip.text=element_text(size=7),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Global plot
g1 <- ggplot(gstats1, aes(x=ndeficient_delta/1e6, y=scenario)) +
  geom_bar(stat="identity") +
  geom_text(mapping=aes(label=label), hjust=-0.2, size=2) +
  # Labels
  labs(x="Millions of people with inadequate calcium intakes\nprevented by aligning calcium fortification standards in wheat flour",
       y="", tag="A") +
  lims(x=c(0, 650)) +
  # Theme
  theme_bw() + base_theme
g1

# Country histograms
g2 <- ggplot(cstats1, aes(x=pdeficient_delta)) +
  facet_wrap(~scenario, scales="free_y", ncol=1) +
  geom_histogram() +
  # Labels
  labs(y="Number of countries", tag="B",
       x="% of people with inadequate calcium intakes\nprevented by aligning calcium fortification standards in wheat flour") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme
g2

# Program map
g3 <- ggplot(data_sf, aes(fill=pdeficient_delta)) +
  facet_wrap(~scenario, ncol=1) +
  geom_sf(color="grey30", lwd=0.1) +
  # Labels
  labs(tag="C", x=" \n ") +
  # Legend
  scale_fill_gradientn(name="% of people", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       labels=scales::percent_format()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.11, 0.82),
        legend.key.size = unit(0.2, "cm"),
        axis.line = element_blank(),
        axis.text=element_text(color="white"),
        axis.ticks.x=element_line(color="white"),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank())
g3

# Merge
layout_matrix <- matrix(data=c(1,1,
                               2,3), byrow=1, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, heights=c(0.25, 0.75))

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_calcium_sensitivity_analysis.png"), 
       width=6.5, height=6, units="in", dpi=600)




