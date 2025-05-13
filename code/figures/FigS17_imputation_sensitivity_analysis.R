

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
# Read data
data_base_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))
data_sens_orig <- readRDS(file.path(outdir, "fortification_scenario_output_imputation_sens.Rds"))


# Build global stats
################################################################################

gstats_sens <- data_sens_orig %>% 
  # Simplify
  select(nutrient, npeople, ndeficient0, ndeficient1:ndeficient5) %>% 
  # Gather
  gather(key="scenario", value="ndeficient", 3:ncol(.)) %>% 
  # mutate(ndeficient=as.numeric(ndeficient)) %>% 
  # Summarize
  select(scenario, everything()) %>% 
  group_by(nutrient, scenario) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient_sens=sum(ndeficient, na.rm=T)) %>% 
  ungroup()

gstats_base <- data_base_orig %>% 
  # Simplify
  select(nutrient, npeople, ndeficient0, ndeficient1:ndeficient5) %>% 
  # Gather
  gather(key="scenario", value="ndeficient", 3:ncol(.)) %>% 
  # Summarize
  select(scenario, everything()) %>% 
  group_by(nutrient, scenario) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient_base=sum(ndeficient, na.rm=T)) %>% 
  ungroup()

gstats <- gstats_base %>% 
  # Add sensitivity
  left_join(gstats_sens) %>% 
  # Calculate delta
  mutate(ndeficient_delta=ndeficient_sens-ndeficient_base) %>% 
  # Calclate proprtion
  mutate(pdeficient_delta=ndeficient_delta/npeople)

# Quick check to confirm scenario 0 is 0
ggplot(gstats, aes(y=nutrient, x=ndeficient_delta/1e6)) +
  facet_wrap(~scenario) +
  geom_bar(stat="identity") +
  theme_bw()



gstats1 <- gstats %>% 
  # Remove 
  filter(scenario!="ndeficient0") %>% 
  # rder nutrients
  # mutate(nutrient=factor(nutrient, levels=nutr_order$nutrient)) %>% 
  # Recode scenarios
  mutate(scenario=recode_factor(scenario,
                                "ndeficient1"="Current fortification",
                                "ndeficient2"="Improved compliance",
                                "ndeficient3"="Aligned standards",
                                "ndeficient4"="Aligned and improved",
                                "ndeficient5"="Aligned, improved, expanded"))


# Plot data
################################################################################

# BAse theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    axis.title.y=element_blank(),
                    strip.text=element_text(size=6),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

g1 <- ggplot(gstats1, aes(y=tidytext::reorder_within(nutrient, ndeficient_delta, scenario),
                    x=ndeficient_delta/1e6)) +
  facet_wrap(~scenario, ncol=1, scales="free_y") +
  geom_bar(stat="identity") +
  # Scales
  tidytext::scale_y_reordered() +
  # Labels
  labs(x="Δ number of people with indadequate intakes\nwhen ignoring proxy values for fortification compliance", y="", tag="A") +
  # Theme
  theme_bw() + base_theme

g2 <- ggplot(gstats1, aes(y=tidytext::reorder_within(nutrient, ndeficient_delta, scenario),
                    x=pdeficient_delta)) +
  facet_wrap(~scenario, ncol=1, scales="free_y") +
  geom_bar(stat="identity") +
  # Scales
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels=scales::percent_format()) +
  # Labels
  labs(x="Δ percent of people with indadequate intakes\nwhen ignoring proxy values for fortification compliance", y="", tag="B") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank())

g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "FigS17_imputation_sensitivity_analysis.png"), 
       width=6.5, height=6.5, units="in", dpi=600)








