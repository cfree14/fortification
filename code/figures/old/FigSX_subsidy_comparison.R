

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
data <- readRDS(file.path(outdir, "fortification_scenario_output.Rds")) %>% 
  mutate(nutrient_label=paste0(nutrient, " (", units, ")"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.title=element_text(size=7),
                   plot.tag=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(data, aes(x=subsidy1, y=subsidy4)) +
  facet_wrap(~nutrient_label, scales="free", ncol=5) +
  geom_point(pch=16) +
  geom_abline(slope=1) +
  # Labels
  labs(x="Daily per capita subsidy\nfrom current standards with current compliance", 
       y="Daily per capita subsidy\nfrom aligned standards with improved compliance") +
  # Theme
  theme_bw() + my_theme 
g

ggsave(g, filename=file.path(plotdir, "FigSX_subsidy_comparison.png"), 
       width=6.5, height=4.25, units="in", dpi=600)
