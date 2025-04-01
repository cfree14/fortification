

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
datadir <- "data/costs"

# Read data
data1_orig <- readxl::read_excel(file.path(datadir, "cost_estimates.xlsx"), sheet=1) 
data2_orig <- readxl::read_excel(file.path(datadir, "cost_estimates.xlsx"), sheet=2)

# Scenarios
scenarios <- c("Current fortification", "Improved compliance", "Aligned standards", "Aligned and improved", "Aligned, improved, and expanded")

# Format data 1
data1 <- data1_orig %>% 
  mutate(scenario=factor(scenario, levels=rev(scenarios)))

data2_order <- data2_orig %>% 
  group_by(vehicle) %>% 
  summarize(percent_avg=mean(percent)) %>% 
  arrange(desc(percent_avg))

# Format data 2
data2 <- data2_orig %>% 
  mutate(scenario=factor(scenario, levels=rev(scenarios)),
         percent=percent/100) %>% 
  # Order by most expensive
  mutate(vehicle=factor(vehicle, levels=data2_order$vehicle))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    axis.title.y=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Panel A
g1 <- ggplot(data1, aes(y=scenario, x=cost_usd2021_billions)) +
  geom_bar(stat="identity") +
  geom_text(mapping=aes(label=cost_usd2021_billions), hjust=0, nudge_x = 0.2, size=1.8) +
  # Labels
  labs(x="Fortification costs (2021USD billions)", y="", tag="A") +
  lims(x=c(0, 25)) +
  # Theme
  theme_bw() + base_theme
g1

# Panel B
g2 <- ggplot(data2, aes(y=scenario, x=percent, fill=vehicle)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE), color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of fortification costs", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Food vehicle",
                    values=c("darkorange", "snow", "gold", "tan3", "lightpink")) +
  # Theme
  theme_bw() + base_theme + 
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "Fig9_cost_estimates.png"), 
       width=6.5, height=2, units="in", dpi=600)

