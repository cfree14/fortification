

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
data <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))


# Country-level results
################################################################################

# Stats
stats <- data %>% 
  # Number deficient by country-nutrient in each scenario
  group_by(nutrient_type, nutrient, iso3, country) %>% 
  summarize(npeople=sum(npeople, na.rm = T),
            ndeficient0=sum(ndeficient0, na.rm = T), 
            ndeficient1=sum(ndeficient1, na.rm = T),
            ndeficient2=sum(ndeficient2, na.rm = T),
            ndeficient3=sum(ndeficient3, na.rm = T),
            ndeficient4=sum(ndeficient4, na.rm = T)) %>% 
  ungroup() %>% 
  # Percent deficient by country-nutrient in each scenario
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople,
         pdeficient3=ndeficient3/npeople,
         pdeficient4=ndeficient4/npeople) %>% 
  # Spread
  select(nutrient_type, nutrient, iso3, country, npeople, pdeficient0, pdeficient1, pdeficient2, pdeficient3, pdeficient4) %>% 
  gather(6:ncol(.), key="scenario", value="pdeficient") %>% 
  mutate(scenario=recode_factor(scenario,
                                "pdeficient0"="No fortification", 
                                "pdeficient1"="Current fortification",
                                "pdeficient2"="Improved compliance",
                                "pdeficient3"="Aligned standards",
                                "pdeficient4"="Aligned and improved")) %>% 
  # Add number deficient
  mutate(ndeficient=npeople*pdeficient) %>% 
  # Calculate difference from no fortificaiton
  group_by(country, nutrient) %>% 
  mutate(pdeficient_diff_no=pdeficient[scenario=="No fortification"]- pdeficient,
         pdeficient_diff_curr=pdeficient[scenario=="Current fortification"]- pdeficient) %>% 
  ungroup()

freeR::complete(stats)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=9),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=8, face="bold"),
                   # Panel
                   # panel.border = element_rect(colour = "white"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.margin = margin(t=-2, b=-2),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Prepare data
stats1 <- stats %>% 
  filter(scenario!="No fortification" & pdeficient_diff_no!=0)
order1 <- stats1 %>% 
  filter(scenario=="Current fortification") %>% 
  group_by(nutrient) %>% 
  summarize(median=median(pdeficient_diff_no)) %>% 
  arrange(desc(median))

# Plot data
g1 <- ggplot(stats1 , 
       aes(x=factor(nutrient, order1$nutrient), y=pdeficient_diff_no, fill=scenario)) +
  geom_boxplot(outlier.shape=21, size=0.2, outlier.stroke = 0.1) +
  # Markers
  geom_vline(xintercept=1:12+0.5, linetype="dashed", color="grey70", size=0.3) +
  # Labels
  labs(y="% of population",
       title="Percent of population with inadequate intakes prevented by fortification",
       x="", tag="A") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("grey40", "white", "lightblue", "deepskyblue3")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="top")
g1

# Prepare data
stats2 <- stats %>% 
  filter(!scenario %in% c("No fortification", "Current fortification")) %>% 
  filter(pdeficient_diff_curr!=0)
order2 <- stats2 %>% 
  filter(scenario=="Aligned and improved") %>% 
  group_by(nutrient) %>% 
  summarize(median=median(pdeficient_diff_curr)) %>% 
  arrange(desc(median))
labels2 <- tibble(label=c("Prevented inadequate intakes", "Added inadequate intakes"),
                y=range(stats2$pdeficient_diff_curr) %>% rev())

# Plot data
g2 <- ggplot(stats2, 
       aes(x=factor(nutrient, levels=order2$nutrient), y=pdeficient_diff_curr, fill=scenario)) +
  geom_boxplot(outlier.shape=21, size=0.2, outlier.stroke = 0.1) +
  # Markers
  geom_vline(xintercept=1:12+0.5, linetype="dashed", color="grey70", size=0.3) +
  # Reference line
  geom_hline(yintercept=0) +
  # Labels
  geom_text(data=labels2, mapping=aes(x=0.5, y=y, label=label), inherit.aes = F, hjust=0, color="grey20", size=2.2) +
  # Labels
  # labs(y="Δ in  of population\nwith inadequate intakes\nrelative to current fortification", x="", tag="B") +
  labs(y="% of population", x="", tag="B", title="Change in percent of population with inadequate intakes relative to current fortification") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.53, 0.47))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_country_results.png"), 
       width=6.5, height=5.5, units="in", dpi=600)




