

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
data_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))

freeR::complete(data_orig)

# Build data
################################################################################

# Nutrients
nutrients_do <- c("Calcium", "Iodine", "Iron", "Selenium", 
                   "Vitamin B6", "Vitamin E", "Zinc")

data_nutr <- data_orig %>% 
  filter(nutrient %in% nutrients_do)

freeR::complete(data_nutr)


# Global stats
######################################################

# Build data
gstats <- data_orig %>% 
  # Convert to proportion
  mutate_at(vars(ul0:ul5), function(x) x/100) %>% 
  # Calculate number above UL
  mutate(noverul0=ul0*npeople,
         noverul1=ul1*npeople,
         noverul2=ul2*npeople,
         noverul3=ul3*npeople,
         noverul4=ul4*npeople,
         noverul5=ul5*npeople) %>% 
  # Summarize by nutrient
  group_by(nutrient) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            noverul0=sum(noverul0, na.rm=T),
            noverul1=sum(noverul1, na.rm=T),
            noverul2=sum(noverul2, na.rm=T),
            noverul3=sum(noverul3, na.rm=T),
            noverul4=sum(noverul4, na.rm=T),
            noverul5=sum(noverul5, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate proportion
  mutate(p_over_ul0=noverul0/npeople, 
         p_over_ul1=noverul1/npeople,
         p_over_ul2=noverul2/npeople,
         p_over_ul3=noverul3/npeople,
         p_over_ul4=noverul4/npeople,
         p_over_ul5=noverul5/npeople) %>% 
  # Reduce
  select(nutrient, p_over_ul0:p_over_ul5) %>% 
  # Spread
  gather(key="scenario", value="p_over_ul", 2:ncol(.)) %>% 
  # Nutrients of interest
  filter(nutrient %in% nutrients_do) %>% 
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "p_over_ul0"="No fortification",
                                "p_over_ul1"="Current fortification",
                                "p_over_ul2"="Impoved compliance",
                                "p_over_ul3"="Aligned standards",
                                "p_over_ul4"="Aligned and improved",
                                "p_over_ul5"="Aligned, improved, expanded"))

# Order
gstats_order <- gstats %>% 
  group_by(nutrient) %>% 
  summarize(ul=mean(p_over_ul)) %>% 
  ungroup() %>% 
  arrange(desc(ul))


# Country stats
######################################################

# Build data
cstats <- data_orig %>% 
  # Convert to proportion
  mutate_at(vars(ul0:ul5), function(x) x/100) %>% 
  # Calculate number above UL
  mutate(noverul0=ul0*npeople,
         noverul1=ul1*npeople,
         noverul2=ul2*npeople,
         noverul3=ul3*npeople,
         noverul4=ul4*npeople,
         noverul5=ul5*npeople) %>% 
  # Summarize by nutrient
  group_by(nutrient, country, iso3) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            noverul0=sum(noverul0, na.rm=T),
            noverul1=sum(noverul1, na.rm=T),
            noverul2=sum(noverul2, na.rm=T),
            noverul3=sum(noverul3, na.rm=T),
            noverul4=sum(noverul4, na.rm=T),
            noverul5=sum(noverul5, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate proportion
  mutate(p_over_ul0=noverul0/npeople, 
         p_over_ul1=noverul1/npeople,
         p_over_ul2=noverul2/npeople,
         p_over_ul3=noverul3/npeople,
         p_over_ul4=noverul4/npeople,
         p_over_ul5=noverul5/npeople) %>% 
  # Reduce
  select(nutrient, country, iso3, p_over_ul0:p_over_ul5) %>% 
  # Spread
  gather(key="scenario", value="p_over_ul", 4:ncol(.)) %>% 
  # Nutrients of interest
  filter(nutrient %in% nutrients_do) %>% 
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "p_over_ul0"="No fortification",
                                "p_over_ul1"="Current fortification",
                                "p_over_ul2"="Impoved compliance",
                                "p_over_ul3"="Aligned standards",
                                "p_over_ul4"="Aligned and improved",
                                "p_over_ul5"="Aligned, improved, expanded"))

# Order
cstats_order <- cstats %>% 
  group_by(nutrient) %>% 
  summarize(ul=mean(p_over_ul, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(ul))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_blank(),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill="white"))

# Plot
g1 <- ggplot(gstats, aes(y=factor(nutrient, gstats_order$nutrient), x=p_over_ul, fill=scenario)) +
  # Data
  geom_bar(stat="identity", position="dodge") +
  # Markers
  geom_hline(yintercept=1:7+0.5, linetype="dashed", color="grey70", size=0.3) +
  # Labels
  labs(y="", x="% of global population\nwith excess intakes", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("red", "black", "grey", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.6, 0.847),
        legend.key.size = unit(0.2, "cm"))
g1

# Plot
g2 <- ggplot(cstats, aes(y=factor(nutrient, gstats_order$nutrient), x=p_over_ul, fill=scenario)) +
  # Data
  geom_boxplot(outlier.shape = 21, outlier.stroke = 0.1, size=0.2) +
  # Markers
  geom_hline(yintercept=1:7+0.5, linetype="dashed", color="grey70", size=0.3) +
  # Labels
  labs(y="", x="% of national population\nwith excess intakes", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("red", "black", "grey", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_upper_limits.png"), 
       width=6.5, height=3.5, units="in", dpi=600)
  


# Tradeoff data
################################################################################

# Change in % inadequate with % over UL
cstats <- data_orig %>%
  # Convert to proportion
  mutate_at(vars(ul0:ul5), function(x) x/100) %>% 
  # Calculate number above UL
  mutate(n_over_ul0=ul0*npeople,
         n_over_ul1=ul1*npeople,
         n_over_ul2=ul2*npeople,
         n_over_ul3=ul3*npeople,
         n_over_ul4=ul4*npeople,
         n_over_ul5=ul5*npeople) %>% 
  # Summarize
  group_by(nutrient, iso3) %>% 
  summarize(npeople=sum(npeople, na.rm=T),
            n_over_ul0=sum(n_over_ul0, na.rm=T),
            n_over_ul1=sum(n_over_ul1, na.rm=T),
            n_over_ul2=sum(n_over_ul2, na.rm=T),
            n_over_ul3=sum(n_over_ul3, na.rm=T),
            n_over_ul4=sum(n_over_ul4, na.rm=T),
            n_over_ul5=sum(n_over_ul5, na.rm=T),
            ndeficient0=sum(ndeficient0, na.rm=T),
            ndeficient1=sum(ndeficient1, na.rm=T),
            ndeficient2=sum(ndeficient2, na.rm=T),
            ndeficient3=sum(ndeficient3, na.rm=T),
            ndeficient4=sum(ndeficient4, na.rm=T),
            ndeficient5=sum(ndeficient5, na.rm=T)) %>% 
  ungroup() %>% 
  # Gather
  gather(key="metric", value="n", 4:ncol(.)) %>% 
  mutate(scenario=gsub("n_over_ul|ndeficient", "", metric),
         metric=ifelse(grepl("n_over", metric), "p_over_ul", "p_inadequate")) %>% 
  # Calculate proportion
  mutate(prop=n/npeople) %>% 
  # Change relative to no fortification
  group_by(nutrient, iso3) %>% 
  mutate(prop_delta=prop[scenario=="0"]-prop) %>% 
  ungroup() %>% 
  # Filter to fort scenarios
  filter(scenario!="0") %>% 
  # Simplify
  select(scenario, nutrient, iso3, metric, prop_delta) %>% 
  # Gather
  spread(key="metric", value="prop_delta") %>% 
  # Nutrients of interest
  filter(nutrient %in% nutrients_do) %>% 
  # Recode scenario
  mutate(scenario=recode_factor(scenario,
                         "1"="Current fortification",
                         "2"="Improved compliance",
                         "3"="Aligned standards",
                         "4"="Aligned and improved",
                         "5"="Aligned, improved, expanded"))

# Quadrant key
quad_key <- tibble(nutrient="Calcium",
                   x=c(-1, 1, 0, 1, 0),
                   y=c(0.5, -0.5, 1, 0, -1),
                   label=c(1, 2, 3, 4, 5))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   # axis.text.x = element_text(angle = 90, hjust = 1),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8, face="bold"),
                   plot.tag=element_text(size=9),
                   # Strip
                   strip.background = element_rect(fill=NA, color=NA),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.margin = margin(b=-4, t=-2),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill="white"))

g <- ggplot(cstats, aes(y=p_over_ul, x=p_inadequate, fill=scenario))  +
  facet_wrap(~nutrient, ncol=4) +
  # Reference lines
  geom_hline(yintercept=0, linetype="dotted", color="grey70") +
  geom_vline(xintercept=0, linetype="dotted", color="grey70") +
  # Reference labels
  geom_text(data=quad_key, mapping=aes(x=x, y=y, label=label), inherit.aes=F, size=2.8, color="black") +
  # Data
  geom_point(pch=21) +
  # Labels
  labs(x="Change in percent of population with inadequate intakes\nrelative to no fortifcation",
       y="Change in percent of population with excess intakes\nrelative to no fortification") +
  # Axes
  scale_x_continuous(labels=scales::percent_format()) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("grey40", "white", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme 
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "Fig6_tradeoffs_country.png"), 
       width=6.5, height=4.25, units="in", dpi=600)




