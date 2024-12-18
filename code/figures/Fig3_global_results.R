

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
            ndeficient2=sum(ndeficient2, na.rm = T)) %>% 
  ungroup() %>% 
  # Percent deficient by country-nutrient in each scenario
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople) %>% 
  # Spread
  select(nutrient_type, nutrient, iso3, country, npeople, pdeficient0, pdeficient1, pdeficient2) %>% 
  gather(6:ncol(.), key="scenario", value="pdeficient") %>% 
  mutate(scenario=recode_factor(scenario,
                                "pdeficient0"="No fortification", 
                                "pdeficient1"="Current fortification",
                                "pdeficient2"="Improved compliance")) %>% 
  # Add number deficient
  mutate(ndeficient=npeople*pdeficient)

freeR::complete(stats)


# Setup theme
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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot minerals
g <- ggplot(stats, 
            aes(x=pdeficient, 
                y=tidytext::reorder_within(country, pdeficient, nutrient),
                color=scenario)) +
  facet_grid(nutrient~., space="free_y", scales="free_y") +
  # geom_path(aes(x=pdeficient, y=tidytext::reorder_within(country, pdeficient, nutrient)), col="grey80", lwd=0.2) +
  geom_point() +
  # Labels
  labs(x="% deficient", y="") +
  # Scales
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="Scenario") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g


# Global results
################################################################################

# Overall stats
gstats <- stats %>% 
  # Calculate number
  group_by(nutrient, scenario) %>% 
  summarize(ncountries=n_distinct(iso3),
            npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate percent
  mutate(pdeficienct=ndeficient/npeople) %>% 
  # Calculate number prevented
  group_by(nutrient) %>% 
  mutate(prevented=ndeficient[scenario=="No fortification"]-ndeficient) %>% 
  ungroup() %>% 
  # Benefits of improved fortification
  group_by(nutrient) %>% 
  mutate(benefits=ndeficient[scenario=="Current fortification"]-ndeficient) %>% 
  ungroup() %>% 
  # Add label
  mutate(nutrient_label=paste0(nutrient, " (", ncountries, ")"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.margin = margin(t=-2, b=-2),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Order
nutr_order1 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(ndeficient)

# Total
g1 <- ggplot(gstats, aes(y=factor(nutrient, levels=nutr_order1$nutrient), 
                         x=ndeficient/1e9, 
                         color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Billions of people\nwith inadequate intakes", y="", tag="A") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

# Order
nutr_order2 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(prevented)

# Prevented
g2 <- ggplot(gstats %>% filter(scenario!="No fortification") , 
             aes(y=factor(nutrient, levels=nutr_order2$nutrient),
                 x=prevented/1e9,
                 color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Billions of people with\nprevented inadequate intakes", y="", tag="B") +
  scale_color_discrete(name="", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")

# Order
nutr_order3 <- gstats %>% 
  filter(scenario=="Improved compliance") %>% 
  arrange(benefits)

# Prevented
g3 <- ggplot(gstats %>% filter(scenario=="Improved compliance") , 
             aes(y=factor(nutrient, levels=nutr_order3$nutrient),
                 x=benefits/1e6,
                 color=scenario)) +
  geom_point() +
  # Labels
  labs(x="Millions of people with inadequate intakes\nprevented through improved compliance", y="", tag="C") +
  scale_color_discrete(name="", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(0.38, 0.31, 0.31))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_global_results.png"), 
       width=5.5, height=6.5, units="in", dpi=600)



# Prepare table
################################################################################

# Selenium and Iodine don't match Passerrelli

gstats1 <- gstats %>% 
  # Simplify
  select(nutrient, scenario, ndeficient) %>% 
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "No fortification"="none",
                                "Current fortification"="current",
                                "Improved compliance"="improved")) %>% 
  # Spread
  spread(key="scenario", value="ndeficient") %>% 
  # Calculate prevented
  mutate(prevented1=none-current,
         prevented2=none-improved) %>% 
  # Convert to billions
  mutate_at(vars(none:prevented2), function(x){x/1e9})

