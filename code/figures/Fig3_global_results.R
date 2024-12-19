

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
  filter(scenario=="No fortification") %>% 
  arrange(ndeficient)

# Total
g1 <- ggplot(gstats, aes(y=factor(nutrient, levels=nutr_order1$nutrient), 
                         x=ndeficient/1e9, 
                         fill=scenario)) +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Billions of people\nwith inadequate intakes", y="", tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("red", "black", "white", "lightblue", "deepskyblue3")) +
  guides(fill = guide_legend(nrow = 2, byrow=T)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.justification = "center",
        legend.key.size = unit(0.3, "cm"),
        legend.title=element_blank())
g1

# Order
nutr_order2 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(prevented)

# Prevented
g2 <- ggplot(gstats %>% filter(scenario!="No fortification") , 
             aes(y=factor(nutrient, levels=nutr_order2$nutrient),
                 x=prevented/1e9,
                 fill=scenario)) +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Billions of people with\nprevented inadequate intakes", y="", tag="B") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "black", "white", "lightblue", "deepskyblue3"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Order
nutr_order3 <- gstats %>% 
  filter(scenario=="Aligned and improved") %>% 
  arrange(benefits)

# Prevented
g3 <- ggplot(gstats %>% filter(!scenario %in% c("No fortification", "Current fortification")), 
             aes(y=factor(nutrient, levels=nutr_order3$nutrient),
                 x=benefits/1e6,
                 fill=scenario)) +
  # Reference line
  geom_vline(xintercept=0, linetype="dashed", color="grey40") +
  annotate(geom="text", y="Vitamin A", x=0, hjust=-0.45, vjust=0.5, label="Prevented", size=2) +
  annotate(geom="text", y="Vitamin A", x=0, hjust=1.2, vjust=0.5, label="Added", size=2) +
  # Data
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Millions of prevented (or added) intake inadeqeuacies\nrelative to current fortification", y="", tag="C") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "black", "white", "lightblue", "deepskyblue3"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
x <- 0.38
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(x, (1-x)/2, (1-x)/2))

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
                                "Improved compliance"="improved",
                                "Aligned standards"="aligned",
                                "Aligned and improved"="aligned_plus")) %>% 
  # Spread
  spread(key="scenario", value="ndeficient") %>% 
  # Calculate prevented
  mutate(prevented1=none-current,
         prevented2=none-improved,
         prevented3=none-aligned,
         prevented4=none-aligned_plus) %>% 
  # Convert to billions
  mutate_at(vars(none:prevented4), function(x){x/1e9}) %>% 
  # Arrange
  arrange(desc(prevented1))

# EXport
write.csv(gstats1, file.path(tabledir, "TableSX_global_results.csv"), row.names=F)
