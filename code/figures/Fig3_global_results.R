

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
            ndeficient4=sum(ndeficient4, na.rm = T),
            ndeficient5=sum(ndeficient5, na.rm = T)) %>% 
  ungroup() %>% 
  # Percent deficient by country-nutrient in each scenario
  mutate(pdeficient0=ndeficient0/npeople,
         pdeficient1=ndeficient1/npeople,
         pdeficient2=ndeficient2/npeople,
         pdeficient3=ndeficient3/npeople,
         pdeficient4=ndeficient4/npeople,
         pdeficient5=ndeficient5/npeople) %>% 
  # Spread
  select(nutrient_type, nutrient, iso3, country, npeople, pdeficient0, pdeficient1,
         pdeficient2, pdeficient3, pdeficient4, pdeficient5) %>% 
  gather(6:ncol(.), key="scenario", value="pdeficient") %>% 
  mutate(scenario=recode_factor(scenario,
                                "pdeficient0"="No fortification", 
                                "pdeficient1"="Current fortification",
                                "pdeficient2"="Improved compliance",
                                "pdeficient3"="Aligned standards",
                                "pdeficient4"="Aligned and improved",
                                "pdeficient5"="Aligned, improved, expanded")) %>% 
  # Add number deficient
  mutate(ndeficient=npeople*pdeficient)

freeR::complete(stats)

# Country plot
################################################################################

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


# Global plot
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9, face="bold"),
                   plot.tag.position = c(0,0.98),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.margin = margin(t=-2, b=-2),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Inset theme
inset_theme <- theme(axis.text=element_text(size=8),
                     axis.text.x=element_text(color="white"),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=9),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=9),
                     strip.text=element_text(size=8),
                     plot.title=element_text(size=9),
                     plot.tag=element_text(size=9, face="bold"),
                     plot.tag.position = c(0,0.98),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     # Legend
                     legend.position = "none",
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
  # geom_point(pch=21, size=2.5) +
  geom_jitter(pch=21, size=2.5, height = 0.2, width=0, alpha=0.8) +
  # Labels
  labs(x="Billions of inadequate intakes", y="", tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy")) +
  guides(fill = guide_legend(nrow = 2, byrow=T)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.justification = "center",
        legend.key.size = unit(0.3, "cm"),
        legend.title=element_blank())
g1

# Totals
gstats1 <- gstats %>% 
  group_by(scenario) %>% 
  summarize(ndeficient=sum(ndeficient, na.rm=T)) %>% 
  ungroup() 
g1a <- ggplot(gstats1, aes(x=reorder(scenario, desc(ndeficient)), 
                           y=ndeficient/1e9, fill=scenario)) +
  geom_bar(stat="identity", color="black", lwd=0.2) +
  # Labels
  labs(y="Billions of\ninadequate intakes", x=" ", tag=" ", title=" \n ") +
  # Legend
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy")) +
  # Theme
  theme_bw() + inset_theme +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA))
g1a 

# Order
nutr_order2 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(prevented)

# Prevented
g2 <- ggplot(gstats %>% filter(scenario!="No fortification") , 
             aes(y=factor(nutrient, levels=nutr_order2$nutrient),
                 x=prevented/1e9,
                 fill=scenario)) +
  # geom_point(pch=21, size=2) +
  geom_jitter(pch=21, size=2.5, height = 0.2, width=0, alpha=0.8) +
  # Labels
  labs(x="Billions of prevented inadequate intakes\nrelative to no fortification", y="", tag="B") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none") 
g2

# Totals
gstats2 <- gstats %>% 
  filter(scenario!="No fortification") %>% 
  group_by(scenario) %>% 
  summarize(prevented=sum(prevented, na.rm=T)) %>% 
  ungroup() 
g2a <- ggplot(gstats2, aes(x=reorder(scenario, desc(prevented)), y=prevented/1e9, fill=scenario)) +
  geom_bar(stat="identity", color="black", lwd=0.2) +
  # Labels
  labs(y="Billions of prevented\ninadequate intakes", x=" ", tag=" ") +
  # Legend
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + inset_theme
g2a 

# Order
nutr_order3 <- gstats %>% 
  filter(scenario=="Aligned, improved, expanded") %>% 
  arrange(benefits)

# Prevented
g3 <- ggplot(gstats %>% filter(!scenario %in% c("No fortification", "Current fortification")), 
             aes(y=factor(nutrient, levels=nutr_order3$nutrient),
                 x=benefits/1e9,
                 fill=scenario)) +
  # Reference line
  # geom_vline(xintercept=0, linetype="dashed", color="grey40") +
  # annotate(geom="text", y="Vitamin A", x=0, hjust=-0.45, vjust=0.5, label="Prevented", size=2) +
  # annotate(geom="text", y="Vitamin A", x=0, hjust=1.2, vjust=0.5, label="Added", size=2) +
  # Data
  # geom_point(pch=21, size=2) +
  geom_jitter(pch=21, size=2.5, height = 0.2, width=0, alpha=0.8) +
  # Labels
  labs(x="Billions of prevented inadequate intakes\nrelative to current fortification", y="", tag="C") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g3

# Totals
gstats3 <- gstats %>% 
  filter(!scenario %in% c("No fortification", "Current fortification")) %>% 
  group_by(scenario) %>% 
  summarize(benefits=sum(benefits, na.rm=T)) %>% 
  ungroup() 
g3a <- ggplot(gstats3, aes(x=reorder(scenario, desc(benefits)), y=benefits/1e9, fill=scenario)) +
  geom_bar(stat="identity", color="black", lwd=0.2) +
  # Labels
  labs(y="Billions of prevented inadequate intakes\nrelative to current fortification", x=" \n ", tag=" ") +
  # Legend
  scale_fill_manual(name="", values=c("red", "grey40", "white", "lightblue", "deepskyblue3", "navy"), drop=F) +
  # Theme
  theme_bw() + inset_theme
g3a 

# Merge
x <- 0.36
g <- gridExtra::grid.arrange(g1, g1a,
                             g2, g2a,
                             g3, g3a,
                             ncol=2, 
                             widths=c(0.75, 0.25),
                             heights=c(x, (1-x)/2, (1-x)/2))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_global_results.png"), 
       width=6.5, height=7, units="in", dpi=600)



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
                                "Aligned and improved"="aligned_plus",
                                "Aligned, improved, expanded"="expanded")) %>% 
  # Spread
  spread(key="scenario", value="ndeficient") %>% 
  # Calculate prevented
  mutate(prevented1=none-current,
         prevented2=none-improved,
         prevented3=none-aligned,
         prevented4=none-aligned_plus,
         prevented5=none-expanded) %>% 
  # Convert to billions
  mutate_at(vars(none:prevented5), function(x){x/1e9}) %>% 
  # Arrange
  arrange(desc(prevented1))

# EXport
write.csv(gstats1, file.path(tabledir, "TableSX_global_results.csv"), row.names=F)


