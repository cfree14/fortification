
# Version 2
################################################################################

bar_width <- 0.6

# Setup theme
my_theme2 <-  theme(axis.text=element_text(size=8),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=9),
                    axis.title.x = element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=9),
                    plot.tag=element_text(size=8, face="bold"),
                    # Gridlines
                    panel.grid.major = element_blank(), 
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
  arrange(desc(ndeficient))

g1 <- ggplot(gstats, aes(x=factor(nutrient, levels=nutr_order1$nutrient), 
                         y=ndeficient/1e9, 
                         fill=scenario)) +
  geom_bar(stat="identity", position="dodge", width=bar_width) +
  # Labels
  labs(y="\nBillions of people\nwith inadequate intakes", x="", tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("red", "black", "grey", "lightblue", "deepskyblue3")) +
  guides(fill = guide_legend(nrow = 2, byrow=T)) +
  # Theme
  theme_bw() + my_theme2 +
  theme(legend.position = "none",
        legend.justification = "center",
        legend.key.size = unit(0.3, "cm"),
        legend.title=element_blank())
g1

# Order
nutr_order2 <- gstats %>% 
  filter(scenario=="Current fortification") %>% 
  arrange(desc(prevented))

# Prevented
g2 <- ggplot(gstats %>% filter(scenario!="No fortification") , 
             aes(x=factor(nutrient, levels=nutr_order2$nutrient),
                 y=prevented/1e9,
                 fill=scenario)) +
  geom_bar(stat="identity", position="dodge", width=bar_width) +
  # Labels
  labs(y=" \nBillions of people with\nprevented inadequate intakes", x="", tag="B") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "black", "grey", "lightblue", "deepskyblue3"), drop=F) +
  # Theme
  theme_bw() + my_theme2 +
  theme(legend.position = "none",
        plot.margin=margin(t=0))
g2

# Order
nutr_order3 <- gstats %>% 
  filter(scenario=="Aligned and improved") %>% 
  arrange(desc(benefits))

# Prevented
g3 <- ggplot(gstats %>% filter(!scenario %in% c("No fortification", "Current fortification")), 
             aes(x=factor(nutrient, levels=nutr_order3$nutrient),
                 y=benefits/1e6,
                 fill=scenario)) +
  # Reference line
  geom_hline(yintercept=0, linetype="solid", color="black") +
  # annotate(geom="text", y="Vitamin A", x=0, hjust=-0.45, vjust=0.5, label="Prevented", size=2) +
  # annotate(geom="text", y="Vitamin A", x=0, hjust=1.2, vjust=0.5, label="Added", size=2) +
  # Data
  geom_bar(stat="identity", position="dodge", width=bar_width) +
  # Labels
  labs(y="Millions of prevented \n(or added) inadequate intakes\nrelative to current fortification", x="", tag="C") +
  # scale_fill_manual(name="", values=c("red", "black", RColorBrewer::brewer.pal(3, "Blues")), drop=F) +
  scale_fill_manual(name="", values=c("red", "black", "grey", "lightblue", "deepskyblue3"), drop=F) +
  # Theme
  theme_bw() + my_theme2 +
  theme(legend.position = "none",
        plot.margin=margin(t=0))
g3

# Merge
x <- 0.38
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(x, (1-x)/2, (1-x)/2))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_global_results_v2.png"), 
       width=5.5, height=7.5, units="in", dpi=600)



g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
ggsave(g, filename=file.path(plotdir, "Fig3_global_results_v3.png"), 
       width=6.5, height=3.5, units="in", dpi=600)
