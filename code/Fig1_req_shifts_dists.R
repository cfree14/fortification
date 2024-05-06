
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "required_shifts.Rds"))



# Build data
################################################################################

# Pivot data
data <- data_orig %>% 
  # Simplify
  select(dist_id, nutrient, units, intake_shift_sev10, intake_shift_sev5, intake_shift_sev1) %>% 
  # Gather
  gather(key="target_perc", value="shift_req", 4:ncol(.)) %>% 
  mutate(target_perc=gsub("intake_shift_sev", "", target_perc) %>% as.numeric() %>% paste0(., "%"),
         target_perc=factor(target_perc, levels=c("1%", "5%", "10%"))) %>% 
  # Remove vitamin D
  filter(nutrient!="Vitamin D") %>% 
  # Format nurtient
  mutate(nutrient_label=paste0(nutrient, " (", units, ")"))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y=element_blank(),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="top",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=shift_req, fill=target_perc)) +
  facet_wrap(~nutrient_label, ncol=3, scales="free") +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Required shift in mean nutrient intake\nto meet target prevalence of indaequate intakes", y="Density") +
  scale_fill_discrete(name="Target prevalence\nof inadequate intakes") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_req_shifts_dist.png"), 
       width=5, height=6.5, units="in", dpi=600)


