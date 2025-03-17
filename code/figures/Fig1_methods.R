

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
outdir <- "output"

# Read data
data_orig <- readRDS(file.path(outdir, "fortification_scenario_output.Rds"))



# Search for good example
################################################################################

data <- data_orig %>% 
  # Current SEV 30-40%
  filter(sev0 >=30 & sev0 <=40) %>% 
  # Current P(>UL) <5%
  filter(ul0 <= 5) %>% 
  # Fortified SEV <10%
  filter(sev1 <= 10) %>% 
  filter(ul1 >5)


# Build data
################################################################################

# Subset example
# Calcium-ARE-Females-25-29 = pretty but no UL
sdata <- data %>% 
  slice(1)
  # filter(nutrient=="Iodine" & country=="Colombia" & sex=="Females" & age_group=="25-29")

# NRVs
ul <- sdata$ul
ar <- sdata$ar
ar_cv <- sdata$ar_cv
ar_sd <- ar * ar_cv

# Dist
dist_type <- sdata$best_dist 

# Build distributions
if(dist_type=="gamma"){
  
  # No fort params
  rate0 <- sdata$g_rate0
  shape0 <- sdata$g_shape0
  xmax0 <- qgamma(p=0.999, shape=shape0, rate=rate0)
  
  # Current fort params
  rate1 <- sdata$g_rate1
  shape1 <- sdata$g_shape1
  xmax1 <- qgamma(p=0.999, shape=shape1, rate=rate1)
  
  # Means and subsidies
  mean0 <- nutriR::mean_dist(shape=shape0, rate=rate0)
  mean1 <- nutriR::mean_dist(shape=shape1, rate=rate1)
  subsidy <- mean1-mean0
  subsidy == sdata$subsidy1
  mean0_dens <- dgamma(x=mean0, shape=shape0, rate=rate0)
  mean1_dens <- dgamma(x=mean1, shape=shape1, rate=rate1)
  
  # Build dist
  xmax <- max(xmax0, xmax1)
  x <- seq(0, xmax, length.out=1000)
  y0 <- dgamma(x=x, shape=shape0, rate=rate0)
  y1 <- dgamma(x=x, shape=shape1, rate=rate1)
  
}else{
  
  # No fort params
  sdlog0 <- sdata$ln_sdlog0
  meanlog0 <- sdata$ln_meanlog0
  xmax0 <- qlnorm(p=0.999, meanlog=meanlog0, sdlog=sdlog0)
  
  # Current fort params
  sdlog1 <- sdata$ln_sdlog1
  meanlog1 <- sdata$ln_meanlog1
  xmax1 <- qlnorm(p=0.999, meanlog=meanlog1, sdlog=sdlog1)
  
  # Means and subsidies
  mean0 <- nutriR::mean_dist(meanlog=meanlog0, sdlog=sdlog0)
  mean1 <- nutriR::mean_dist(meanlog=meanlog1, sdlog=sdlog1)
  subsidy <- mean1-mean0
  subsidy == sdata$subsidy1
  mean0_dens <- dlnorm(x=mean0, meanlog=meanlog0, sdlog=sdlog0)
  mean1_dens <- dlnorm(x=mean1, meanlog=meanlog1, sdlog=sdlog1)
  
  # Build dist
  xmax <- max(xmax0, xmax1)
  x <- seq(0, xmax, length.out=1000)
  y0 <- dlnorm(x=x, meanlog=meanlog0, sdlog=sdlog0)
  y1 <- dlnorm(x=x, meanlog=meanlog1, sdlog=sdlog1)
  
}


# Risk curve
y_ar <- dnorm(x=x, mean=ar, sd=ar_sd)
risk <- 1 - pnorm(x, mean=ar, sd=ar_sd)

# Combine info
dists <- tibble(intake=x,
                y0=y0, 
                y1=y1,
                risk=risk) %>% 
  gather(key="scenario", value="density", 2:ncol(.)) %>% 
  mutate(scenario=recode_factor(scenario,
                                "y0"="No fortification",
                                "y1"="Current fortification", 
                                "risk"="Risk curve")) %>% 
  group_by(scenario) %>% 
  mutate(density_rel=density/max(density))

# Build average lines
dens_max <- dists %>% 
  group_by(scenario) %>% 
  summarize(dens_max=max(density))
df_avg <- tibble(scenario=c("No fortification", "Current fortification"),
                 intake=c(mean0, mean1),
                 density=c(mean0_dens, mean1_dens)) %>% 
  mutate(scenario=factor(scenario, levels=c("No fortification", "Current fortification", "Risk curve"))) %>% 
  left_join(dens_max, by="scenario") %>% 
  mutate(density_rel=density/dens_max)

# Subsidy label
subs_label <- paste(round(subsidy,1), sdata$units)
subs_x <- mean(c(mean0, mean1))

# Build title
plot_title <- paste0(sdata$nutrient, " intake by ", sdata$age_group, "-year-old ", 
                     tolower(sdata$sex), " in ", sdata$country)
xaxis_title <- paste0("Usual intake (", sdata$units, "/day)")

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.text.y.right = element_text(angle = 270, hjust = 0.5),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.margin = margin(t=-3, b=0),
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(dists, aes(x=intake, y=density_rel, color=scenario)) +
  # Distributions
  geom_ribbon(data=dists %>% filter(scenario!="Risk curve"),
              mapping=aes(x=intake, ymin=0, ymax=density_rel, fill=scenario), 
              alpha=0.5, linewidth=0.2) +
  geom_line() +
  geom_segment(data=df_avg, 
               mapping=aes(x=intake, xend=intake, y=0, yend=density_rel)) +
  # Ref lines
  geom_vline(xintercept=ul, color="grey40", size=0.5, linetype="dashed") +
  geom_vline(xintercept=ar, color="grey40", size=0.5, linetype="dashed") +
  annotate(geom="text", x=ul+20, y=1.1, label="Tolerable upper\nlevel of intake", hjust=0, color="grey20", size=2) +
  annotate(geom="text", x=ar+20, y=1.1, label="Average\nrequirement", hjust=0, color="grey20", size=2) +
  # Subsidy lines
  geom_segment(x=mean0, xend=mean1, y=0.75, yend=0.75, size=0.5, 
               arrow = arrow(length=unit(0.1, "cm"), ends="last", type = "closed"), show.legend=F) +
  annotate(geom="text", x= subs_x, y=0.75, label=subs_label, vjust=-0.4, size=2) +
  # Labels
  labs(x=xaxis_title, y="Relative density", title=plot_title) +
  scale_y_continuous(breaks=seq(0, 1, 0.25), 
                     sec.axis = sec_axis(~., name = "Risk of inadequacy", breaks=seq(0, 1, 0.25))) +
  # Scenario
  scale_color_manual(name="", values=c("red", "grey70", "black")) +
  scale_fill_manual(name="", values=c("red", "grey70", "black"), drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_methods_illustation.png"), 
       width=4.5, height=3.5, units="in", dpi=600)

