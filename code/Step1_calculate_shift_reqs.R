
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/global_intake_inadequacies/output/"
outdir <- "output"


# Read data
data_orig <- readRDS(file.path(datadir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))

# Function
################################################################################

# Calculate required shift
target <- 10
data <- data_orig
calc_shift_req <- function(data, target){
  
  # Reduce to distributions not meeting target
  data_do <- data %>% 
    filter(sev>target)
  
  # Loop through data
  output <- purrr::map_df(1:nrow(data_do), function(x){
    
    # Dist do
    dist_do <- data_do$best_dist[x]
    dist_id_do <- data_do$dist_id[x]
    
    # Estimate new mean
    if(dist_do=="gamma"){
      mean_new <- nutriR::shift_req(ear=data_do$ar[x],
                                    cv=data_do$ar_cv[x],
                                    target=target,
                                    shape=data_do$g_shape_shift[x],
                                    rate=data_do$g_rate_shift[x], 
                                    plot=F)
    }else{
      mean_new <- nutriR::shift_req(ear=data_do$ar[x],
                                    cv=data_do$ar_cv[x],
                                    target=target,
                                    meanlog=data_do$ln_meanlog_shift[x],
                                    sdlog=data_do$ln_sdlog_shift[x], 
                                    plot=F)
      
    }
    
    # Build df
    out <- tibble(dist_id=dist_id_do,
                  mean_new=mean_new$mean)
    
  })
  
}

# Calculate new means
################################################################################

# Calculate shifts
data10 <- calc_shift_req(data=data_orig, target=10)
data5 <- calc_shift_req(data=data_orig, target=5)
data1 <- calc_shift_req(data=data_orig, target=1)


# Merge with data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  select(continent, iso3, country, 
         nutrient, units, 
         sex, age_range, dist_id,
         ar, ar_cv, 
         supply_med,
         best_dist, 
         ln_meanlog_shift, ln_sdlog_shift, 
         g_rate_shift, g_shape_shift,
         sev) %>% 
  # Rename
  rename(intake_avg_orig=supply_med,
         meanlog_orig=ln_meanlog_shift, 
         sdlog_orig=ln_sdlog_shift, 
         rate_orig=g_rate_shift, 
         shape_orig=g_shape_shift,
         sev_orig=sev) %>% 
  # Add mean required to achieve 10% inadequate
  left_join(data10, by="dist_id") %>% 
  rename(intake_avg_sev10=mean_new) %>% 
  mutate(intake_shift_sev10=intake_avg_sev10-intake_avg_orig) %>% 
  # Add mean required to achieve 10% inadequate
  left_join(data5, by="dist_id") %>% 
  rename(intake_avg_sev5=mean_new) %>% 
  mutate(intake_shift_sev5=intake_avg_sev5-intake_avg_orig) %>% 
  # Add mean required to achieve 10% inadequate
  left_join(data1, by="dist_id") %>% 
  rename(intake_avg_sev1=mean_new) %>% 
  mutate(intake_shift_sev1=intake_avg_sev1-intake_avg_orig)

# Pivot data
data_wide <- data %>% 
  # Simplify
  select(dist_id, nutrient, intake_shift_sev10, intake_shift_sev5, intake_shift_sev1) %>% 
  # Gather
  gather(key="target_perc", value="shift_req", 3:ncol(.)) %>% 
  mutate(target_perc=gsub("intake_shift_sev", "", target_perc) %>% as.numeric(),
         target_perc=factor(target_perc, levels=c(1, 5, 10))) %>% 
  # Remove vitamin D
  filter(nutrient!="Vitamin D")


# Export data
saveRDS(data, file=file.path(outdir, "required_shifts.Rds"))
  


# Plot data
################################################################################

# Plot data
g <- ggplot(data_wide, aes(x=shift_req, fill=target_perc)) +
  facet_wrap(~nutrient, ncol=3, scales="free") +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Intake shift", y="Density") +
  scale_fill_discrete(name="% inadequacy") +
  # Theme
  theme_bw()+
  theme(legend.position="top")
g



