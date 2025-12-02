

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
data <- readRDS(file.path(outdir, "fortification_scenario_output_final.Rds"))

# Population size
################################################################################

no_cal <- c("Afghanistan",              
            "Andorra",                   
            "Brunei",                    
            "Eritrea",                  
            "Equatorial Guinea",        
            "St. Kitts & Nevis",         
            "Marshall Islands",          
            "Nauru",                   
            "Palau",                     
            "North Korea",               
            "Palestinian Territories",   
            "Singapore",                
            "Somalia",                   
            "South Sudan",              
            "Tonga",                     
            "Tuvalu",                   
            "Vatican City")
no_cal[!no_cal %in% unique(data$country)]

pop <- data %>% 
  filter(nutrient=="Calcium") %>% 
  group_by(country, iso3) %>% 
  summarize(npeople=sum(npeople, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(calorie_yn=ifelse(country %in% no_cal, "no", "yes")) %>% 
  group_by(calorie_yn) %>% 
  summarize(npeople=sum(npeople, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(perc=npeople/sum(npeople))
pop 

sum(pop$npeople, na.rm = T)/1e9
