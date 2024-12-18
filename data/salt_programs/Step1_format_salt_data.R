
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/salt_programs"
outdir <- "data/gfdx/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20240930 - GFDx Salt Data - Industrially processed.xlsx"),
                                 skip=2)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(country=country_name,
         source=ip_source,
         source_eng=ip_source_english,
         year=ip_year,
         processed_mt=industrially_processed_mt,
         processed_prop=industrially_processed_pc,
         comments=ip_comment) %>% 
  # Filter
  filter(!is.na(processed_prop)) %>% 
  # Format proportion
  mutate(processed_prop=processed_prop/100) %>% 
  # Format country
  mutate(country=countrycode(country, "country.name", "country.name")) %>% 
  # Simplify
  select(country, processed_prop, year, source) %>% 
  # Get data for most recent year
  arrange(country, desc(year)) %>% 
  group_by(country) %>% 
  slice(1) %>% 
  ungroup()

freeR::which_duplicated(data$country)

# Inspect
colnames(data)

# Plot data
g <- ggplot(data, aes(x=processed_prop, y=reorder(country, processed_prop))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="% of salt\nthat is industrially processed", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=8),
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
g

# Export
ggsave(g, filename=file.path(indir, "FigSX_salt_processed_prop.png"), 
       width=5.5, height=4, units="in", dpi=600)

