
# Packages
library(tidyverse)

# Lognormal iron AR quantiles from de Jong et al. (2024)
ar50 <- 7.73
ar975 <- 17.51

# Derive lognormal iron AR CV
sigma <- log(ar975/ar50)/1.96
cv <- sqrt(exp(sigma^2)-1)

# Set lognormal iron AR parameters
meanlog <- log(ar50)
sdlog <- sigma

# Simulate normal and lognormal dists
intakes <- seq(0,20, 0.1)
density_normal <- dnorm(x=intakes, mean=ar50, sd=0.1*ar50)
density_lognormal <- dlnorm(x=intakes, meanlog=meanlog, sdlog=sdlog)

# Merge
df <- tibble(intake=intakes,
             density_normal=density_normal,
             density_lognormal=density_lognormal) %>% 
  # Gather
  gather(key="dist", value="density", 2:ncol(.)) %>% 
  # Format
  mutate(dist=recode_factor(dist,
                            "density_normal"="Normal",
                            "density_lognormal"="Log-normal") )

# Plot
ggplot(df, aes(x=intake, y=density, color=dist)) +
  geom_line() + 
  # Labels
  labs(x="Iron requirement (mg/day)", y="Density") +
  # Legend
  scale_color_discrete(name="Distribution type") +
  # Theme
  theme_bw()

vals <- nutriR::dists_full



