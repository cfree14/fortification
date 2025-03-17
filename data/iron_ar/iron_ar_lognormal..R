
# Read data
df <- readxl::read_excel("data/iron_ar/iron_ar_data.xlsx")

# Convert percentile to standard normal quantiles
df$z <- qnorm(df$percentile / 100)

# Transform requirement values to log-space
df$log_req <- log(df$requirement)

# Fit a linear model: log(requirement) ~ z
fit <- lm(log_req ~ z, data = df)
summary(fit)

# Extract parameters
mu <- coef(fit)[1]  # Intercept (lognormal mean log)
sigma <- coef(fit)[2]  # Slope (lognormal standard deviation)

# Display estimated parameters
list(mu = mu, sigma = sigma)

# Plot distribution
x <- seq(0,25,0.01)
y <- dlnorm(x, meanlog=mu, sdlog=sigma)
plot(y ~ x)  

# Curve
curve <- tibble(requirement=x,
                density=y)

# Add predicted percentile
df$requirement_pred <- qlnorm(p=df$percentile/100, meanlog=mu, sdlog=sigma)
plot(requirement_pred ~ requirement, df)
abline(a=0, b=1)

# Plot
ggplot(curve, aes(x=requirement, y=density)) +
  geom_line() +
  theme_bw()

# Compute CV from estimated sigma
cv <- sqrt(exp(sigma^2) - 1)

# Compute new sigma for a lognormal distribution with meanlog = 3
sigma_new <- sqrt(log(1 + cv^2))

# Display results
list(cv = cv, sigma_new = sigma_new)

 
  
  