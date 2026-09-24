# p10_30 = population in 2010 within 30 km of the plant.
# power2010 = amount of power produced by a reactor in 2010
library(ggplot2)
library(patchwork)


reactors <- read.csv("data/reactors.csv")
plants <- read.csv("data/energy_data.csv")


reactors$Plant <- trimws(reactors$Plant)
plants$Plant <- trimws(plants$Plant)
reactors$Country <- trimws(reactors$Country)
plants$Country <- trimws(plants$Country)

# add up 2010 output across each plant's reactors (reactor NAs get skipped)
power <- aggregate(Ref_2010 ~ Plant + Country, data = reactors, FUN = sum)
names(power)[3] <- "power2010"

# join to plants -> this is your one dataset
data <- merge(plants, power, by = c("Plant", "Country"))

ggplot(data, aes(x = power2010)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(x = "Total plant capacity in 2010 (MW)", y = "Count")


ggplot(data, aes(x = p10_30)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(x = "Population within 30km (p10_30)", y = "Count")


ggplot(data, aes(x = log(p10_30))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(x = "Log population within 30km", y = "Count")
# ------------- FIRST TEST, WITH THE LARGE OUTLIER --------------

# Get the data where the plants generate power
a <- data[data$power2010 > 0, ]

ggplot(a, aes(x = power2010, y = p10_30)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Total plant capacity in 2010 (MW)", y = "Population within 30km")

model <- lm(p10_30 ~ power2010, data = a)
summary(model)

slopeInterval <- confint(model, level = 0.95)
slopeInterval["power2010", ] 

p_val <- summary(model)$coefficients["power2010", "Pr(>|t|)"]
p_val

z <- list(p_value = p_val, slope_interval = slopeInterval["power2010", ] ) 

z[1]

resid_df <- data.frame(fitted = fitted(model), resid = resid(model))

p1 <- ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Fitted value", y = "Residual")

p2 <- ggplot(resid_df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

p1 + p2
# ------------- SECOND TEST, WITH A LOG TRANSFORMATION --------------

b <- a
b$log_pop30 <- log(b$p10_30)

ggplot(b, aes(x = power2010, y = log_pop30)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Total plant capacity in 2010 (MW)", y = "Log population within 30km")

model <- lm(log_pop30 ~ power2010, data = b)
summary(model)

slopeInterval <- confint(model, level = 0.95)
slopeInterval["power2010", ] 

p_val <- summary(model)$coefficients["power2010", "Pr(>|t|)"]
p_val


resid_df_log <- data.frame(fitted = fitted(model), resid = resid(model))

p3 <- ggplot(resid_df_log, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Fitted value", y = "Residual")

p4 <- ggplot(resid_df_log, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

p3 + p4



plot(a$Plant, a$Country)

