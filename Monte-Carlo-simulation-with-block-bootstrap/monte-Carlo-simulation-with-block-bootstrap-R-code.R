library(tidyverse)

# Read data directly from Datahub.io
url <- "https://datahub.io/world-development-indicators/fp.cpi.totl.zg/_r/-/data.csv"
df_raw <- read.csv(url, stringsAsFactors = FALSE)

# Filter for the United States and clean columns
df_usa <- df_raw %>%
  filter(Country.Code == "USA") %>%
  select(Year, Inflation = Value) %>%
  filter(!is.na(Inflation)) %>%
  arrange(Year)

# Extract historical parameters
average_inflation <- mean(df_usa$Inflation) / 100
inflation_sd <- sd(df_usa$Inflation) / 100

initial_savings <- 10000
num_years <- 10
num_simulations <- 10000

cat("Historical period:", min(df_usa$Year), "-", max(df_usa$Year), "\n")
cat("Number of observations:", nrow(df_usa), "\n")
cat("Historical average inflation:", round(average_inflation * 100, 2), "%\n")

set.seed(123)

# Define block length
block_length <- 3
num_blocks <- ceiling(num_years / block_length)

# Function to generate a 10-year scenario
generate_scenario <- function() {
  start_indices <- sample(1:(nrow(df_usa) - block_length + 1), 
                          num_blocks, 
                          replace = TRUE)
  
  scenario <- unlist(lapply(start_indices, function(idx) {
    df_usa$Inflation[idx:(idx + block_length - 1)] / 100
  }))
  
  return(scenario[1:num_years])
}

# Run 10,000 simulations
simulated_inflation <- t(replicate(num_simulations, generate_scenario()))

# Calculate the real value of savings after 10 years
real_value <- initial_savings / apply(1 + simulated_inflation, 1, prod)

# Expected mean value
mean_value <- mean(real_value)

# Probability of losing more than 20% (real value < 8000 $)
prob_loss_20 <- mean(real_value < 8000) * 100

# VaR (Value at Risk) 95% - financial standard
loss <- initial_savings - real_value
VaR_95_loss <- quantile(loss, 0.95)

cat("--- KEY RESULTS ---\n")
cat("Expected mean value of savings:", round(mean_value, 2), "$\n")
cat("Probability of losing more than 20% of purchasing power:", round(prob_loss_20, 2), "%\n")
cat("VaR (95%): The 95th percentile of losses is", round(VaR_95_loss, 2), "$\n")

set.seed(456)
comparison <- data.frame()

for (L in c(2, 3, 5)) {
  n_blocks <- ceiling(num_years / L)
  
  generate_scenario_L <- function() {
    idx <- sample(1:(nrow(df_usa) - L + 1), n_blocks, replace = TRUE)
    scenario <- unlist(lapply(idx, function(i) df_usa$Inflation[i:(i + L - 1)] / 100))
    return(scenario[1:num_years])
  }
  
  simulations_L <- t(replicate(num_simulations, generate_scenario_L()))
  values_L <- initial_savings / apply(1 + simulations_L, 1, prod)
  losses_L <- initial_savings - values_L
  
  comparison <- rbind(comparison, data.frame(
    Block = paste(L, "years"),
    Mean = mean(values_L),
    Prob_Loss_20 = mean(values_L < 8000) * 100,
    VaR_95 = quantile(losses_L, 0.95)
  ))
}
comparison

df_sim <- data.frame(Real_Value = real_value)

ggplot(df_sim, aes(x = Real_Value)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = 10000, color = "Initial Value"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean_value, color = "Simulated Mean"), linewidth = 1) +
  geom_vline(aes(xintercept = 8000, color = "20% Loss Threshold"), linetype = "dotdash", linewidth = 1) +
  geom_vline(aes(xintercept = 10000 - VaR_95_loss, color = "VaR 95% (Remaining Value)"), linetype = "dotted", linewidth = 1) +
  scale_color_manual(name = "Indicators", 
                     values = c("Initial Value" = "red", 
                                "Simulated Mean" = "green", 
                                "20% Loss Threshold" = "orange",
                                "VaR 95% (Remaining Value)" = "darkred")) +
  labs(
    title = "Distribution of the Real Value of Savings (3-year Block Bootstrap)",
    x = "Real value of savings ($)",
    y = "Frequency"
  ) +
  theme_minimal()