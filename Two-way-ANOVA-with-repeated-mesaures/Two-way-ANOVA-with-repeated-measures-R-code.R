# 1. Load required packages
library(tidyverse)
library(rstatix)
library(car)
library(MASS)

# 2. Read the data
url <- "https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Cigarette.csv"
df <- read.csv(url)

# 3. Create the tax grouping
state_taxes <- df %>%
  group_by(state) %>%
  summarise(avg_tax = mean(tax, na.rm = TRUE)) %>%
  mutate(Tax_Group = ifelse(avg_tax > median(avg_tax, na.rm = TRUE), "High Tax", "Low Tax"))

# 4. Clean and join the data
df_clean <- df %>%
  left_join(state_taxes, by = "state") %>%
  mutate(
    state = as.factor(state),
    year = as.factor(year),
    Tax_Group = as.factor(Tax_Group)
  ) %>%
  dplyr::select(state, year, Tax_Group, packpc) %>%
  drop_na()

# 5. Create the three transformations
df_clean$packpc_log <- log(df_clean$packpc + 1)
df_clean$packpc_sqrt <- sqrt(df_clean$packpc)

# 6. For Box-Cox, find the optimal lambda
bc <- boxcox(packpc ~ 1, data = df_clean, plotit = FALSE) 
lambda_opt <- bc$x[which.max(bc$y)] 

if(abs(lambda_opt) < 0.01) { 
  df_clean$packpc_bc <- log(df_clean$packpc) 
} else {
  df_clean$packpc_bc <- (df_clean$packpc^lambda_opt - 1) / lambda_opt
}

# 7. Preliminary tests for transformations
cat("--- TRANSFORMATION 1: LOGARITHMIC ---\n")
print(shapiro.test(df_clean$packpc_log))
print(leveneTest(packpc_log ~ Tax_Group, data = df_clean))

cat("\n--- TRANSFORMATION 2: SQUARE ROOT ---\n")
print(shapiro.test(df_clean$packpc_sqrt))
print(leveneTest(packpc_sqrt ~ Tax_Group, data = df_clean))

cat("\n--- TRANSFORMATION 3: BOX-COX ---\n")
print(shapiro.test(df_clean$packpc_bc))
print(leveneTest(packpc_bc ~ Tax_Group, data = df_clean))

# 8. Descriptive Statistics
aggregate(
  packpc ~ year + Tax_Group, 
  data = df_clean, 
  FUN = function(x) c(Mean = mean(x), SD = sd(x))
)

# 9. Two-Way Repeated Measures ANOVA
anova_model <- anova_test(
  data = df_clean, 
  dv = packpc_bc, 
  wid = state, 
  within = year, 
  between = Tax_Group
)

get_anova_table(anova_model)

# 10. Post-hoc Test
# Load the emmeans package for the post-hoc test
library(emmeans)

# Create the linear model (lm) for the post-hoc test
model_lm <- lm(packpc_bc ~ year * Tax_Group, data = df_clean)

# Post-hoc test comparing each year to the base year 1985
posthoc_interaction <- emmeans(model_lm, ~ year | Tax_Group)
contrast(posthoc_interaction, method = "trt.vs.ctrl", ref = "year1985", adjust = "bonferroni")

# 11. Data Visualization
ggplot(df_clean, aes(x = year, y = packpc, color = Tax_Group, group = Tax_Group)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Trend in Cigarette Consumption by Taxes (1985-1995)",
    x = "Year",
    y = "Packs per capita (Mean)",
    color = "Tax Group"
  ) +
  theme_minimal()