# 1. Load the dataset
df <- read.csv("adult.csv", stringsAsFactors = FALSE)
# 2. Clean the data
# Replace "?" with NA
df[df == "?"] <- NA
# Remove rows with missing values (we only care about our variables for this test)
df <- na.omit(df[, c("education", "hours.per.week")])
# 3. Group education into broader categories (to make ANOVA reliable)
# We will create a new column called "edu_level"
df$edu_level <- "Other"
df$edu_level[df$education %in% c("HS-grad", "11th", "10th", "9th", "7th-8th", "12th", "1st-4th", "5th-6th", "Preschool")] <- "High School"
df$edu_level[df$education %in% c("Some-college", "Assoc-voc", "Assoc-acdm")] <- "College"
df$edu_level[df$education %in% c("Bachelors", "Masters", "Prof-school", "Doctorate")] <- "Postgraduate"
# Convert to factor
df$edu_level <- as.factor(df$edu_level)
# 4. Descriptive Statistics
aggregate(hours.per.week ~ edu_level, data = df, FUN = mean)
aggregate(hours.per.week ~ edu_level, data = df, FUN = sd)
# 5. Visualize with a Boxplot
boxplot(hours.per.week ~ edu_level, data = df,
        main = "Comparison of Weekly Work Hours by Education Level",
        xlab = "Education Level",
        ylab = "Hours per Week",
        col = c("red", "blue", "green"))
anova_result <- aov(hours.per.week ~ edu_level, data = df)
summary(anova_result)
# 7. Post-hoc Test (Tukey HSD)
TukeyHSD(anova_result)