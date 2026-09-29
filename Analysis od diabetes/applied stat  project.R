packages <- c("tidyverse", "psych", "car", "MASS", "coin", "nortest", 
              "moments", "ggpubr", "GGally", "ggcorrplot")

installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed_packages) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
df <- read.csv("C:\\Users\\Simco Laptop\\Downloads\\Diabetes_prediction.csv")
str(df)
head(df)
summary(df)
df$Diabetic <- as.factor(df$Diabetic)
# 1. DESCRIPTIVE STATISTICS
describe(df[, sapply(df, is.numeric)])
colSums(is.na(df))
# 2. PARAMETRIC TESTS
# Shapiro-Wilk Test for Normality
cat("\nShapiro-Wilk Test (Normality):\n")
lapply(df[, sapply(df, is.numeric)], shapiro.test)

# t-test for Diabetic vs Non-Diabetic
cat("\nT-tests (Diabetic vs Non-Diabetic):\n")
ttest_results <- lapply(names(df)[sapply(df, is.numeric)], function(var) {
  t.test(df[[var]] ~ df$Diabetic)
})
names(ttest_results) <- names(df)[sapply(df, is.numeric)]
ttest_results

# ANOVA
df$AgeGroup <- cut(df$Age, breaks = 3)
anova_result <- aov(Glucose ~ AgeGroup, data = df)
summary(anova_result)

# Pearson Correlation Matrix
cat("\nPearson Correlation Matrix:\n")
cor(df[, sapply(df, is.numeric)], method = "pearson")
# 3. NON-PARAMETRIC TESTS

# Kolmogorov-Smirnov for Normality
cat("\nKolmogorov–Smirnov Test:\n")
lapply(df[, sapply(df, is.numeric)], function(x) {
  ks.test(x, "pnorm", mean(x), sd(x))
})

# Wilcoxon Rank-Sum Test (Mann-Whitney)
cat("\nWilcoxon Test (Diabetic vs Non-Diabetic):\n")
wilcox_results <- lapply(names(df)[sapply(df, is.numeric)], function(var) {
  wilcox.test(df[[var]] ~ df$Diabetic)
})
names(wilcox_results) <- names(df)[sapply(df, is.numeric)]
wilcox_results

# Kruskal-Wallis Test (Glucose ~ AgeGroup)
cat("\nKruskal-Wallis Test:\n")
kruskal.test(Glucose ~ AgeGroup, data = df)

# Spearman Correlation
cat("\nSpearman Correlation Matrix:\n")
cor(df[, sapply(df, is.numeric)], method = "spearman")

# Chi-Square Test - Pregnancies Grouped vs Diabetic
df$PregGroup <- cut(df$Pregnancies, breaks = 3)
cat("\nChi-Square Test (PregGroup vs Diabetic):\n")
chisq.test(table(df$PregGroup, df$Diabetic))
# 4. VISUALIZATIONS

# Boxplots
ggboxplot(df, x = "Diabetic", y = "Glucose", color = "Diabetic", palette = "jco") + ggtitle("Glucose by Diabetes Status")
ggboxplot(df, x = "Diabetic", y = "BMI", color = "Diabetic", palette = "lancet") + ggtitle("BMI by Diabetes Status")
ggboxplot(df, x = "Diabetic", y = "Age", color = "Diabetic", palette = "dark2") + ggtitle("Age by Diabetes Status")

# Histograms
ggplot(df, aes(BMI)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  theme_minimal() + ggtitle("Histogram of BMI")


ggplot(df, aes(x = Glucose, fill = Diabetic)) +
  geom_histogram(binwidth = 10, alpha = 0.6, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("#ff7f0e", "#1f77b4")) +
  labs(title = "Distribution of Glucose", x = "Glucose", y = "Count")

ggplot(df, aes(x = BMI, fill = Diabetic)) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("#2ca02c", "#d62728")) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count")

# Density Plot
ggplot(df, aes(x = Age, fill = Diabetic)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = c("#9467bd", "#8c564b")) +
  labs(title = "Density Plot of Age by Diabetes Status")

# Bar Plot
df$PregGroup <- cut(df$Pregnancies, breaks = c(-1, 1, 5, max(df$Pregnancies)),
                    labels = c("Low", "Medium", "High"))

ggplot(df, aes(x = PregGroup, fill = Diabetic)) +
  geom_bar(position = "dodge") +
  labs(title = "Pregnancy Count Group vs Diabetes", x = "Pregnancy Group", y = "Count") +
  scale_fill_manual(values = c("#17becf", "#bcbd22")) +
  theme_minimal()

# Violin Plot
ggviolin(df, x = "Diabetic", y = "Insulin", fill = "Diabetic", palette = "npg", trim = FALSE) +
  labs(title = "Insulin Levels by Diabetes Status")

# Pair Plot
GGally::ggpairs(df[, c("Glucose", "BMI", "Age", "Insulin", "Diabetic")])

# Correlation Matrix
corr_matrix <- round(cor(df[, sapply(df, is.numeric)], use = "complete.obs"), 2)
ggcorrplot::ggcorrplot(corr_matrix, lab = TRUE, colors = c("#6D9EC1", "white", "#E46726")) +
  ggtitle("Correlation Matrix of Numeric Variables")

# Scatter Plot - Glucose vs BMI
ggplot(df, aes(x = Glucose, y = BMI, color = Diabetic)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  scale_color_manual(values = c("#1f78b4", "#e31a1c")) +
  labs(title = "Scatter Plot: Glucose vs BMI")

# Density Plot - Insulin by Diabetes Status
ggplot(df, aes(x = Insulin, fill = Diabetic)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = c("#a6cee3", "#fb9a99")) +
  labs(title = "Density of Insulin by Diabetes Status")

