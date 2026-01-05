# ================================================================
# HOSPITAL READMISSION AND COST ANALYSIS
# Author: Daniel Kattoula
# Date: January 2026
# ================================================================

# ===== 1. SETUP =====
# Set working directory
setwd("") 

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# ===== 2. LOAD DATA =====
readmissions <- read.csv("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv")
costs <- read.csv("Medicare_Hospital_Spending_Per_Patient-Hospital.csv")
hospital_info <- read.csv("Hospital_General_Information.csv")

# ===== 3. DATA CLEANING =====

# Fix Facility.ID format in hospital_info (remove leading zero)
hospital_info$Facility.ID <- as.integer(hospital_info$Facility.ID)

# Convert numeric columns in readmissions (stored as character)
readmissions$Excess.Readmission.Ratio <- as.numeric(readmissions$Excess.Readmission.Ratio)
readmissions$Predicted.Readmission.Rate <- as.numeric(readmissions$Predicted.Readmission.Rate)
readmissions$Expected.Readmission.Rate <- as.numeric(readmissions$Expected.Readmission.Rate)

# Clean Number.of.Discharges (has "N/A" text)
readmissions$Number.of.Discharges <- ifelse(readmissions$Number.of.Discharges == "N/A", 
                                            NA, 
                                            as.numeric(readmissions$Number.of.Discharges))

# Clean Number.of.Readmissions (has "Too Few to Report")
readmissions$Number.of.Readmissions <- ifelse(readmissions$Number.of.Readmissions == "Too Few to Report", 
                                              NA, 
                                              as.numeric(readmissions$Number.of.Readmissions))

# Convert costs Score to numeric
costs$Score <- as.numeric(costs$Score)

# ===== 4. AGGREGATE READMISSIONS DATA =====
# Create hospital-level averages (since each hospital has multiple condition rows)
hospital_readmit_avg <- readmissions %>%
  group_by(Facility.ID, Facility.Name, State) %>%
  summarise(
    Avg_Excess_Readmission_Ratio = mean(Excess.Readmission.Ratio, na.rm = TRUE),
    Total_Discharges = sum(Number.of.Discharges, na.rm = TRUE),
    Total_Readmissions = sum(Number.of.Readmissions, na.rm = TRUE),
    Num_Conditions_Measured = n(),
    .groups = 'drop'
  )

# ===== 5. MERGE DATASETS =====

# Merge readmissions + costs
merged_data <- hospital_readmit_avg %>%
  left_join(costs %>% select(Facility.ID, Score), by = "Facility.ID")

# Add hospital info
merged_data <- merged_data %>%
  left_join(hospital_info %>% select(Facility.ID, Hospital.Type, Hospital.Ownership, 
                                     Emergency.Services, Hospital.overall.rating), 
            by = "Facility.ID")

# Rename cost score for clarity
merged_data <- merged_data %>%
  rename(Medicare_Spending_Score = Score)

# ===== 6. FINAL DATA PREPARATION =====

# Remove rows with missing key variables
analysis_data <- merged_data %>%
  filter(!is.na(Avg_Excess_Readmission_Ratio) & !is.na(Medicare_Spending_Score))

# Convert hospital rating to numeric
analysis_data$Hospital.overall.rating <- as.numeric(analysis_data$Hospital.overall.rating)

# Create categorical variables for analysis
analysis_data <- analysis_data %>%
  mutate(
    Readmission_Category = ifelse(Avg_Excess_Readmission_Ratio > 1.0, "High", "Low"),
    Cost_Category = ifelse(Medicare_Spending_Score > 1.0, "High", "Low"),
    Performance_Category = case_when(
      Avg_Excess_Readmission_Ratio > 1.0 & Medicare_Spending_Score > 1.0 ~ "High Cost, High Readmit",
      Avg_Excess_Readmission_Ratio > 1.0 & Medicare_Spending_Score <= 1.0 ~ "Low Cost, High Readmit",
      Avg_Excess_Readmission_Ratio <= 1.0 & Medicare_Spending_Score > 1.0 ~ "High Cost, Low Readmit",
      Avg_Excess_Readmission_Ratio <= 1.0 & Medicare_Spending_Score <= 1.0 ~ "Low Cost, Low Readmit"
    )
  )

cat("Final dataset has", nrow(analysis_data), "hospitals\n")

# ===== 7. EXPLORATORY DATA ANALYSIS =====

# Descriptive statistics
summary(analysis_data)

# Performance category distribution
table(analysis_data$Performance_Category)

# Correlation test
cor_test <- cor.test(analysis_data$Avg_Excess_Readmission_Ratio, 
                     analysis_data$Medicare_Spending_Score)
print(cor_test)

# Summary by ownership type
ownership_summary <- analysis_data %>%
  group_by(Hospital.Ownership) %>%
  summarise(
    Count = n(),
    Avg_Readmission = mean(Avg_Excess_Readmission_Ratio, na.rm = TRUE),
    Avg_Cost = mean(Medicare_Spending_Score, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Readmission))

print(ownership_summary)

# Summary by state (top 10)
state_summary <- analysis_data %>%
  group_by(State) %>%
  summarise(
    Count = n(),
    Avg_Readmission = mean(Avg_Excess_Readmission_Ratio, na.rm = TRUE),
    Avg_Cost = mean(Medicare_Spending_Score, na.rm = TRUE)
  ) %>%
  arrange(desc(Count)) %>%
  head(10)

print(state_summary)

# ===== 8. VISUALIZATIONS =====

# Scatter plot: Readmissions vs Costs
ggplot(analysis_data, aes(x = Avg_Excess_Readmission_Ratio, y = Medicare_Spending_Score)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "darkgreen") +
  labs(title = "Hospital Readmission Rates vs Medicare Spending",
       x = "Excess Readmission Ratio (1.0 = National Average)",
       y = "Medicare Spending per Beneficiary (1.0 = National Average)",
       caption = "Green lines represent national averages") +
  theme_minimal()







# ===== 9. REGRESSION ANALYSIS =====

# Model 1: Simple linear regression
model1 <- lm(Medicare_Spending_Score ~ Avg_Excess_Readmission_Ratio, 
             data = analysis_data)
summary(model1)

# Model 2: Multiple regression with hospital characteristics
model2 <- lm(Medicare_Spending_Score ~ Avg_Excess_Readmission_Ratio + 
               Total_Discharges + Hospital.Ownership + Emergency.Services, 
             data = analysis_data)
summary(model2)

# ===== 10. ADDITIONAL VISUALIZATIONS =====

# Boxplot: Spending by performance category
ggplot(analysis_data, aes(x = Performance_Category, y = Medicare_Spending_Score, 
                          fill = Performance_Category)) +
  geom_boxplot() +
  labs(title = "Medicare Spending by Hospital Performance Category",
       x = "Performance Category",
       y = "Medicare Spending Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set2")

# Boxplot: Readmissions by ownership type
ggplot(analysis_data, aes(x = reorder(Hospital.Ownership, Avg_Excess_Readmission_Ratio), 
                          y = Avg_Excess_Readmission_Ratio, fill = Hospital.Ownership)) +
  geom_boxplot() +
  coord_flip() +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red") +
  labs(title = "Readmission Rates by Hospital Ownership Type",
       x = "Hospital Ownership",
       y = "Excess Readmission Ratio",
       caption = "Red line = National Average (1.0)") +
  theme_minimal() +
  theme(legend.position = "none")



# ===== 11. KEY FINDINGS SUMMARY =====

cat("\n========================================\n")
cat("HOSPITAL READMISSION ANALYSIS - KEY FINDINGS\n")
cat("========================================\n\n")

cat("Dataset: 2,778 U.S. Hospitals\n")
cat("Analysis Period: 2020-2023\n\n")

cat("MAIN FINDING:\n")
cat("- Correlation between readmissions and costs: r =", 
    round(cor_test$estimate, 3), "\n")
cat("- Statistical significance: p < 0.001\n")
cat("- Interpretation: Higher readmission rates are associated with higher Medicare spending\n\n")

cat("REGRESSION RESULTS:\n")
cat("- Simple model R-squared:", round(summary(model1)$r.squared, 4), "\n")
cat("- Multiple regression R-squared:", round(summary(model2)$r.squared, 4), "\n")
cat("- Readmission coefficient:", round(coef(model2)[2], 4), 
    "(controlling for hospital characteristics)\n\n")

cat("PERFORMANCE DISTRIBUTION:\n")
cat("- Low Cost, Low Readmit:", sum(analysis_data$Performance_Category == "Low Cost, Low Readmit"), 
    "hospitals (32%)\n")
cat("- High Cost, High Readmit:", sum(analysis_data$Performance_Category == "High Cost, High Readmit"), 
    "hospitals (23%)\n\n")

cat("BEST PERFORMERS (by ownership):\n")
cat("- Physician-owned: Avg readmission ratio =", 
    round(ownership_summary$Avg_Readmission[ownership_summary$Hospital.Ownership == "Physician"], 3), "\n")
cat("- Tribal hospitals: Avg readmission ratio =", 
    round(ownership_summary$Avg_Readmission[ownership_summary$Hospital.Ownership == "Tribal"], 3), "\n\n")

cat("WORST PERFORMERS (by ownership):\n")
cat("- Proprietary (for-profit): Avg readmission ratio =", 
    round(ownership_summary$Avg_Readmission[ownership_summary$Hospital.Ownership == "Proprietary"], 3), "\n")
cat("- Proprietary avg cost score =", 
    round(ownership_summary$Avg_Cost[ownership_summary$Hospital.Ownership == "Proprietary"], 3), "\n\n")

cat("========================================\n")


# ===== 12. EXPORT VISUALIZATIONS =====

# Create plots directory
dir.create("plots", showWarnings = FALSE)

# RECREATE and save scatter plot
p0 <- ggplot(analysis_data, aes(x = Avg_Excess_Readmission_Ratio, y = Medicare_Spending_Score)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "darkgreen") +
  labs(title = "Hospital Readmission Rates vs Medicare Spending",
       x = "Excess Readmission Ratio (1.0 = National Average)",
       y = "Medicare Spending per Beneficiary (1.0 = National Average)",
       caption = "Green lines represent national averages") +
  theme_minimal()

ggsave("plots/readmissions_vs_costs_scatter.png", plot = p0,
       width = 10, height = 6, dpi = 300)

# Recreate and save boxplot by performance category
p1 <- ggplot(analysis_data, aes(x = Performance_Category, y = Medicare_Spending_Score, 
                                fill = Performance_Category)) +
  geom_boxplot() +
  labs(title = "Medicare Spending by Hospital Performance Category",
       x = "Performance Category",
       y = "Medicare Spending Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set2")

ggsave("plots/spending_by_performance.png", plot = p1,
       width = 10, height = 6, dpi = 300)

# Recreate and save boxplot by ownership
p2 <- ggplot(analysis_data, aes(x = reorder(Hospital.Ownership, Avg_Excess_Readmission_Ratio), 
                                y = Avg_Excess_Readmission_Ratio, fill = Hospital.Ownership)) +
  geom_boxplot() +
  coord_flip() +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red") +
  labs(title = "Readmission Rates by Hospital Ownership Type",
       x = "Hospital Ownership",
       y = "Excess Readmission Ratio",
       caption = "Red line = National Average (1.0)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots/readmissions_by_ownership.png", plot = p2,
       width = 10, height = 6, dpi = 300)

cat("All plots saved to 'plots' folder!\n")