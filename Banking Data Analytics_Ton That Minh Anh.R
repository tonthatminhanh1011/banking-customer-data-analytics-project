# Section 1: Setup + Load Data
rm(list = ls())
graphics.off()
set.seed(80007)

##---Packages---
library(readxl)
library(dplyr)
library(janitor)
library(skimr)
library(ggplot2)
library(performance)
library(modelsummary)
library(sjPlot)
library(psych)
library(readr)
library(stringr)
library(tidyr)

# Section 1 - Setup & Load Data
##---Load Data---
cat("\n=== SECTION 1: SETUP & LOAD DATA ===\n")
cat("\n=== 1.1. Load Data ===\n")
bank_marketing_dataset <- read_excel("bank_marketing_dataset.xlsx")
View(bank_marketing_dataset)

##---Confirm Structure---
cat("\n--- 1.2. Data loaded ---\n")
print(paste("Dimensions:", nrow(bank_marketing_dataset), "rows x", ncol(bank_marketing_dataset), "columns"))
print(paste("Target variable:", names(bank_marketing_dataset)[21]))


# Section 2 - Exploratory Data Analysis
cat("\n=== SECTION 2: EXPLORATORY DATA ANALYSIS ===\n")
cat("\n--- 2.1. Define Predictors ---\n")
predictors <- c(
  "job", "education",                                  # Domain 1: Capacity & Life Stage
  "default", "housing",                                # Domain 2: Financial Situation
  "poutcome", "previous", "pdays", "contact",          # Domain 3: Engagement
  "nr_employed", "euribor3m", "emp_var_rate", "month"  # Domain 4: Macroecon Environment
)
target <- "response"

## 2.2. Subset to Analysis Dataset
cat("\n--- 2.2. Subset to Analysis Dataset ---\n")
df_analysis <- bank_marketing_dataset %>% 
  select(all_of(predictors), all_of(target)) %>%
  janitor::clean_names()
cat("\n--- Analysis dataset dimensions ---\n")
print(dim(df_analysis))

## 2.3. Numeric Predictors
cat("\n--- 2.3. Numeric Predictors ---\n")
numeric_cols <- c("previous", "pdays", "nr_employed", "euribor3m", "emp_var_rate")

## 2.4. Categorical Preditors
cat("\n--- 2.4. Categorical Predictors ---\n")
categorical_cols <- setdiff(predictors, numeric_cols)
cat("\n--- Numeric Predictors Summary ---\n")
psych::describe(df_analysis %>% select(all_of(numeric_cols)))

## 2.5. Visualizations
cat("\n--- VISUALIZATION: Class Distribution ---\n")

# Calculate class distribution
class_dist <- data.frame(
  class = c("No (Non-Adopters)", "Yes (Adopters)"),
  count = c(sum(df_analysis$response == "no"), sum(df_analysis$response == "yes"))
)
class_dist$percent <- round(class_dist$count / sum(class_dist$count) * 100, 2)

# Create donut chart
ggplot(class_dist, aes(x = 2, y = percent, fill = class)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c("No (Non-Adopters)" = "#CCCCCC", 
                               "Yes (Adopters)" = "#FF6600")) +
  geom_text(aes(label = paste0(percent, "%")), 
            position = position_stack(vjust = 0.5),
            size = 6, fontface = "bold") +
  labs(title = "CLASS IMBALANCE: Target Variable Distribution",
       subtitle = paste0("Imbalance Ratio: ", round(class_dist$count[1]/class_dist$count[2], 2), ":1"),
       fill = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

ggsave("slide_6_class_imbalance.png", width = 8, height = 6, dpi = 300)


# Section 3 - Data Preprocessing
## 3.1. Handling Missing Values
cat("\n=== SECTION 3: DATA PREPROCESSING ===\n")
cat("\n--- 3.1. pdays handling (DROP) ---\n")

p999_n  <- sum(df_analysis$pdays == 999, na.rm = TRUE)
p999_pct <- mean(df_analysis$pdays == 999, na.rm = TRUE) * 100

cat("pdays=999 count:", p999_n, "\n")
cat("pdays=999 percent:", round(p999_pct, 2), "%\n")
cat("Decision: Drop pdays from modeling due to extreme dominance of 999.\n")

## 3.2. Transform Skewed Numeric Variables
cat("\n--- 3.2. Transform Skewed Numeric Variables ---\n")

# Log-transform 'previous' to handle right-skewness
df_analysis <- df_analysis %>%
  mutate(
    previous_log = log(previous + 1)
  )

cat("\n--- previous (before transformation) ---\n")
cat("Summary statistics:\n")
desc_before <- psych::describe(df_analysis$previous)
print(desc_before)

cat("\nDistribution:\n")
print(table(df_analysis$previous))

cat("\n--- previous_log (after log transformation) ---\n")
cat("Summary statistics:\n")
desc_after <- psych::describe(df_analysis$previous_log)
print(desc_after)

cat("\nSkewness improvement:\n")
cat("Before: Skewness =", round(desc_before$skew, 3), "\n")
cat("After:  Skewness =", round(desc_after$skew, 3), "\n")

cat("\n--- VISUALIZATION: Skewness Transformation ---\n")

library(gridExtra)

# Before transformation
p1 <- ggplot(df_analysis, aes(x = previous)) +
  geom_histogram(bins = 30, fill = "#FF6666", alpha = 0.7, color = "white") +
  labs(title = "BEFORE: 'previous' (Right-Skewed)",
       subtitle = paste0("Skewness: ", round(psych::describe(df_analysis$previous)$skew, 3)),
       x = "Previous Contacts", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# After transformation
p2 <- ggplot(df_analysis, aes(x = previous_log)) +
  geom_histogram(bins = 30, fill = "#66DD66", alpha = 0.7, color = "white") +
  labs(title = "AFTER: 'previous_log' (Normalized)",
       subtitle = paste0("Skewness: ", round(psych::describe(df_analysis$previous_log)$skew, 3)),
       x = "Log(Previous Contacts + 1)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# Combine plots
grid.arrange(p1, p2, ncol = 2)

ggsave("section_3_transformation.png", arrangeGrob(p1, p2, ncol = 2), 
       width = 12, height = 5, dpi = 300)

cat("\n--- Section 3.2 Complete ---\n")

## 3.3. Create Analysis Dataset with Transformed Variables
cat("\n--- 3.3. Create Analysis Dataset with Transformed Variables ---\n")

# Select: keep categorical + transformed numeric + target
df_model <- df_analysis %>%
  select(
    # Categorical
    all_of(categorical_cols),
    # Numeric 
    nr_employed, euribor3m, emp_var_rate,
    # Transformed version (only previous_log now - pdays dropped)
    previous_log,
    # Target
    response
  ) %>%
  # IMPUTE MISSING VALUES (if any)
  mutate(
    previous_log = replace_na(previous_log, 0)
  )

cat("Modeling dataset dimensions:", dim(df_model), "\n")

cat("\nMissing values after imputation:\n")
print(colSums(is.na(df_model)))

cat("\nColumn names:\n")
print(names(df_model))

cat("\nFinal variables for modeling:\n")
cat("- Categorical:", paste(categorical_cols, collapse=", "), "\n")
cat("- Numeric: nr_employed, euribor3m, emp_var_rate, previous_log\n")
cat("- Target: response\n")

cat("\n--- Section 3.3 Complete ---\n")

## 3.4. Create Dummy Variables
cat("\n--- 3.4. Create Dummy Variables ---\n")
# Encode categorical variables
# model.matrix() creates dummies, then convert to data.frame
dummy_matrix <- model.matrix(
  ~job + education + default + housing + poutcome + contact + month - 1,
  data = df_model
)
df_dummies <- as.data.frame(dummy_matrix)
cat("Dummy variables created:\n")
print(colnames(df_dummies))

# Combine dummies with numeric + target
df_model_encoded <- cbind(
  df_dummies,
  df_model %>% select(nr_employed, euribor3m, emp_var_rate, previous_log, response)
)
cat("\n--- Encoded dataset dimensions ---\n")
print(paste("Rows:", nrow(df_model_encoded)))
print(paste("Columns:", ncol(df_model_encoded)))
cat("\nFirst 10 column names:\n")
print(head(colnames(df_model_encoded), 10))

## 3.5. Correlation and Multicollinearity Check
cat("\n--- 3.5. Correlation & Multicollinearity Check ---\n")

# Get all numeric columns (dummies + original numeric)
numeric_for_vif <- df_model_encoded %>%
  select(-response)

# Correlation matrix
corr_mat <- cor(numeric_for_vif, use = "complete.obs")

# Identify high correlations (> 0.7 or < -0.7)
high_corr_pairs <- which(abs(corr_mat) > 0.7 & row(corr_mat) != col(corr_mat), arr.ind = TRUE)
if(nrow(high_corr_pairs) > 0) {
  cat("HIGH CORRELATIONS (|r| > 0.7):\n")
  high_corr_unique <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
  
  for(i in 1:nrow(high_corr_unique)) {
    r <- high_corr_unique[i, 1]
    c <- high_corr_unique[i, 2]
    cat(colnames(corr_mat)[r], "  <->  ", colnames(corr_mat)[c], " : ", 
        round(corr_mat[r, c], 3), "\n")
  }
} else {
  cat("No high correlations (|r| > 0.7) detected.\n")
}

# Remove redundant variables due to multicollinearity
df_model_encoded_reduced <- df_model_encoded %>%
  select(
    -emp_var_rate,        # Drop: high corr with euribor3m (r≈0.97)
    -nr_employed          # Drop: high corr with euribor3m (r≈0.95)
  )

cat("\nVariables removed due to multicollinearity:\n")
cat("- emp_var_rate (corr with euribor3m: 0.972)\n")
cat("- nr_employed (corr with euribor3m: 0.945)\n")
cat("\nRationale: Keep euribor3m as the primary macroeconomic indicator.\n")
cat("\nNew dimensions:", dim(df_model_encoded_reduced), "\n")

cat("\n--- Section 3.5 Complete ---\n")

## 3.6. Stratified Train/Test Split
cat("\n--- 3.6. Stratified Train/Test Split ---\n")

set.seed(80007)

# Stratified split on response variable (maintains class balance in both sets)
train_idx <- createDataPartition(
  df_model_encoded_reduced$response,
  p = 0.8,
  list = FALSE
)

df_train <- df_model_encoded_reduced[train_idx, ]
df_test <- df_model_encoded_reduced[-train_idx, ]

cat("Training set size:", nrow(df_train), "rows\n")
cat("Test set size:", nrow(df_test), "rows\n")
cat("Total:", nrow(df_train) + nrow(df_test), "rows\n")

# Check class balance in both sets
cat("\n--- CLASS DISTRIBUTION ---\n")
cat("Training set:\n")
print(table(df_train$response))
cat("Proportions:\n")
print(round(prop.table(table(df_train$response)), 4))

cat("\nTest set:\n")
print(table(df_test$response))
cat("Proportions:\n")
print(round(prop.table(table(df_test$response)), 4))

cat("\nImbalance ratio (no/yes):\n")
cat("Training:", round(sum(df_train$response == "no") / sum(df_train$response == "yes"), 2), ":1\n")
cat("Test:", round(sum(df_test$response == "no") / sum(df_test$response == "yes"), 2), ":1\n")

cat("\n--- Section 3.6 Complete ---\n")


# Section 4: Handling Class Imbalance
cat("\n=== SECTION 4: HANDLE CLASS IMBALANCE WITH ROSE ===\n")

# Fix column names (hyphens → underscores)
colnames(df_train) <- gsub("-", "_", colnames(df_train))
colnames(df_test) <- gsub("-", "_", colnames(df_test))

cat("\n--- Before ROSE (Training Set) ---\n")
cat("Response distribution:\n")
print(table(df_train$response))
cat("Proportions:\n")
print(round(prop.table(table(df_train$response)), 4))
imbalance_ratio <- sum(df_train$response == "no") / sum(df_train$response == "yes")
cat("Imbalance ratio (no/yes):", round(imbalance_ratio, 2), ":1\n")

# Apply ROSE to training set ONLY
set.seed(80007)
df_train_rose <- ROSE(
  response ~ .,
  data = df_train,
  seed = 80007,
  N = nrow(df_train),
  p = 0.5
)$data

cat("\n--- After ROSE (Training Set) ---\n")
cat("Response distribution:\n")
print(table(df_train_rose$response))
cat("Proportions:\n")
print(round(prop.table(table(df_train_rose$response)), 4))
imbalance_ratio_rose <- sum(df_train_rose$response == "no") / sum(df_train_rose$response == "yes")
cat("Imbalance ratio (no/yes):", round(imbalance_ratio_rose, 2), ":1\n")

cat("\n--- Test Set (UNCHANGED - No Resampling) ---\n")
cat("Response distribution:\n")
print(table(df_test$response))
cat("Proportions:\n")
print(round(prop.table(table(df_test$response)), 4))

cat("\nTraining set before ROSE:", nrow(df_train), "rows\n")
cat("Training set after ROSE:", nrow(df_train_rose), "rows\n")

cat("\n=== SECTION 4 COMPLETE ===\n")



# Section 5 - Logistic Regression
## 5.1. Model Fitting
cat("\n=== SECTION 5: LOGISTIC REGRESSION MODEL ===\n")
cat("\n--- 5.1. Model Fitting ---\n")

# Convert response to numeric (yes=1, no=0)
df_train_rose$response_numeric <- as.numeric(df_train_rose$response == "yes")
df_test$response_numeric <- as.numeric(df_test$response == "yes")

# Fit logistic regression on ROSE-balanced training data
model_logistic <- glm(
  response_numeric ~ . - response, 
  family = binomial(link = "logit"),
  data = df_train_rose
)

cat("\n--- Model Summary ---\n")
print(summary(model_logistic))

cat("\n--- Business Interpretation of Key Coefficients ---\n")
cat("Top predictors by impact on conversion:\n\n")

coefs_sorted <- sort(abs(coef(model_logistic))[-1], decreasing=TRUE)[1:8]

for(i in 1:length(coefs_sorted)) {
  var_name <- names(coefs_sorted)[i]
  coef_value <- coef(model_logistic)[var_name]
  
  cat(sprintf("%d. %s = %.4f", i, var_name, coef_value))
  
  if(coef_value > 0) {
    cat(" (increases conversion probability)\n")
  } else {
    cat(" (decreases conversion probability)\n")
  }
}

cat("\n--- Model Performance (Training Set) ---\n")
# Predictions on training set
train_pred_prob <- predict(model_logistic, newdata = df_train_rose, type = "response")
train_pred_class <- ifelse(train_pred_prob >= 0.5, 1, 0)

# Confusion matrix on training
cat("Training Confusion Matrix:\n")
print(table(Predicted = train_pred_class, Actual = df_train_rose$response_numeric))

# Accuracy
train_acc <- mean(train_pred_class == df_train_rose$response_numeric)
cat("Training Accuracy:", round(train_acc, 4), "\n")

cat("\n=== SECTION 5.1 COMPLETE ===\n")

## 5.2. Predictions on Test Set
cat("\n--- 5.2. Predictions on Test Set ---\n")

# Make probability predictions on test set
test_pred_prob <- predict(model_logistic, newdata = df_test, type = "response")

# Convert to class predictions (0.5 threshold)
test_pred_class <- ifelse(test_pred_prob >= 0.5, 1, 0)

# Confusion matrix on test
cat("Test Confusion Matrix:\n")
print(table(Predicted = test_pred_class, Actual = df_test$response_numeric))

# Calculate metrics
test_acc <- mean(test_pred_class == df_test$response_numeric)
cat("\nTest Accuracy:", round(test_acc, 4), "\n")

# Sensitivity & Specificity
tn <- sum(test_pred_class == 0 & df_test$response_numeric == 0)
fp <- sum(test_pred_class == 1 & df_test$response_numeric == 0)
fn <- sum(test_pred_class == 0 & df_test$response_numeric == 1)
tp <- sum(test_pred_class == 1 & df_test$response_numeric == 1)

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)

cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("F1-Score:", round(f1, 4), "\n")

cat("\n=== SECTION 5.2 COMPLETE ===\n")

## 5.3. Model Evaluation
### 5.3.1. ROC Curve & AUC Score
cat("\n--- 5.3. Model Evaluation ---\n")

# PART 1: ROC CURVE & AUC SCORE

cat("\n=== PART 1: ROC CURVE & AUC (Area Under Curve) ===\n")

# Calculate ROC curve
roc_logistic <- roc(
  response = df_test$response_numeric,
  predictor = test_pred_prob,
  levels = c(0, 1),
  direction = "<"
)

auc_score <- auc(roc_logistic)

cat("\nAUC Score:", round(auc_score, 4), "\n")
cat("\nAUC Interpretation:\n")
cat("0.50 = Random guessing (useless model)\n")
cat("0.70 = Acceptable discrimination\n")
cat("0.80 = Excellent discrimination\n")
cat("0.90 = Outstanding discrimination\n\n")

if(auc_score < 0.70) {
  cat("Your AUC: ", round(auc_score, 4), " (Acceptable - room for improvement)\n")
} else if(auc_score < 0.80) {
  cat("Your AUC: ", round(auc_score, 4), " (Good discrimination)\n")
} else if(auc_score < 0.90) {
  cat("Your AUC: ", round(auc_score, 4), " (Excellent discrimination)\n")
} else {
  cat("Your AUC: ", round(auc_score, 4), " (Outstanding discrimination)\n")
}

cat("\n--- VISUALIZATION: ROC Curve ---\n")

# Plot ROC curve
plot(roc_logistic, 
     col = "#0066CC", 
     lwd = 3,
     main = "ROC Curve: Logistic Regression",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     print.auc = TRUE,
     print.auc.x = 0.6,
     print.auc.y = 0.2,
     auc.polygon = TRUE,
     auc.polygon.col = "#CCDDFF",
     grid = TRUE)

# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "red", lwd = 2)
legend("bottomright", 
       legend = c(paste0("Logistic (AUC = ", round(auc_score, 4), ")"), "Random (AUC = 0.50)"),
       col = c("#0066CC", "red"),
       lwd = c(3, 2),
       lty = c(1, 2))

png("slide_8_roc_curve.png", width = 800, height = 600, res = 120)
plot(roc_logistic, col = "#0066CC", lwd = 3,
     main = "ROC Curve: Logistic Regression",
     print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "#CCDDFF")
abline(a = 0, b = 1, lty = 2, col = "red", lwd = 2)
legend("bottomright", 
       legend = c(paste0("Logistic (AUC = ", round(auc_score, 4), ")"), "Random (AUC = 0.50)"),
       col = c("#0066CC", "red"), lwd = c(3, 2), lty = c(1, 2))
dev.off()

### 5.3.2. Threshold Optimization
# PART 2: THRESHOLD OPTIMIZATION
cat("\n=== PART 2: THRESHOLD OPTIMIZATION ===\n")
cat("Testing different decision thresholds (0.3 to 0.8)...\n\n")

thresholds <- seq(0.3, 0.8, by = 0.05)
threshold_results <- data.frame(
  Threshold = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Precision = numeric(),
  F1_Score = numeric(),
  Accuracy = numeric()
)

for(thresh in thresholds) {
  pred_class <- ifelse(test_pred_prob >= thresh, 1, 0)
  
  tp <- sum(pred_class == 1 & df_test$response_numeric == 1)
  tn <- sum(pred_class == 0 & df_test$response_numeric == 0)
  fp <- sum(pred_class == 1 & df_test$response_numeric == 0)
  fn <- sum(pred_class == 0 & df_test$response_numeric == 1)
  
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  threshold_results <- rbind(threshold_results, data.frame(
    Threshold = thresh,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1_Score = f1,
    Accuracy = accuracy
  ))
}

cat("THRESHOLD PERFORMANCE COMPARISON:\n\n")
print(round(threshold_results, 4))

best_f1_idx <- which.max(threshold_results$F1_Score)
best_threshold <- threshold_results$Threshold[best_f1_idx]

cat("OPTIMAL THRESHOLD:\n")
cat("Best F1-Score at threshold:", best_threshold, "\n")
cat("- Sensitivity:", round(threshold_results$Sensitivity[best_f1_idx], 4), "\n")
cat("- Specificity:", round(threshold_results$Specificity[best_f1_idx], 4), "\n")
cat("- Precision:", round(threshold_results$Precision[best_f1_idx], 4), "\n")
cat("- Accuracy:", round(threshold_results$Accuracy[best_f1_idx], 4), "\n")

cat("\n--- VISUALIZATION: Threshold vs Metrics ---\n")

# Reshape data for plotting
library(reshape2)
threshold_long <- melt(threshold_results, id.vars = "Threshold")

# Create multi-metric plot
ggplot(threshold_long, aes(x = Threshold, y = value, color = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Sensitivity" = "#FF6600",
    "Specificity" = "#0066CC",
    "Precision" = "#00AA33",
    "F1_Score" = "#FF00FF",
    "Accuracy" = "#666666"
  )) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = seq(0.3, 0.8, 0.05)) +
  geom_vline(xintercept = best_threshold, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = best_threshold + 0.05, y = 0.95, 
           label = paste0("Optimal: ", best_threshold), 
           size = 4, fontface = "bold", color = "red") +
  labs(
    title = "THRESHOLD OPTIMIZATION: Trade-offs Across Metrics",
    subtitle = "Balance sensitivity (recall), precision, and F1-Score",
    x = "Classification Threshold",
    y = "Metric Value",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "lightgray")
  )

ggsave("slide_10_threshold_metrics.png", width = 11, height = 6, dpi = 300)

### 5.3.3. Confusion Matrices at Different Thresholds
# PART 3: CONFUSION MATRICES AT DIFFERENT THRESHOLDS
cat("\n=== PART 3: CONFUSION MATRICES AT KEY THRESHOLDS ===\n")

key_thresholds <- c(0.3, 0.5, best_threshold, 0.7)

for(thresh in key_thresholds) {
  pred_class <- ifelse(test_pred_prob >= thresh, 1, 0)
  
  cat("\n--- THRESHOLD:", thresh, "---\n")
  confusion_mat <- table(Predicted = pred_class, Actual = df_test$response_numeric)
  print(confusion_mat)
  
  tp <- confusion_mat[2, 2]
  tn <- confusion_mat[1, 1]
  fp <- confusion_mat[2, 1]
  fn <- confusion_mat[1, 2]
  
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  
  cat(sprintf("Sensitivity: %.4f | Specificity: %.4f | Precision: %.4f | Accuracy: %.4f\n",
              sensitivity, specificity, precision, accuracy))
}

### 5.3.4. Lift Analysis
# PART 4: LIFT ANALYSIS
cat("\n=== PART 4: LIFT ANALYSIS (Business Impact) ===\n")

percentiles <- c(5, 10, 15, 20)
total_positives <- sum(df_test$response_numeric == 1)
test_size <- nrow(df_test)

pred_data <- data.frame(
  actual = df_test$response_numeric,
  prob = test_pred_prob
)
pred_data <- pred_data[order(pred_data$prob, decreasing = TRUE), ]

lift_results <- data.frame(
  Percentile = numeric(),
  Num_Contacted = numeric(),
  Positives_Captured = numeric(),
  Capture_Rate = numeric(),
  Lift = numeric(),
  Precision = numeric()
)

for(pct in percentiles) {
  n_contact <- ceiling(test_size * pct / 100)
  positives_in_top <- sum(pred_data$actual[1:n_contact] == 1)
  capture_rate <- positives_in_top / total_positives
  random_capture_rate <- pct / 100
  lift <- capture_rate / random_capture_rate
  precision <- positives_in_top / n_contact
  
  lift_results <- rbind(lift_results, data.frame(
    Percentile = pct,
    Num_Contacted = n_contact,
    Positives_Captured = positives_in_top,
    Capture_Rate = capture_rate,
    Lift = lift,
    Precision = precision
  ))
}

cat("LIFT RESULTS:\n\n")
print(round(lift_results, 4))

cat("\n--- VISUALIZATION: Lift Chart ---\n")

# Create lift bar chart
ggplot(lift_results, aes(x = factor(Percentile), y = Lift, fill = Lift)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = paste0(round(Lift, 2), "x")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#FFAA00", high = "#FF0000", guide = "none") +
  scale_y_continuous(limits = c(0, 6)) +
  annotate("text", x = 0.5, y = 1.2, label = "Baseline (1.0x)", 
           size = 4, color = "red", fontface = "bold") +
  labs(
    title = "LIFT ANALYSIS: Logistic Regression Performance",
    subtitle = "Model identifies 5.32x more converters in top 5% vs random",
    x = "Top Percentile of Customers (by Score)",
    y = "Lift (vs Random Baseline)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "lightgray"),
    panel.grid.major.x = element_blank()
  )

ggsave("slide_11_lift_analysis_logistic.png", width = 10, height = 6, dpi = 300)

cat("\n--- SECTION 5.3 COMPLETE ---\n")

### 5.3.5. Profit Curve Analysis
# PART 5: PROFIT CURVE ANALYSIS
cat("\n=== PART 5: PROFIT CURVE ANALYSIS ===\n")

thresholds <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
cost_per_contact <- 1        # €1
revenue_per_convert <- 100   # €100

profit_df <- data.frame(
  Threshold = numeric(),
  Contacts = numeric(),
  Converters = numeric(),
  Profit = numeric()
)

for(thresh in thresholds) {
  pred <- ifelse(test_pred_prob >= thresh, 1, 0)
  n_contacts <- sum(pred)
  n_converters <- sum(pred == 1 & df_test$response_numeric == 1)
  n_wasted <- n_contacts - n_converters
  
  profit <- (n_converters * revenue_per_convert) - (n_contacts * cost_per_contact)
  
  profit_df <- rbind(profit_df, data.frame(
    Threshold = thresh,
    Contacts = n_contacts,
    Converters = n_converters,
    Profit = profit
  ))
}

cat("PROFIT ANALYSIS:\n")
print(profit_df)

best_profit_threshold <- profit_df$Threshold[which.max(profit_df$Profit)]
cat("\nMaximum profit at threshold:", best_profit_threshold, "\n")
cat("Recommendation: Use threshold", best_profit_threshold, "for maximum ROI\n")

cat("\n--- VISUALIZATION: Profit Curve ---\n")

# Add model label
profit_df$Model <- "Logistic Regression"

# Create profit curve
ggplot(profit_df, aes(x = Threshold, y = Profit)) +
  geom_line(color = "#0066CC", linewidth = 1.5) +
  geom_point(size = 3, color = "#0066CC") +
  geom_point(data = profit_df[which.max(profit_df$Profit), ],
             aes(x = Threshold, y = Profit),
             size = 5, shape = 17, color = "#FF0000") +
  geom_text(data = profit_df[which.max(profit_df$Profit), ],
            aes(x = Threshold, y = Profit, 
                label = paste0("Peak: €", format(round(Profit), big.mark = ","))),
            vjust = -1.5, size = 5, fontface = "bold", color = "#FF0000") +
  scale_y_continuous(labels = function(x) paste0("€", format(x/1000, digits = 1), "k")) +
  scale_x_continuous(breaks = seq(0.3, 0.8, 0.1)) +
  labs(
    title = "PROFIT CURVE: Expected ROI by Threshold",
    subtitle = "Cost: €1/contact | Revenue: €100/conversion",
    x = "Classification Threshold",
    y = "Expected Profit (€)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "lightgray")
  )

ggsave("slide_12_profit_curve_logistic.png", width = 10, height = 6, dpi = 300)


# 6. Decision Tree
cat("\n=== SECTION 6: DECISION TREE (CLASS-WEIGHTED) ===\n")

# Use original training set (not ROSE-resampled)
df_train_tree <- df_train %>% dplyr::select(-dplyr::any_of("response_numeric"))
df_test_tree  <- df_test  %>% dplyr::select(-dplyr::any_of("response_numeric"))

df_train_tree$response <- factor(df_train_tree$response, levels = c("no","yes"))
df_test_tree$response  <- factor(df_test_tree$response,  levels = c("no","yes"))

# 1) Calculate class weights (inversely proportional to frequency)
class_freq <- table(df_train_tree$response)
class_weights <- 1 / class_freq
class_weights <- class_weights / sum(class_weights) * 2  # normalize to sum to 2

cat("Class weights: no =", class_weights["no"], ", yes =", class_weights["yes"], "\n")

# 2) Fit tree with class weights (handles imbalance without resampling)
set.seed(80007)
model_tree <- rpart(
  response ~ .,
  data = df_train_tree,
  method = "class",
  weights = ifelse(df_train_tree$response == "yes", class_weights["yes"], class_weights["no"]),
  control = rpart.control(cp = 0.001, minsplit = 20, minbucket = 10, maxdepth = 6, xval = 10)
)

cat("\n--- Initial tree (class-weighted) ---\n")
print(model_tree)

cat("\n--- Cross-validation CP table ---\n")
printcp(model_tree)

best_cp <- model_tree$cptable[which.min(model_tree$cptable[, "xerror"]), "CP"]
cat("\nBest CP:", best_cp, "\n")

model_tree_pruned <- prune(model_tree, cp = best_cp)

# 3) Plot pruned tree
rpart.plot(
  model_tree_pruned,
  main = "Decision Tree: Insurance Adoption Prediction (Class-Weighted)",
  extra = 104,                  # Show probabilities + node info
  box.palette = "RdYlGn",       # Red-Yellow-Green color scheme
  branch.lty = 3,               # Dotted branches
  shadow.col = "gray",          # Shadow for depth
  nn = TRUE,                    # Display node numbers
  fallen.leaves = TRUE,         # Leaves at bottom
  cex.main = 1.3,
  roundint = FALSE)

  # 4) Variable importance
  cat("\n--- Variable Importance (Top 15) ---\n")
  var_imp_tree <- sort(model_tree_pruned$variable.importance, decreasing = TRUE)
  print(head(var_imp_tree, 15))
  
  # 5) Test predictions
  test_pred_prob_tree <- predict(model_tree_pruned, newdata = df_test_tree, type = "prob")[, "yes"]
  test_pred_class_tree <- ifelse(test_pred_prob_tree >= 0.5, "yes", "no") |> 
    factor(levels = c("no","yes"))
  
  # 6) Confusion matrix
  cat("\n=== Test Set Performance (Decision Tree) ===\n")
  cm_tree <- confusionMatrix(
    data = test_pred_class_tree,
    reference = df_test_tree$response,
    positive = "yes"
  )
  print(cm_tree)
  
  ## Profit Analysis for Decision Tree
  # === PROFIT ANALYSIS FOR DECISION TREE ===
  cat("\n=== PROFIT ANALYSIS: DECISION TREE ===\n")
  
  thresholds <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  cost_per_contact <- 1
  revenue_per_convert <- 100
  
  # Get tree probabilities
  profit_df_tree <- data.frame(
    Threshold = numeric(),
    Contacts = numeric(),
    Converters = numeric(),
    Profit = numeric()
  )
  
  for(thresh in thresholds) {
    pred <- ifelse(test_pred_prob_tree >= thresh, 1, 0)
    n_contacts <- sum(pred)
    n_converters <- sum(pred == 1 & as.numeric(df_test_tree$response == "yes"))
    
    profit <- (n_converters * revenue_per_convert) - (n_contacts * cost_per_contact)
    
    profit_df_tree <- rbind(profit_df_tree, data.frame(
      Threshold = thresh,
      Contacts = n_contacts,
      Converters = n_converters,
      Profit = profit
    ))
  }
  
  cat("PROFIT ANALYSIS - DECISION TREE:\n")
  print(profit_df_tree)
  
  max_profit_tree <- max(profit_df_tree$Profit)
  best_threshold_tree <- profit_df_tree$Threshold[which.max(profit_df_tree$Profit)]
  
  cat("\n--- Comparison: Logistic vs Tree ---\n")
  cat("Tree max profit:", max_profit_tree, "EUR (at threshold", best_threshold_tree, ")\n")
  cat("Logistic max profit: €79,391 EUR\n")
  cat("Difference:", 79391 - max_profit_tree, "EUR (", 
      round((79391 - max_profit_tree)/79391 * 100, 1), "% less)\n")
  
  cat("\n--- VISUALIZATION: Profit Curve Comparison ---\n")
  
  # Combine profit data
  profit_df$Model <- "Logistic Regression"
  profit_df_tree$Model <- "Decision Tree"
  profit_combined <- rbind(profit_df, profit_df_tree)
  
  # Create comparison plot
  ggplot(profit_combined, aes(x = Threshold, y = Profit, color = Model)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    # Mark peaks
    geom_point(data = profit_combined[profit_combined$Model == "Logistic Regression", ][which.max(profit_df$Profit), ],
               size = 5, shape = 17, color = "#0066CC") +
    geom_point(data = profit_combined[profit_combined$Model == "Decision Tree", ][which.max(profit_df_tree$Profit), ],
               size = 5, shape = 17, color = "#FF9900") +
    annotate("text", x = 0.3, y = 79000, 
             label = paste0("Logistic Peak: €", format(round(max(profit_df$Profit)), big.mark = ",")),
             size = 4, color = "#0066CC", fontface = "bold") +
    annotate("text", x = 0.7, y = 65000, 
             label = paste0("Tree Peak: €", format(round(max(profit_df_tree$Profit)), big.mark = ",")),
             size = 4, color = "#FF9900", fontface = "bold") +
    scale_color_manual(values = c("Logistic Regression" = "#0066CC", 
                                  "Decision Tree" = "#FF9900")) +
    scale_y_continuous(labels = function(x) paste0("€", format(x/1000, digits = 1), "k")) +
    scale_x_continuous(breaks = seq(0.3, 0.8, 0.1)) +
    labs(
      title = "PROFIT CURVE COMPARISON: Logistic vs Decision Tree",
      subtitle = "Logistic generates 18.6% more profit at optimal threshold",
      x = "Classification Threshold",
      y = "Expected Profit (€)",
      color = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "top",
      panel.grid.major = element_line(color = "lightgray")
    )
  
  ggsave("profit_comparison.png", width = 10, height = 6, dpi = 300)
  
  # 7) AUC
  roc_tree <- roc(
    response = as.numeric(df_test_tree$response == "yes"),
    predictor = test_pred_prob_tree,
    levels = c(0,1),
    direction = "<"
  )
  auc_tree <- auc(roc_tree)
  cat("\nDecision Tree AUC:", round(auc_tree, 4), "\n")
  
  # 8) Lift analysis
  cat("\n=== Lift Analysis (Decision Tree) ===\n")
  
  percentiles <- c(5, 10, 15, 20)
  test_size <- nrow(df_test_tree)
  total_pos <- sum(df_test_tree$response == "yes")
  
  ranked_tree <- data.frame(
    actual = as.numeric(df_test_tree$response == "yes"),
    prob = test_pred_prob_tree
  )
  ranked_tree <- ranked_tree[order(ranked_tree$prob, decreasing = TRUE), ]
  
  lift_results_tree <- data.frame(
    Percentile = numeric(),
    Num_Contacted = numeric(),
    Positives_Captured = numeric(),
    Capture_Rate = numeric(),
    Lift = numeric(),
    Precision = numeric()
  )
  
  for(pct in percentiles) {
    n_contact <- ceiling(test_size * pct / 100)
    pos_captured <- sum(ranked_tree$actual[1:n_contact] == 1)
    capture_rate <- pos_captured / total_pos
    lift <- capture_rate / (pct/100)
    precision <- pos_captured / n_contact
    
    lift_results_tree <- rbind(lift_results_tree, data.frame(
      Percentile = pct,
      Num_Contacted = n_contact,
      Positives_Captured = pos_captured,
      Capture_Rate = capture_rate,
      Lift = lift,
      Precision = precision
    ))
  }
  
  print(round(lift_results_tree, 4))
  
  cat("\n--- VISUALIZATION: Lift Comparison (Logistic vs Tree) ---\n")
  
  # Combine lift results
  lift_results$Model <- "Logistic Regression"
  lift_results_tree$Model <- "Decision Tree"
  lift_combined <- rbind(lift_results, lift_results_tree)
  
  # Create grouped bar chart
  ggplot(lift_combined, aes(x = factor(Percentile), y = Lift, fill = Model)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
    geom_text(aes(label = paste0(round(Lift, 2), "x")), 
              position = position_dodge(width = 0.7),
              vjust = -0.3, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Logistic Regression" = "#0066CC", 
                                 "Decision Tree" = "#FF9900")) +
    scale_y_continuous(limits = c(0, 6)) +
    annotate("text", x = 0.5, y = 1.15, label = "Baseline (1.0x)", 
             size = 4, color = "red", fontface = "bold") +
    labs(
      title = "LIFT ANALYSIS: Model Comparison",
      subtitle = "Logistic outperforms Tree at identifying top converters",
      x = "Top Percentile of Customers (by Score)",
      y = "Lift (vs Random Baseline)",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "top",
      axis.text = element_text(size = 11),
      panel.grid.major.y = element_line(color = "lightgray"),
      panel.grid.major.x = element_blank()
    )
  
  ggsave("slide_11_lift_comparison.png", width = 10, height = 6, dpi = 300)
  
  cat("\n--- VISUALIZATION: Variable Importance ---\n")
  
  # Extract variable importance
  var_imp_df <- data.frame(
    Variable = names(model_tree_pruned$variable.importance),
    Importance = model_tree_pruned$variable.importance
  )
  var_imp_df <- var_imp_df[order(var_imp_df$Importance, decreasing = TRUE), ]
  var_imp_df <- head(var_imp_df, 10)
  
  # Create bar chart
  ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
    geom_col(alpha = 0.8) +
    scale_fill_gradient(low = "#FF9900", high = "#FF0000", guide = "none") +
    coord_flip() +
    labs(
      title = "VARIABLE IMPORTANCE: Top 10 Predictors (Decision Tree)",
      subtitle = "euribor3m dominates, followed by poutcome and previous contacts",
      x = "Variable",
      y = "Importance Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major.x = element_line(color = "lightgray"),
      panel.grid.major.y = element_blank()
    )
  
  ggsave("variable_importance.png", width = 10, height = 6, dpi = 300)
  
  cat("\n=== SECTION 6 COMPLETE ===\n")
  
  
  # 7. Model Comparison
  cat("\n=== SECTION 7: MODEL COMPARISON ===\n")
  
  comparison_table <- data.frame(
    Metric = c("AUC", "Accuracy", "Sensitivity", "Specificity", "Precision", 
               "F1-Score", "Lift (Top 5%)", "Lift (Top 10%)",
               "Max Profit (€)", "Interpretability", "Deployment Ease"),
    Logistic = c("0.7861", "78.12%", "67.03%", "79.53%", "29.37%", 
                 "0.4084", "5.32x", "4.18x", 
                 "€79,391", "Medium", "Easy"),
    Tree = c("0.7739", "84.25%", "62.93%", "86.96%", "38.00%",
             "0.4679", "3.53x", "3.91x",
             "€64,624", "High", "Easy"),
    Winner = c("Logistic", "Tree", "Logistic", "Tree", "Tree",
               "Tree", "Logistic", "Logistic",
               "Logistic", "Tree", "Tie")
  )
  
  print(comparison_table)
  
  cat("\n=== KEY INSIGHTS ===\n")
  cat("1. DISCRIMINATION: Logistic (AUC=0.786) slightly better at ranking customers\n")
  cat("2. ACCURACY: Tree (84.25%) more accurate at classification\n")
  cat("3. LIFT: Logistic (5.32x) MUCH better at identifying top converters\n")
  cat("4. PRECISION: Tree (38%) better at avoiding false positives\n")
  cat("5. PROFIT: Logistic (€79,391) generates 35% more revenue\n")
  cat("6. INTERPRETABILITY: Tree easier to explain to non-technical stakeholders\n")
  
  cat("\n=== PROFIT COMPARISON INSIGHT ===\n")
  cat("Tree achieves €64,624 profit (at threshold 0.3)\n")
  cat("Logistic achieves €79,391 profit (at threshold 0.3)\n")
  cat("Difference: €14,767 (18.6% MORE with logistic)\n\n")
  cat("WHY? Logistic's superior lift (5.32x vs 3.53x at top 5%) means:\n")
  cat("- More converters identified in high-probability segment\n")
  cat("- Better customer ranking → higher precision at same threshold\n")
  cat("- 35% profit advantage despite Tree's higher precision at 0.5-0.6\n\n")
  cat("BUSINESS IMPLICATION FOR SIXSEVENBANK:\n")
  cat("For LARGE-SCALE bancassurance campaigns (6000+ contacts), logistic wins\n")
  cat("For PREMIUM insurance products (300-400 high-value customers), Tree may be comparable\n")
  
  
  cat("\n--- VISUALIZATION: Model Comparison Heatmap ---\n")
  
  # Create numeric comparison data
  comparison_numeric <- data.frame(
    Metric = c("AUC", "Accuracy", "Sensitivity", "Specificity", "Precision", "F1-Score", "Lift (Top 5%)"),
    Logistic = c(0.7861, 78.12, 67.03, 79.53, 29.37, 40.84, 5.32),
    Tree = c(0.7739, 84.25, 62.93, 86.96, 38.00, 46.79, 3.53)
  )
  
  # Normalize to 0-100 scale for heatmap
  comparison_numeric$Logistic_scaled <- comparison_numeric$Logistic
  comparison_numeric$Tree_scaled <- comparison_numeric$Tree
  
  # AUC needs scaling (multiply by 100)
  comparison_numeric$Logistic_scaled[1] <- comparison_numeric$Logistic[1] * 100
  comparison_numeric$Tree_scaled[1] <- comparison_numeric$Tree[1] * 100
  
  # Melt for heatmap
  library(reshape2)
  comparison_melted <- melt(comparison_numeric[, c("Metric", "Logistic_scaled", "Tree_scaled")], 
                            id.vars = "Metric")
  colnames(comparison_melted)[2] <- "Model"
  comparison_melted$Model <- gsub("_scaled", "", comparison_melted$Model)
  
  # Create heatmap
  ggplot(comparison_melted, aes(x = Model, y = factor(Metric, levels = rev(comparison_numeric$Metric)), 
                                fill = value)) +
    geom_tile(color = "white", size = 1.5) +
    geom_text(aes(label = round(value, 1)), color = "black", size = 5, fontface = "bold") +
    scale_fill_gradient2(low = "#FF6666", mid = "#FFFF99", high = "#66DD66",
                         midpoint = 50, limits = c(0, 100),
                         name = "Score") +
    labs(
      title = "MODEL COMPARISON SCORECARD",
      subtitle = "Logistic wins on discrimination (AUC, Lift), Tree wins on accuracy",
      x = "Model",
      y = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13, face = "bold"),
      legend.position = "right"
    )
  
  ggsave("slide_13_model_comparison.png", width = 9, height = 7, dpi = 300)
  