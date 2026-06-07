################################################################################
###################      PROGETTO ESEMPIO    #########################

library(ggplot2)

loan_data <- read.csv("loan_data.csv")

#guardare descrizione dataset https://www.kaggle.com/datasets/taweilo/loan-approval-classification-data/data

# First look
head(loan_data)
tail(loan_data)
str(loan_data)
dim(loan_data)
names(loan_data)
summary(loan_data)

# The variable we want to predict is loan_status.
# This is a binary classification problem.
# We will use logistic regression to estimate:
#   P(loan_status = Approved | predictors)

################################################################################
# 2. BASIC DATA CLEANING
################################################################################

# Check missing values by column
missing_by_column <- colSums(is.na(loan_data))
missing_by_column

# Check duplicate rows
sum(duplicated(loan_data))

# Convert categorical variables to factors.
# This is important because glm() treats factors as categorical predictors.
cat_vars <- c(
  "person_gender",
  "person_education",
  "person_home_ownership",
  "loan_intent",
  "previous_loan_defaults_on_file"
)

for (v in cat_vars) {
  loan_data[[v]] <- factor(loan_data[[v]])
}

# Convert outcome to a factor for summaries/plots.
# Keep a numeric copy for logistic regression.
loan_data$loan_status_factor <- factor(
  loan_data$loan_status,
  levels = c(0, 1),
  labels = c("Rejected", "Approved")
)

loan_data$loan_status_num <- loan_data$loan_status

# Check the result
str(loan_data)
table(loan_data$loan_status_factor)
prop.table(table(loan_data$loan_status_factor))

################################################################################
# 3. EXPLORATORY DATA ANALYSIS: TARGET VARIABLE
################################################################################

# Frequency table
status_table <- table(loan_data$loan_status_factor)
status_table

# Percentage table
status_percent <- round(100 * prop.table(status_table), 2)
status_percent

# Bar chart of target variable
ggplot(loan_data, aes(x = loan_status_factor)) +
  geom_bar() +
  labs(
    title = "Distribution of Loan Status",
    x = "Loan status",
    y = "Number of observations"
  ) +
  theme_minimal()

# If one class is much more common than the other, accuracy alone may be misleading.
# We will later compare logistic regression to a simple baseline model.

################################################################################
# 4. EXPLORATORY DATA ANALYSIS: NUMERIC VARIABLES
################################################################################

num_vars <- c(
  "person_age",
  "person_income",
  "person_emp_exp",
  "loan_amnt",
  "loan_int_rate",
  "loan_percent_income",
  "cb_person_cred_hist_length",
  "credit_score"
)

# Summary statistics for numeric variables
summary(loan_data[num_vars])

# Standard deviations
sapply(loan_data[num_vars], sd, na.rm = TRUE)

# Histograms for numeric variables
for (v in num_vars) {
  print(
    ggplot(loan_data, aes_string(x = v)) +
      geom_histogram(bins = 30) +
      labs(
        title = paste("Histogram of", v),
        x = v,
        y = "Count"
      ) +
      theme_minimal()
  )
}

# Boxplots of numeric variables by loan status
for (v in num_vars) {
  print(
    ggplot(loan_data, aes_string(x = "loan_status_factor", y = v)) +
      geom_boxplot() +
      labs(
        title = paste(v, "by Loan Status"),
        x = "Loan status",
        y = v
      ) +
      theme_minimal()
  )
}

# Correlation matrix for numeric variables
cor_matrix <- cor(loan_data[num_vars], use = "complete.obs")
round(cor_matrix, 2)

# Base R correlation plot using pairs
pairs(loan_data[num_vars], main = "Pairs Plot of Numeric Variables")

################################################################################
# 5. EXPLORATORY DATA ANALYSIS: CATEGORICAL VARIABLES
################################################################################

# Frequency tables
for (v in cat_vars) {
  cat("\n==============================\n")
  cat("Variable:", v, "\n")
  print(table(loan_data[[v]]))
  print(round(100 * prop.table(table(loan_data[[v]])), 2))
}

# Bar charts for categorical variables
for (v in cat_vars) {
  print(
    ggplot(loan_data, aes_string(x = v)) +
      geom_bar() +
      labs(
        title = paste("Distribution of", v),
        x = v,
        y = "Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# Relationship between categorical variables and loan status
for (v in cat_vars) {
  cat("\n==============================\n")
  cat("Cross-tabulation:", v, "by loan_status_factor\n")
  tab <- table(loan_data[[v]], loan_data$loan_status_factor)
  print(tab)
  cat("\nRow percentages:\n")
  print(round(100 * prop.table(tab, margin = 1), 2))
}

# Stacked bar charts: loan status within each categorical variable
for (v in cat_vars) {
  print(
    ggplot(loan_data, aes_string(x = v, fill = "loan_status_factor")) +
      geom_bar(position = "fill") +
      labs(
        title = paste("Loan Status Proportion by", v),
        x = v,
        y = "Proportion",
        fill = "Loan status"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

################################################################################
# 6. FEATURE ENGINEERING
################################################################################

# Some financial variables can be very skewed.
# Log transformations may make relationships easier for logistic regression.

loan_data$log_person_income <- log(loan_data$person_income + 1)
loan_data$log_loan_amnt <- log(loan_data$loan_amnt + 1)

# Visualize transformed variables
ggplot(loan_data, aes(x = log_person_income)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histogram of log(person_income + 1)",
    x = "log(person_income + 1)",
    y = "Count"
  ) +
  theme_minimal()

ggplot(loan_data, aes(x = log_loan_amnt)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histogram of log(loan_amnt + 1)",
    x = "log(loan_amnt + 1)",
    y = "Count"
  ) +
  theme_minimal()

################################################################################
# 7. TRAIN/TEST SPLIT
################################################################################

# We use a stratified split so that the percentage of Approved/Rejected loans is
# approximately the same in train and test.

set.seed(123)

index_0 <- which(loan_data$loan_status_num == 0)
index_1 <- which(loan_data$loan_status_num == 1)

train_index_0 <- sample(index_0, size = floor(0.70 * length(index_0)))
train_index_1 <- sample(index_1, size = floor(0.70 * length(index_1)))

train_index <- c(train_index_0, train_index_1)

data_train <- loan_data[train_index, ]
data_test  <- loan_data[-train_index, ]

# Check sizes
nrow(data_train)
nrow(data_test)

# Check class proportions in full, train, and test data
prop.table(table(loan_data$loan_status_factor))
prop.table(table(data_train$loan_status_factor))
prop.table(table(data_test$loan_status_factor))

################################################################################
# 8. BASELINE MODEL
################################################################################

# A baseline model is a very simple rule.
# Here: always predict the most common class in the training data.

majority_class <- names(which.max(table(data_train$loan_status_factor)))
majority_class

baseline_pred <- factor(
  rep(majority_class, nrow(data_test)),
  levels = levels(data_test$loan_status_factor)
)

# Function to compute classification metrics from a confusion matrix.
# Positive class = "Approved".
classification_metrics <- function(actual, predicted, positive_class = "Approved") {
  actual <- factor(actual)
  predicted <- factor(predicted, levels = levels(actual))
  
  cm <- table(Actual = actual, Predicted = predicted)
  
  TP <- cm[positive_class, positive_class]
  FP <- sum(cm[, positive_class]) - TP
  FN <- sum(cm[positive_class, ]) - TP
  TN <- sum(cm) - TP - FP - FN
  
  accuracy <- (TP + TN) / sum(cm)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
               NA,
               2 * precision * recall / (precision + recall))
  
  results <- c(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    F1 = f1
  )
  
  return(list(confusion_matrix = cm, metrics = results))
}

baseline_results <- classification_metrics(
  actual = data_test$loan_status_factor,
  predicted = baseline_pred,
  positive_class = "Approved"
)

baseline_results$confusion_matrix
round(baseline_results$metrics, 3)

################################################################################
# 9. LOGISTIC REGRESSION MODEL
################################################################################

# Logistic regression predicts the probability of the positive class.
# In R, we use glm(..., family = binomial).

model_full <- glm(
  loan_status_num ~ person_age + person_gender + person_education +
    log_person_income + person_emp_exp + person_home_ownership +
    log_loan_amnt + loan_intent + loan_int_rate + loan_percent_income +
    cb_person_cred_hist_length + credit_score +
    previous_loan_defaults_on_file,
  data = data_train,
  family = binomial
)

summary(model_full)

# Logistic regression coefficients are on the log-odds scale.
# Exponentiating the coefficients gives odds ratios.

odds_ratios <- exp(coef(model_full))
round(odds_ratios, 3)

################################################################################
# 10. PREDICTED PROBABILITIES ON TEST DATA
################################################################################

# Predict probabilities for the test set.
test_prob <- predict(model_full, newdata = data_test, type = "response")

summary(test_prob)

# Histogram of predicted probabilities
ggplot(data.frame(prob = test_prob), aes(x = prob)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Predicted Probabilities on Test Set",
    x = "Predicted probability of approval",
    y = "Count"
  ) +
  theme_minimal()

# Compare probabilities by true status
test_plot_data <- data.frame(
  prob = test_prob,
  actual = data_test$loan_status_factor
)

ggplot(test_plot_data, aes(x = actual, y = prob)) +
  geom_boxplot() +
  labs(
    title = "Predicted Probabilities by True Loan Status",
    x = "True loan status",
    y = "Predicted probability of approval"
  ) +
  theme_minimal()

################################################################################
# 11. CLASSIFICATION USING THRESHOLD = 0.50
################################################################################

# If predicted probability >= 0.50, predict Approved.
test_pred_050 <- ifelse(test_prob >= 0.50, "Approved", "Rejected")
test_pred_050 <- factor(test_pred_050, levels = levels(data_test$loan_status_factor))

logistic_results_050 <- classification_metrics(
  actual = data_test$loan_status_factor,
  predicted = test_pred_050,
  positive_class = "Approved"
)

logistic_results_050$confusion_matrix
round(logistic_results_050$metrics, 3)

