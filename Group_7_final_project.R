# Load required libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(corrplot)
library(randomForest)
library(reshape2)
library(stringr)

# Load the dataset (your path)
setwd("C:/Users/yuzha/Desktop/")
data <- read.csv("Dry_Eye_Dataset.csv")


# Split Blood Pressure into systolic and diastolic
bp <- str_split_fixed(data$Blood.pressure, "/", 2)
data$Systolic <- as.numeric(bp[, 1])
data$Diastolic <- as.numeric(bp[, 2])

# Encode categorical variables as numeric (0/1)
data_clean <- data %>%
  mutate(across(c(Gender, Sleep.disorder, Wake.up.during.night,
                  Caffeine.consumption, Alcohol.consumption, Smoking,
                  Medical.issue, Ongoing.medication,
                  Smart.device.before.bed, Blue.light.filter,
                  Discomfort.Eye.strain, Redness.in.eye,
                  Itchiness.Irritation.in.eye, Dry.Eye.Disease),
                ~ ifelse(. == "Y" | . == "M", 1, 0))) %>%
  rename_with(~ gsub("\\.", "_", .))  # Rename columns for consistency

# Remove original Blood Pressure column
data_clean$Blood_pressure <- NULL

# Convert types
data_clean <- data_clean %>%
  mutate(Gender = as.factor(Gender),
         Dry_Eye_Disease = as.factor(Dry_Eye_Disease))

# Normalize continuous variables
norm_vars <- c("Age", "Sleep_duration", "Sleep_quality", "Stress_level",
               "Heart_rate", "Daily_steps", "Physical_activity",
               "Height", "Weight", "Average_screen_time",
               "Systolic", "Diastolic")

data_norm <- data_clean
data_norm[norm_vars] <- scale(data_clean[norm_vars])

# Train-test split
set.seed(123)
train_index <- createDataPartition(data_norm$Dry_Eye_Disease, p = 0.7, list = FALSE)
train_data <- data_norm[train_index, ]
test_data <- data_norm[-train_index, ]

# ------------------ Logistic Regression ------------------
log_model <- glm(Dry_Eye_Disease ~ ., data = train_data, family = binomial)
log_pred <- predict(log_model, newdata = test_data, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)

# ------------------ Decision Tree ------------------
tree_model <- rpart(Dry_Eye_Disease ~ ., data = train_data, method = "class")
tree_pred <- predict(tree_model, newdata = test_data, type = "class")

# ------------------ Random Forest ------------------
rf_model <- randomForest(Dry_Eye_Disease ~ ., data = train_data, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, newdata = test_data, type = "response")
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# ------------------ Evaluation Function ------------------
eval_metrics <- function(true, pred) {
  cm <- confusionMatrix(as.factor(pred), as.factor(true), positive = "1")
  return(data.frame(Accuracy = cm$overall["Accuracy"],
                    Precision = cm$byClass["Precision"],
                    Recall = cm$byClass["Recall"],
                    F1 = cm$byClass["F1"]))
}

# ------------------ Model Results ------------------
log_res <- eval_metrics(test_data$Dry_Eye_Disease, log_pred_class)
tree_res <- eval_metrics(test_data$Dry_Eye_Disease, tree_pred)
rf_res <- eval_metrics(test_data$Dry_Eye_Disease, rf_pred)

model_results <- bind_rows(log_res, rf_res, tree_res)
model_results$Model <- c("Logistic Regression", "Random Forest", "Decision Tree")
print(model_results)

# ------------------ ROC Curves ------------------
log_roc <- roc(test_data$Dry_Eye_Disease, as.numeric(log_pred))
tree_roc <- roc(test_data$Dry_Eye_Disease, as.numeric(tree_pred))
rf_roc <- roc(test_data$Dry_Eye_Disease, rf_prob)

plot(log_roc, col = "blue", main = "ROC Curves")
plot(tree_roc, col = "red", add = TRUE)
plot(rf_roc, col = "green", add = TRUE)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest"),
       col = c("blue", "red", "green"), lwd = 2)

# ------------------ Symptom vs DED Visualization ------------------
symptoms <- c("Discomfort_Eye_strain", "Redness_in_eye", "Itchiness_Irritation_in_eye")

symptom_df <- data_clean %>%
  pivot_longer(cols = all_of(symptoms), names_to = "Symptom", values_to = "Present") %>%
  group_by(Symptom, Present) %>%
  summarize(DED_Rate = mean(as.numeric(as.character(Dry_Eye_Disease)))) %>%
  mutate(Symptom = gsub("_", " ", Symptom))

ggplot(symptom_df, aes(x = Symptom, y = DED_Rate, fill = factor(Present))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "DED Prevalence by Eye Symptoms", y = "DED Proportion", fill = "Symptom Present") +
  theme_minimal()

# ------------------ Correlation Heatmap ------------------
corr_matrix <- cor(data_norm %>% select(where(is.numeric)))
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.6)


# ------------------ Model Comparison Bar Plot ------------------
library(ggplot2)

# Add model names to the results dataframe
model_results$Model <- c("Logistic Regression", "Random Forest", "Decision Tree")

# Convert to long format for ggplot
model_long <- reshape2::melt(model_results, id.vars = "Model")

# Plot
ggplot(model_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison",
       y = "Score", x = "Model", fill = "Metric") +
  ylim(0, 1) +
  theme_minimal()

