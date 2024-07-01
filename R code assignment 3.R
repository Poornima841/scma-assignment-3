R code A

library(readr)
library(dplyr)
library(caret)
library(rpart)
library(pROC)
library(ggplot2)

# Set working directory
setwd("/Users/poornima/Desktop")

# Load the data
df <- read_csv('diagnosis.csv')

# Display the first few rows of the dataset
head(df)

# Drop diagnosis column
df <- df %>% select(-diagnosis)

# Impute missing values with mean
dfdiagnosis <- ifelse(is.na(df$Diagnosis), mean(df$Diagnosis, na.rm = TRUE), df$Diagnosis)
df$Diagnosis <- ifelse(is.na(df$Diagnosis), mean(df$Diagnosis, na.rm = TRUE), df$diagnosis)
df$Credit_History <- ifelse(is.na(df$Credit_History), mean(df$Credit_History, na.rm = TRUE), df$Credit_History)

# Impute missing categorical values with most frequent
df$Gender <- ifelse(is.na(df$Gender), names(which.max(table(df$Gender))), df$Gender)
df$Married <- ifelse(is.na(df$Married), names(which.max(table(df$Married))), df$Married)
df$Dependents <- ifelse(is.na(df$Dependents), names(which.max(table(df$Dependents))), df$Dependents)
df$Self_Employed <- ifelse(is.na(df$Self_Employed), names(which.max(table(df$Self_Employed))), df$Self_Employed)

# Convert Dependents column to numeric
df$Dependents <- as.numeric(gsub("\\+", "", df$Dependents))

# Encode categorical variables
df$Gender <- as.numeric(factor(df$Gender))
df$Married <- as.numeric(factor(df$Married))
df$Education <- as.numeric(factor(df$Education))
df$Self_Employed <- as.numeric(factor(df$Self_Employed))
df$Property_Area <- as.numeric(factor(df$Property_Area))
df$Diagnosis <- as.numeric(factor(df$Diagnosis))

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df$Diagnosis, p = 0.7, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

X_train <- subset(train_data, select = -c(Diagnosis))
y_train <- train_data$Diagnosis
X_test <- subset(test_data, select = -c(Diagnosis))
y_test <- test_data$Diagnosis

# Fit logistic regression model
log_reg <- glm(Diagnosis ~ ., family = binomial(link = "logit"), data = train_data)

# Predict on test set
y_pred <- predict(log_reg, newdata = test_data, type = "response")
y_pred_proba <- y_pred

# Calculate ROC curve and AUC
roc_obj <- roc(test_data$Diagnosis, y_pred_proba)
roc_auc <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, col = "blue", main = "ROC Curve", legacy.axes = TRUE)
lines(x = c(0,1), y = c(0,1), col = "red", lty = 2)
legend("bottomright", legend = paste("AUC =", round(roc_auc, 2)), col = "blue", lty = 1, cex = 0.8)













