R code for B and C:
  # Load necessary libraries
  library(readr)
library(MASS)   # For probit regression
library(censReg)  # For tobit regression

# Load the data
file_path <- "NSSO68.csv"
df <- read_csv(“/Users/poornima/Documents/SCMA”)

# Display the first few rows of the dataset
head(df)

# Define the probit regression model
# Assuming 'non_vegetarian' is the binary dependent variable
probit_model <- glm(non_vegetarian ~ age + gender + education, family = binomial(link = "probit"), data = df)
summary(probit_model)

# Define the tobit regression model
# Assuming 'non_vegetarian' is the dependent variable
tobit_model <- censReg(non_vegetarian ~ age + gender + education, left = 0, right = 1, data = df)
summary(tobit_model)
