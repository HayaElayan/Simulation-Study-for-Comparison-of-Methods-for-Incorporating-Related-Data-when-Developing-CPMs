# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Function to generate synthetic dataset
generate_data <- function(n) {
  data <- tibble(
    outcome = sample(0:1, n, replace = TRUE),
    Age = sample(40:90, n, replace = TRUE),
    Sex = sample(c("Male", "Female"), n, replace = TRUE),
    AF_atrial_flutter = sample(c("Yes", "No"), n, replace = TRUE),
    Diabetes = sample(c("Yes", "No"), n, replace = TRUE),
    BMI = round(rnorm(n, mean = 27, sd = 4), 8),
    eGFR = round(rnorm(n, mean = 85, sd = 15), 8),
    LVEF35 = sample(0:1, n, replace = TRUE)
  )
  return(data)
}

# Generate datasets
imputed_data_1 <- generate_data(600)
imputed_data_all_centres_except_1 <- generate_data(2300)

# Save to CSV
write.csv(imputed_data_1, "imputed_data_LUND.csv", row.names = FALSE)
write.csv(imputed_data_all_centres_except_1, "imputed_data_all_centres_except_LUND.csv", row.names = FALSE)

# Print first few rows to verify
head(imputed_data_1)
head(imputed_data_all_centres_except_1)
