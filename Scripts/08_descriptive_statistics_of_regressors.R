# We will create a table of the following descriptive statistics
# 1. Overview: i.e. proportion of children for overall vaccination status
# 2. Proportional table of each independent variable BY vaccination stat 

# scripts
library(here)
library(gtsummary)

# data
data <- readRDS(
  here("Data", "JACSIS2023", "processed", "reshaped_combined_data.RDS")
)

# need to show NAs too? 
# 1. Overview proportional table
data |>
  tbl_summary(
    include = c(covid_coverage_age, covid_coverage_dosage, influenza_coverage_age, influenza_coverage_dosage)
    )


# 2. proportional table
# Categorical: Chi squared
# Numerical: univariate Logistic regression (check if the slope is significant)

# the columns which we'd like to describe, this takes reference from 06_reshape_data
variables_to_describe <- colnames(data)

# table for covid_coverage_age
data |>
  tbl_summary(
    by = c(covid_coverage_age)
  )

# table for influenza_coverage_age
