## 06_reshape data

## This script serves to reshape our dataset, drop variables which we've deemed 
## redundant from our EDA, and create dummy variables for for all the multi-level
## categorical variables

# Making a master data frame
  # transformed independent variable data
  ivd <-
    readRDS(
      here(
        "Data",
        "JACSIS2023", 
        "processed", 
        "transformed_independent_variable_data.RDS"
      )
    )
  
  # transformed covid data
  cov <-
    readRDS(
      here(
        "Data",
        "JACSIS2023", 
        "processed", 
        "transformed_covid_data.RDS"
      )
    )
  
  # transformed influenza data
  inf <-
    readRDS(
      here(
        "Data",
        "JACSIS2023", 
        "processed", 
        "transformed_influenza_data.RDS"
      )
    )
  
  # transformed age data
  age <- 
    readRDS(
      here(
        "Data",
        "JACSIS2023", 
        "processed", 
        "transformed_age_data.RDS"
      )
    )
  
  # Combining all the data
  data <- bind_cols(age, inf, cov, ivd) |>
    select(-age_of_interest...60, -birth_order...58) |>
    rename( birth_order = birth_order...12, age_of_interest = age_of_interest...11)
  
# data exclusion
  # choosing variables
  data <- data |>
    select(
      child_1_age,
      child_2_age,
      child_3_age,
      child_4_age,
      child_5_age,
      age_of_interest,
      birth_order,
      covid_coverage_dosage,
      covid_coverage_age,
      influenza_coverage_dosage,
      influenza_coverage_age,
      child_parents,
      household_total,
      child_grand,
      sex_of_interest,
      child_of_interest,
      age_of_interest,
      age_cat,
      child_siblings,
      siblings_over_18,
      test_parents_marital_status,
      parent_1_sex,
      parent_2_sex,
      parent_1_age,
      test_household_income,
      parent_chronic_illness,
      father_employment_status,
      mother_employment_status,
      parents_healthcare,
      father_highest_education,
      mother_highest_education,
      parents_highest_education,
      starts_with("perception_")
    )
  
  # choosing valid cases
  data <- data |>
    filter(child_of_interest == 1 & !is.na(covid_coverage_age) & !is.na(influenza_coverage_age))
  
# creating dummy variables for all variables
data <- dummy_cols(data)

# save the reshaped data frame
saveRDS(
  data,
  here("Data", "JACSIS2023", "processed", "reshaped_combined_data.RDS")
)