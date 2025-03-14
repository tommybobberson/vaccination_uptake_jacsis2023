# 04_01_EDA_Univariate

# read
# transformed independent variable data
transformed_ivd <-
  readRDS(
    here(
      "Data",
      "JACSIS2023", 
      "processed", 
      "transformed_independent_variable_data.RDS"
      )
    )

# transformed covid data
transformed_cov <-
  readRDS(
    here(
      "Data",
      "JACSIS2023", 
      "processed", 
      "transformed_covid_data.RDS"
    )
  )

# transformed influenza data
transformed_inf <-
  readRDS(
    here(
      "Data",
      "JACSIS2023", 
      "processed", 
      "transformed_influenza_data.RDS"
    )
  )
  
# tabular counts
# influenza data
  
  # degree of influenza coverage by age
  transformed_inf |>
    filter(!is.na(influenza_coverage_age)) |>
    summarise(individuals = n(), .by = influenza_coverage_age) |>
    mutate(proportion = individuals / sum(individuals)) |>
    arrange(influenza_coverage_age) |>
    mutate(
      influenza_coverage_age = case_when(
        influenza_coverage_age == 0 ~ "Not vaxxed",
        influenza_coverage_age == 1 ~ "Partially vaxxed",
        influenza_coverage_age == 2 ~ "Fully vaxxed"
      )
    ) |>
    ggplot(aes(x = influenza_coverage_age, y = proportion)) + geom_col() +
    geom_text(aes(label = individuals), vjust = 1.0) +
    ggtitle(
      paste(
        "Vaccination status of eligible children, N =",
        sum(!is.na(transformed_inf$influenza_coverage_age))
      )
    )
  
  # degree of influenza coverage by doses
  transformed_inf |>
    filter(!is.na(influenza_coverage_dosage)) |>
    summarise(individuals = n(), .by = influenza_coverage_dosage) |>
    mutate(proportion = individuals / sum(individuals)) |>
    arrange(influenza_coverage_dosage)

  
# covid data
  
  # degree of covid coverage by age
  transformed_cov |>
    filter(!is.na(covid_coverage_age)) |>
    summarise(individuals = n(), .by = covid_coverage_age) |>
    mutate(proportion = individuals / sum(individuals)) |>
    arrange(covid_coverage_age) |>
    mutate(
      covid_coverage_age = case_when(
        covid_coverage_age == 0 ~ "Not vaxxed",
        covid_coverage_age == 1 ~ "Partially vaxxed",
        covid_coverage_age == 2 ~ "Fully vaxxed"
      )
    )
  
  # degree of covid coverage by doses
  transformed_cov |>
    filter(!is.na(covid_coverage_dosage)) |>
    summarise(individuals = n(), .by = covid_coverage_dosage) |>
    mutate(proportion = individuals / sum(individuals)) |>
    arrange(covid_coverage_dosage) 

  
# independent variable data
# paired plots
  

