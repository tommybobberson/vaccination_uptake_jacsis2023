# 03_transform_data

# this script transforms age_data, influenza_data, covid_data and 
# independent_variable_data to data to be used for plotting



# transform age data ------------------------------------------------------

# read age_data
age_data <- readRDS(
  here("Data", "JACSIS2023", "processed", "age_data.RDS")
  )

# create variables that represent the date of births of each child and their 
# ages relative to the time of response

age_data <- mutate(
  age_data,
  
  # calculate the DOB of each child in the form: yyyy-mm-01
  child_1_DOB = make_date(child_1_DOB_year, child_1_DOB_month),
  child_2_DOB = make_date(child_2_DOB_year, child_2_DOB_month),
  child_3_DOB = make_date(child_3_DOB_year, child_3_DOB_month),
  child_4_DOB = make_date(child_4_DOB_year, child_4_DOB_month),
  child_5_DOB = make_date(child_5_DOB_year, child_5_DOB_month),

  #convert response_time to a date
  response_time = as.Date(response_time),
  
  # calculate the age of each child at the time of response
  child_1_age = time_length(response_time - child_1_DOB, "years"),
  child_2_age = time_length(response_time - child_2_DOB, "years"),
  child_3_age = time_length(response_time - child_3_DOB, "years"),
  child_4_age = time_length(response_time - child_4_DOB, "years"),
  child_5_age = time_length(response_time - child_5_DOB, "years"),
) 

# determine the age of the child that corresponds to the
# vaccination data, age_of_interest

age_indexing <- c("child_1_age", "child_2_age", "child_3_age", "child_4_age", "child_5_age") # create a selection vector to reference the childrens' ages

# extract age_of_interest for the child whose vax status we're interested in
age_data$age_of_interest <- apply(
  age_data[,age_indexing],
  1,
  
  # function to filter out maximum ages of interest that are < 18
  function(ages) {
    ages[ages>=18] <- NA # index all ages under 18
    result <- max(ages, na.rm = TRUE) # extract maximum ages

    # check all values under 18
    if (result == -Inf) {
    return(NA) } else { # replace -Inf values with NA
    return(result)}
  }
)


# select relevant columns to transformed data that will be used for analysis
transformed_age_data <- age_data |>
  select(
    child_1_DOB,
    child_2_DOB,
    child_3_DOB,
    child_4_DOB,
    child_5_DOB,
    child_1_age,
    child_2_age,
    child_3_age,
    child_4_age,
    child_5_age,
    age_of_interest,
  )

# save the transformed age data
saveRDS(
  transformed_age_data,
  here("Data", "JACSIS2023", "processed", "transformed_age_data.RDS")
  )

# transform influenza data ------------------------------------------------

# read influenza_data
influenza_data <- readRDS(
  here("Data", "JACSIS2023", "processed", "influenza_data.RDS")
  )
# create variables that represent the dates of when the first and  
# second doses of the influenza vaccine were received
influenza_data <- mutate(
    influenza_data,
    
    # date of first dose in yyyy/mm/01
    influenza_first_dose_date = make_date( 
      influenza_first_dose_year,
      influenza_first_dose_month
      ),
    
    # date of second dose in yyyy/mm/01
    influenza_second_dose_date = make_date(
      influenza_second_dose_year,
      influenza_second_dose_month
      ),
    
    #age of child of interest (COI)
    age_of_interest = transformed_age_data$age_of_interest
  )

# influenza_coverage_dosage
# map vaccination status to the number of doses of the influenza
# vaccine received by the child in the past year
influenza_data <- influenza_data |>
  mutate(
    influenza_coverage_dosage = 
      case_when(
        
        # exclude individuals over the age threshold
        is.na(age_of_interest) ~ NA, # children > 18y/o don't have an AOI
        
        # children ineligible for the influenza vax
        age_of_interest < 0.5 ~ NA, # exclude vaccination status
        
        # children eligible for the influenza vax
        age_of_interest >= 0.5 ~ case_when( 
          influenza_first_dose >= 2 & influenza_second_dose >= 2 ~ 0, # no doses of influenza
          influenza_first_dose == 1 & influenza_second_dose >= 2 ~ 1, # 1 dose of influenza 
          influenza_second_dose == 1 ~ 2 # 2 doses of influenza
        )
      ) |>
      factor(
        levels = c(0, 1, 2)
      )
  )

# influenza_coverage_age
# map the degree of coverage of each child based on their age 
# group and doses received
influenza_data <- influenza_data |>
  mutate(
    influenza_coverage_age =
      case_when(
        
        # degree of coverage for those aged 6 months to less than 3 y/o
        age_of_interest < 3 & age_of_interest >= 0.5 ~ case_when(
          influenza_coverage_dosage == 0 ~ 0, # no coverage, 0 doses
          influenza_coverage_dosage %in% 1:2 ~ 2 # full coverage, 1 or more doses
        ),
        
        # degree of coverage for those aged 3y/o to less than 13y/o
        age_of_interest >= 3 & age_of_interest < 13 ~ case_when(
          influenza_coverage_dosage == 0 ~ 0, # no coverage, 0 doses
          influenza_coverage_dosage == 1 ~ 1, # partial coverage, 1 dose
          influenza_coverage_dosage == 2 ~ 2 # full coverage, 2 doses
        ),
        
        # degree of coverage for those aged 13 y/o to less than 18y/o
        age_of_interest >= 13 & age_of_interest < 18 ~ case_when(
          influenza_coverage_dosage == 0 ~ 0, # no coverage, 0 doses
          influenza_coverage_dosage %in% 1:2 ~ 2 # full coverage, 1 dose or more
        ),
        
        # exclude those who are aged less than 6 months who are ineligible 
        age_of_interest < 0.5 ~ NA
      ) |>
      
      # factorise results
      factor(
        levels = c(0, 1, 2)#,
        #labels = c("no coverage", "partial coverage", "full coverage")
      )
  )

# select prepared columns of data to save to transformed data set to be used for analysis
transformed_influenza_data <- influenza_data |>
  select(
    influenza_first_dose_date,
    influenza_second_dose_date,
    influenza_coverage_dosage,
    influenza_coverage_age
  )
  
# save transformed influenza data
  saveRDS(
    transformed_influenza_data, 
    here("Data", "JACSIS2023", "processed", "transformed_influenza_data.RDS")
    )


# transform covid data ----------------------------------------------------

# read covid data
covid_data <- readRDS(
  here("Data", "JACSIS2023", "processed", "covid_data.RDS")
  )

# create variables to represent the dates of when the first, second and 
# third doses of the covid vaccine were received
covid_data <- covid_data |>
  mutate(
    
    # date of first dose in yyyy/mm/01
    covid_first_dose_date = make_date( 
      covid_first_dose_year,
      covid_first_dose_month
    ),
    
    # date of second dose in yyyy/mm/01
    covid_second_dose_date = make_date( 
      covid_second_dose_year,
      covid_second_dose_month
    ),
    
    # date of third dose in yyyy/mm/01
    covid_third_dose_date = make_date( 
      covid_third_dose_year,
      covid_third_dose_month
    ),
    
    # age of child of interest
    age_of_interest = transformed_age_data$age_of_interest,
  )


# covid_coverage_dosage
# create a variable that represents the number of doses of the
# covid vaccine received by the child
covid_data <- covid_data |>
  mutate(
    covid_coverage_dosage = 
      case_when(
        # exclude children over the threshold of 18y/o
        is.na(age_of_interest) ~ NA, # children above 18 don't have an AOI 
        
        # exclude children who are ineligible for the covid vaccine
        age_of_interest < 0.5 ~ NA,
        
        # children eligible for the covid vaccine
        age_of_interest >= 0.5 ~ case_when(
            covid_first_dose >= 2 ~ 0, # no doses of the covid vax
            covid_first_dose == 1 & covid_second_dose >= 2 ~ 1, # 1 dose of the covid vax
            covid_second_dose == 1 & covid_third_dose >= 2 ~ 2, # 2 doses of the covid vax 
            covid_third_dose == 1 ~ 3 # 3 doses of the covid vax
         )
      ) |>
      factor(
        levels = c(0, 1, 2, 3)
      )
  )


# covid_coverage_age
# map the degree of coverage of each child based on their age 
# group and doses received
covid_data <- covid_data |>
  mutate(
    covid_coverage_age = case_when(
      
      # exclude children who are ineligible for the vax
      age_of_interest < 0.5 ~ NA, 
      
      # children eligible for the vax
      
      # children less than 5 years of age
      age_of_interest >= 0.5 & age_of_interest < 5 ~ case_when(
        covid_coverage_dosage %in% 1:3 ~ 2, # full coverage, use != 0 cause dosage is a factor
        covid_coverage_dosage == 0 ~ 0  # no coverage at all
      ),
      
      # children over 5 years of age
      age_of_interest >= 5 ~ case_when(
        covid_coverage_dosage == 0 ~ 0, # no coverage
        covid_coverage_dosage %in% 1:2 ~ 1, # partial coverage
        covid_coverage_dosage == 3 ~ 2 # full coverage
      )
    ) |>
      
      # factorise results
      factor(
        levels = c(0, 1, 2)#,
        # = c("no coverage", "partial coverage", "full coverage")
      )
  )


# select prepared columns of data to save as transformed data to be used for analysis
transformed_covid_data <- covid_data |>
  select(
    covid_first_dose_date,
    covid_second_dose_date,
    covid_third_dose_date,
    covid_coverage_dosage,
    covid_coverage_age
  )

# save the transformed covid data
  saveRDS(
    transformed_covid_data, 
    here("Data", "JACSIS2023", "processed", "transformed_covid_data.RDS")
    )


# transform independent variable data -------------------------------------

# read independent variable data
independent_variable_data <- readRDS(
  here("Data", "JACSIS2023", "processed", "independent_variable_data.RDS")
  )

  
# create variable(s)
  
# child_parents
  # create a variable that represents the number of parents the child has
  independent_variable_data <- 
    mutate(
      independent_variable_data,
      child_parents = spouses + 1L
    )
  
  # replace NA values with 0
  independent_variable_data$child_parents[is.na(independent_variable_data$child_parents)] <- 0L


# child_auntcles
  # create variable that represents the number of aunt and uncles the child has
  independent_variable_data$child_auntcles <- independent_variable_data$siblings # equal to the respondent's number of siblings

    
# vulnerable_individuals
  # create a variable that represents the number of vulnerable individuals in
  # the household
  independent_variable_data <- independent_variable_data |>
    mutate(
      vulnerable_individuals = need_ventilation + need_feeding + bedridden
    )

# household_total
  # create a variable that represents the total number of individuals in the 
  # household
  independent_variable_data <- independent_variable_data |>
    mutate(
      household_total = total_individuals
    )
  
# child_grand
  # create a variable that represents the number of grandparents and great 
  # grandaprents as the child
  independent_variable_data <- independent_variable_data |>
    mutate(child_grand = parents_in_law + grandparents)
  
  
# sex_of_interest
  # create a variable to index the columns denoting the sexes of each child
  sex_indexing <- 
    c("child_1_sex", "child_2_sex", "child_3_sex", "child_4_sex", "child_5_sex")

  # create a vector that contains the sexes of the children of interest, by
  # indexing it with a vector with their corresponding ages
  
  # choose the columns containing sex
  sexes <- 
    independent_variable_data[, sex_indexing]  
  
  # choose the corresponding age columns
  ages <- 
    transformed_age_data[, age_indexing] 
  
  # match location of sex of interest to location of age of interest
  sex_indexing_vector <- 
    ages == transformed_age_data$age_of_interest
  
  # assign NA values to sex values (for children of non interest)
  sexes[is.na(sex_indexing_vector) | sex_indexing_vector == FALSE] <- NA
  
  # assign the sex of the child of interest to the relevant column
  # apply function row wise
  independent_variable_data$sex_of_interest <- apply(
    sexes, 1, function(sexes) { 
      sex_of_interest <- sexes[!is.na(sexes)] # filter out the sex value 
        
      # return the sex indicator or NA if there are no eligible children
      return(
        ifelse(length(sex_of_interest) == 0, NA, sex_of_interest) 
      )
    }
  )
  
  # assign males to be 0 and females to be 1
  independent_variable_data <- independent_variable_data |>
    mutate(
      sex_of_interest = case_when(
        sex_of_interest == 1 ~ 0, # reperesent males with 0
        sex_of_interest == 2 ~ 1  # represent females with 1
      ) |>
      
        # factorise the variables
        factor()
    )


# child_of_interest
  # create a variable to indicate whether a response includes a child < 18y/o
  independent_variable_data <- independent_variable_data |>
    mutate(
      child_of_interest = case_when(
        !is.na(sex_of_interest) ~ 1, # child of interest
        is.na(sex_of_interest) ~ 0 # no child of interest
      ) |>
        factor()
    )
  
  
# child_sisters
  # number of sisters of the child of interest
  sexes <- independent_variable_data[, sex_indexing] # columns containing the sexes of the children
  
  # count the number of females in every row
  daughters <- apply(sexes, 1, function(x) {
    return(
      sum(x == 2, na.rm = TRUE) # sum the number of values == 2 
    )
  }
  )
  
  # account for the sex of the child of interest
    independent_variable_data$child_sisters <- 
      ifelse(
        independent_variable_data$sex_of_interest == 1, # if the COI is female
        daughters - 1, # remove 1 from daughter count if COI is female
        daughters # no. of sisters daughter count if COI isn't female
      )

# child_brothers
    # number of brothers of the child of interest
    sexes <- independent_variable_data[, sex_indexing] # columns containing the sexes of the children
    
    # count the number of males in every row
    sons <- apply(sexes, 1, function(x) {
      return(
        sum(x == 1, na.rm = TRUE) # sum the number of values == 1
        )
      }
    )
    
    # account for the sex of the child of interest
    independent_variable_data$child_brothers <- 
      ifelse(
        independent_variable_data$sex_of_interest == 0, # if the COI is male
        sons - 1, # remove 1 from son count if COI is male
        sons # no. of brothers = son count if COI isn't male
      )
    
# child_siblings
# the number of siblings the child has
  independent_variable_data$child_siblings <-
    independent_variable_data$child_brothers +
    independent_variable_data$child_sisters
  
  
# parents_marital_status
# parent's marital status
independent_variable_data <- independent_variable_data |> 
  mutate(
    parents_marital_status = case_when(
    child_of_interest == 0 ~ NA, # no eligibile child of interest
    marital_status %in% 1:3 ~ 0, # married
    marital_status %in% 5:7 ~ 1, # bereaved
    marital_status %in% 8:10 ~ 2, # divorced
    marital_status == 4 ~ 3 # unmarried
    ) |>
      
      # factorise variables
      factor()
  )

# test_parents_marital_status
# parent's marital status but combining everythin that isn't married tgth
independent_variable_data <- independent_variable_data |> 
  mutate(
    test_parents_marital_status = case_when(
      child_of_interest == 0 ~ NA, # no eligbile child of interest
      marital_status %in% 1:3 ~ 0, # married
      marital_status %in% 4:10 ~ 1, # unmarried
    ) |>
      
      # factorise variables
      factor()
  )

if(FALSE) {
# parents_sexual_makeup    
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_sexual_make_up = case_when(
      sexual_orientation != 2 & !is.na(sexual_orientation) ~ 0, # different sex
      sexual_orientation == 2 ~ 1, # same sex
    ) |>
      
      # factorise variables
      factor()
  )
}

# parent_1_sex
# sex of the responding parent/individual
independent_variable_data <- independent_variable_data |>
  mutate(parent_1_sex = case_when(
    respondent_sex == 1 ~ 0, # male (father) respondent
    respondent_sex == 2 ~ 1 # female (mother) respondent
    ) |>
      factor()
  )


# parent_2_sex
# sex of the other parent
independent_variable_data <-independent_variable_data |>
  mutate(
    parent_2_sex = case_when(
    
      # unmarried individuals
      parents_marital_status != 0 ~ NA, # no parent_2
      
      # married individuals
      parents_marital_status == 0 ~ 
        case_when(
          parent_1_sex == 1 ~ 0, # father as second parent
          parent_1_sex == 0 ~ 1  # mother is second parent
        )
    ) |>
      factor()
  )


# parent_1_age
# age of responding parent/individual
independent_variable_data <- independent_variable_data |>
  mutate(
    
    # parent_1_age only valid if child of interest is present
    parent_1_age = ifelse(
      child_of_interest == 1,
      respondent_age,
      NA
    )
  )
         
         


# household_income
# categorising the respondent's annual household income
independent_variable_data <- independent_variable_data |>
  mutate(
    household_income = case_when(
      household_income_annual %in% 1:12 ~ household_income_annual, # retain categories for 0 up to 10000 (thousand) yen / year
      household_income_annual %in% 13:18 ~ 13, # group annual incomes > 10000 (thousand) yen / year 
      household_income_annual %in% 19:20 ~ NA, # people who refused to or didn't know how to answer
    ) |>
      
      # factorise variables
      factor()
  )

# test_household_income
# fleshing out all categories of household income
independent_variable_data <- independent_variable_data |>
  mutate(
    test_household_income = case_when(
      household_income_annual %in% 1:18 ~ household_income_annual, # retain categories
      household_income_annual %in% 19:20 ~ NA, # people who refused to or didn't know how to answer
    ) |>
      as.factor()
  )


# parent_chronic_illness
# describes whether the parent of the child who  
# responded to the survey has chronic afflictions

independent_variable_data <- independent_variable_data |>
  mutate(
    parent_chronic_illness = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # filter out individuals who currently have a chronic illness
      if_any(respondent_hypertension:respondent_psychiatric_other, .fns = ~. %in% 3:5) == 1 ~ 2, # current chronic illness
      
      # filter out individuals who currently do not have chronic illnesses
      if_all(respondent_hypertension:respondent_psychiatric_other, .fns = ~. %in% 1:2) ~ case_when(
        
        # never had a chronic illness
        if_all(respondent_hypertension:respondent_psychiatric_other, .fns = ~. == 1) ~ 0, 
        
        # have had ANY chronic illness in the past
        if_any(respondent_hypertension:respondent_psychiatric_other, .fns = ~. == 2) ~ 1 
      )
    ) |>
      
      # factorise parent_chronic_illness
      factor()
  )


# father_employment_status
# status of father's employment
# considering only heterosexual couples
independent_variable_data <- independent_variable_data |>
  mutate(
    father_employment_status = case_when(
    
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # When the respondent is the father
      parent_1_sex == 0 ~ case_when(
        
        # respondent's employment status is father's employment status
        respondent_employment_status %in% c(13:16) ~ 0, # unemployed
        respondent_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        respondent_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        respondent_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      ),
      
      # When the partner is the father
      parent_2_sex == 0 ~ case_when(
        
        # partner's employment status is father's employment status
        partner_employment_status %in% c(13:16) ~ 0, # unemployed
        partner_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        partner_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        partner_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      )
    ) |>
      
      factor()
  )



# mother_employment_status
# status of mother's employment
# considering only heterosexual couples
independent_variable_data <- independent_variable_data |>
  mutate(
    mother_employment_status = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # When respondent is the mother
      parent_1_sex == 1 ~ case_when(
        
        # respondent's employment status is mother's employment status
        respondent_employment_status %in% c(13:16) ~ 0, # unemployed
        respondent_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        respondent_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        respondent_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      ),
      
      # When the partner is the mother
      parent_2_sex ==  1 ~ case_when(
        
        # partner's employment status is mother's employment status
        partner_employment_status %in% c(13:16) ~ 0, # unemployed
        partner_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        partner_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        partner_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      )
    ) |>
    factor()
  )


# parents_student_status
# whether the mother or father is currently a student
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_student_status = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # none are students
      !(respondent_employment_status %in% c(12:13)) & 
      !(partner_employment_status %in% c(12:13)) ~ 0,
      
      # both are students
      respondent_employment_status & partner_employment_status %in% 12:13 ~ 3,
      
      # father is a student
      parent_1_sex == 0 & respondent_employment_status %in% 12:13 ~ 1,
      parent_2_sex == 0 & partner_employment_status %in% 12:13 ~ 1,
      
      # mother is a student
      parent_1_sex == 1 & respondent_employment_status %in% 12:13 ~ 2,
      parent_2_sex == 1 & partner_employment_status %in% 12:13 ~ 2,
    ) |>
      factor()
  )


# parents_retired_status
# indicates which of the parents are retired
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_retired_status = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # neither are retired
      # only true if both parents aren't retired
      respondent_employment_status != 14 & partner_employment_status != 14 ~ 0,
      
      # both retired
      respondent_employment_status == 14 & partner_employment_status == 14 ~ 3,
      
      # father retired
      parent_1_sex == 0 & respondent_employment_status == 14 ~ 1,
      parent_2_sex == 0 & partner_employment_status == 14 ~ 1,
      
      # mother retired
      parent_1_sex == 1 & respondent_employment_status == 14 ~ 2,
      parent_2_sex == 1 & partner_employment_status == 14 ~ 2,
    ) |>
      factor()
  )


# parents_stay_home
# indicates which of the child of interest's parents are stay home parents
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_stay_home = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # neither are stay home parents
      respondent_employment_status != 15 & partner_employment_status !=15 ~ 0,
      
      # both parents are stay home parents
      respondent_employment_status == 15 & partner_employment_status ==15 ~ 3,
      
      # only father is the stay home parent
      parent_1_sex == 0 & respondent_employment_status == 15 ~ 1,
      parent_2_sex == 0 & partner_employment_status == 15 ~ 1,
      
      # only mother is the stay home parent
      parent_1_sex == 1 & respondent_employment_status == 15 ~ 2,
      parent_2_sex == 1 & partner_employment_status == 15 ~ 2,
    ) |>
      factor()
  )

# mother_stay_home
# indicates whether the child's mother is a stay home parent
independent_variable_data <- independent_variable_data |>
  mutate(
    mother_stay_home = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
  # mother doesn't stay home
      # responding parent is the mother
      parent_1_sex == 1 & respondent_employment_status != 15 ~ 0,
      
      # partner is the mother
      parent_2_sex == 1 & partner_employment_status != 15 ~ 0,
      
  # mother stays home
      # responding parent is the mother
      parent_1_sex == 1 & respondent_employment_status == 15 ~ 1,
      
      # partner is the mother
      parent_2_sex == 1 & partner_employment_status == 15 ~ 1
    ) |>
    factor()
  )


# parents_healthcare
# indicates whether the the child's mother or father works in the
# healthcare sector

independent_variable_data <- independent_variable_data |>
  mutate(
    parents_healthcare = case_when(
      
    # ignore cases where there are no children of interest
    child_of_interest == 0 ~ NA,
      
    # seperate individuals who work in healthcare and non-healthcare sectors
    respondent_industry_of_work %in% c(1:14, 17:20) ~ 0, # non-healthcare
    respondent_industry_of_work %in% 15:16 & parent_1_sex == 0 ~ 1, # responding father in HC
    respondent_industry_of_work %in% 15:16 & parent_1_sex == 1 ~ 2, # responding mother in HC
    ) |>
      factor()
  )


# father_highest_education
# a variable that describes the highest educational attainment of a COI's father
independent_variable_data <- independent_variable_data |>
  mutate(
    father_highest_education = ifelse(
      
      # valid status must be confirmed by the presence of COI
      child_of_interest == 1,
      case_when(
        
  
      # father is respondent
      parent_1_sex == 0 ~ case_when(
        highest_education == 1 ~ 0, # junior high qualification
        highest_education %in% 2:3 ~ 1, # high school qualification
        highest_education %in% 4:8 ~ 2, # tertiary qualification
        highest_education == 9 ~ 3, # post-graduate qualification
        highest_education %in% 10:11 ~ NA # no response 
      ),
      
      # father is respondent's partner
      parent_2_sex == 0 ~ case_when(
        highest_education_partner == 1 ~ 0, # junior high qualification
        highest_education_partner %in% 2:3 ~ 1, # high school qualification
        highest_education_partner %in% 4:8 ~ 2, # tertiary qualification
        highest_education_partner == 9 ~ 3, # post-graduate qualification
        highest_education_partner %in% 10:11 ~ NA # no response 
      )
    ),
    NA
    ) |>
    factor(
      levels = c(0, 1, 2, 3)#,
      #labels = c("Junior High", "Highsch", "Tertiary", "Postgrad")
    )
  )


# mother_highest_education
# a variable that describes the highest educational attainment of a COI's mother
independent_variable_data <- independent_variable_data |>
  mutate(
    mother_highest_education = ifelse(
      child_of_interest == 1,
      case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      # mother is respondent
      parent_1_sex == 1 ~ case_when(
        highest_education == 1 ~ 0, # junior high qualification
        highest_education %in% 2:3 ~ 1, # high school qualification
        highest_education %in% 4:8 ~ 2, # tertiary qualification
        highest_education == 9 ~ 3, # post-graduate qualification
        highest_education %in% 10:11 ~ NA # no response 
      ),
      
      #  mother is respondent's partner
      parent_2_sex == 1 ~ case_when(
        highest_education_partner == 1 ~ 0, # junior high qualification
        highest_education_partner %in% 2:3 ~ 1, # high school qualification
        highest_education_partner %in% 4:8 ~ 2, # tertiary qualification
        highest_education_partner == 9 ~ 3, # post-graduate qualification
        highest_education_partner %in% 10:11 ~ NA # no response 
      )
    ),
    NA
    ) |>
      factor(
        levels = c(0, 1, 2, 3)#,
        #labels = c("Junior High", "Highsch", "Tertiary", "Postgrad")
      )
  )


# parents_highest_education
# a variable that describes the highest educational attainment between parent(s)

# extract a dataframe that contains the educational statuses
# of the responding parent and their partner (if applicable)
edu <- independent_variable_data |>
  select(c("mother_highest_education", "father_highest_education"))

# compare educational attainments and extract the highest one
independent_variable_data$edu <- 
  apply(edu, 1, max, na.rm = TRUE)

# assign the highest educational attainment achieved by both parents combined
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_highest_education = ifelse(
      child_of_interest == 1 & edu != -Inf,
      edu,
      NA
    ) |>
      factor()
      
    # drop edu - but how not to drop child_of_interest either???
    #.keep = "unused"
  )

# father_highest_education
# a variable that describes the highest educational attainment of a COI's father
independent_variable_data <- independent_variable_data |>
  mutate(
    father_highest_education = ifelse(
      
      # valid status must be confirmed by the presence of COI
      child_of_interest == 1,
      case_when(
        
        
        # father is respondent
        parent_1_sex == 0 ~ case_when(
          highest_education == 1 ~ 0, # junior high qualification
          highest_education %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:8 ~ 2, # tertiary qualification
          highest_education == 9 ~ 3, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        ),
        
        # father is respondent's partner
        parent_2_sex == 0 ~ case_when(
          highest_education_partner == 1 ~ 0, # junior high qualification
          highest_education_partner %in% 2:3 ~ 1, # high school qualification
          highest_education_partner %in% 4:8 ~ 2, # tertiary qualification
          highest_education_partner == 9 ~ 3, # post-graduate qualification
          highest_education_partner %in% 10:11 ~ NA # no response 
        )
      ),
      NA
    ) |>
      factor(
        levels = c(0, 1, 2, 3)#,
        #labels = c("Junior High", "Highsch", "Tertiary", "Postgrad")
      )
  )


# mother_highest_education
# a variable that describes the highest educational attainment of a COI's mother
independent_variable_data <- independent_variable_data |>
  mutate(
    mother_highest_education = ifelse(
      child_of_interest == 1,
      case_when(
        
        # ignore cases where there are no children of interest
        child_of_interest == 0 ~ NA,
        
        # mother is respondent
        parent_1_sex == 1 ~ case_when(
          highest_education == 1 ~ 0, # junior high qualification
          highest_education %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:8 ~ 2, # tertiary qualification
          highest_education == 9 ~ 3, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        ),
        
        #  mother is respondent's partner
        parent_2_sex == 1 ~ case_when(
          highest_education_partner == 1 ~ 0, # junior high qualification
          highest_education_partner %in% 2:3 ~ 1, # high school qualification
          highest_education_partner %in% 4:8 ~ 2, # tertiary qualification
          highest_education_partner == 9 ~ 3, # post-graduate qualification
          highest_education_partner %in% 10:11 ~ NA # no response 
        )
      ),
      NA
    ) |>
      factor(
        levels = c(0, 1, 2, 3)#,
        #labels = c("Junior High", "Highsch", "Tertiary", "Postgrad")
      )
  )

# test_father_highest_education
# a variable that describes the highest educational attainment of a COI's father
independent_variable_data <- independent_variable_data |>
  mutate(
    test_father_highest_education = ifelse(
      
      # valid status must be confirmed by the presence of COI
      child_of_interest == 1,
      case_when(
        
        
        # father is respondent
        parent_1_sex == 0 ~ case_when(
          highest_education == 1 ~ 0, # junior high qualification
          highest_education %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:5 ~ 2, # junior college/technical colleges
          highest_education %in% 6:8 ~ 3, # tertiary qualification
          highest_education == 9 ~ 4, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        ),
        
        # father is respondent's partner
        parent_2_sex == 0 ~ case_when(
          highest_education_partner == 1 ~ 0, # junior high qualification
          highest_education_partner %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:5 ~ 2, # junior college/technical colleges
          highest_education %in% 6:8 ~ 3, # tertiary qualification
          highest_education == 9 ~ 4, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        )
      ),
      NA
    ) |>
      factor(
        levels = c(0, 1, 2, 3, 4),
        labels = c("Junior High", "Highsch", "Junior/Technical College", "Tertiary", "Postgrad")
      )
  )


# test_mother_highest_education
# a variable that describes the highest educational attainment of a COI's mother
independent_variable_data <- independent_variable_data |>
  mutate(
    test_mother_highest_education = ifelse(
      child_of_interest == 1,
      case_when(
        
        # ignore cases where there are no children of interest
        child_of_interest == 0 ~ NA,
        
        # mother is respondent
        parent_1_sex == 1 ~ case_when(
          highest_education == 1 ~ 0, # junior high qualification
          highest_education %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:5 ~ 2, # junior college/technical colleges
          highest_education %in% 6:8 ~ 3, # tertiary qualification
          highest_education == 9 ~ 4, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        ),
        
        #  mother is respondent's partner
        parent_2_sex == 1 ~ case_when(
          highest_education_partner == 1 ~ 0, # junior high qualification
          highest_education_partner %in% 2:3 ~ 1, # high school qualification
          highest_education %in% 4:5 ~ 2, # junior college/technical colleges
          highest_education %in% 6:8 ~ 3, # tertiary qualification
          highest_education == 9 ~ 4, # post-graduate qualification
          highest_education %in% 10:11 ~ NA # no response 
        )
      ),
      NA
    ) |>
      factor(
        levels = c(0, 1, 2, 3, 4),
        labels = c("Junior High", "Highsch", "Junior/Technical College", "Tertiary", "Postgrad")
      )
  )

# parent_influenza
# variable that indicates whether the child's (responding parent)
# has received the flu vaccine in the past year
independent_variable_data <- independent_variable_data |>
  mutate(
      parent_influenza = case_when(
        
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
        
      influenza_vaccination_respondent == 2 ~ 0, # has not received the influenza vaccine in the past year
      influenza_vaccination_respondent == 1 ~ 1 # received the influenza vacicine in the past year
    ) |>
      
      # factorise parent's influenza vaccination status
      factor()
  )


# parent_covid_doses
# the numerical number of doses of the responding parent
independent_variable_data <- independent_variable_data |>
  mutate(
    
    parent_covid_doses = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      covid_vaccination_respondent %in% 8:10 ~ 0, # no doses
      covid_vaccination_respondent == 6 ~ 1, # 1 dose
      covid_vaccination_respondent == 5 ~ 2, # 2 doses
      covid_vaccination_respondent == 4 ~ 3, # 3 doss
      covid_vaccination_respondent == 3 ~ 4, # 4 doses
      covid_vaccination_respondent == 2 ~ 5, # 5 doses
      covid_vaccination_respondent == 1 ~ 6, # 6 doses
      covid_vaccination_respondent == 7 ~ 7 # ineligible
    )
  )

# parent_covid_coverage
# degree of coverage of parent and their reasons for not wanting to be vaccinated 
independent_variable_data <- independent_variable_data |>
  mutate(
    parent_covid_coverage = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      covid_vaccination_respondent == 7 ~ 0, # ineligible
      covid_vaccination_respondent %in% 5:6 ~ 1, # partially vaxxed
      covid_vaccination_respondent %in% 1:4 ~ 2, # fully vaccinated
      covid_vaccination_respondent == 8 ~ 3, # unsure
      covid_vaccination_respondent == 9 ~ 4, # feel no need
      covid_vaccination_respondent == 10 ~ 5 # don't want to get vaccinated
      ) |>
        
        # factorise parent's covid coverage
        factor()
  )
  

# parents_time_since_covvax
# time period within which the respondent parent receieved their last covid vaccination
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_time_since_covvax = case_when(
      
      # ignore cases where there are no children of interest
      child_of_interest == 0 ~ NA,
      
      covid_vaccination_within1y == 2 &
      covid_vaccination_within6m == 2 ~ 0, # more than a year ago
      
      covid_vaccination_within1y == 1 &
      covid_vaccination_within6m == 2 ~ 1, # within a year
      
      covid_vaccination_within6m == 1 ~ 2 # within 6m
    ) |>
      
      # factorise time since parents' last covid vaccine
      factor()
  )


# select all columns that have been derived and prepared for use in analysis
# to transformed data
transformed_independent_variable_data <- independent_variable_data |>
  select(
    respondent_hypertension,
    respondent_diabetes,
    respondent_dyslipidemia,
    respondent_pneumonia,
    respondent_asthma,
    respondent_atopic_dermatitis,
    respondent_allergic_rhinitis,
    respondent_periodontitis,
    respondent_caries,
    respondent_cataract,
    respondent_angina,
    respondent_stroke,
    respondent_COPD,
    respondent_kidney_disease,
    respondent_hepatitis_cirrhosis,
    respondent_immune_abnormalities,
    respondent_cancer,
    respondent_chronic_pain,
    respondent_depression,
    respondent_psychiatric_other,
    respondent_employment_status,
    respondent_industry_of_work,
    perception_safety,
    perception_infection_danger,
    perception_immunisation_importance,
    perception_immunisation_benefits,
    perception_collective_immunisation,
    perception_immunisation_sanctions,
    perception_immunisation_adverse,
    perception_immunisation_herd,
    child_parents,
    child_auntcles,
    vulnerable_individuals,
    household_total,
    child_grand,
    sex_of_interest,
    child_of_interest,
    child_sisters,
    child_brothers,
    child_siblings,
    parents_marital_status,
    test_parents_marital_status,
    parent_1_sex,
    parent_2_sex,
    parent_1_age,
    household_income,
    test_household_income,
    parent_chronic_illness,
    father_employment_status,
    mother_employment_status,
    parents_student_status,
    parents_retired_status,
    parents_stay_home,
    mother_stay_home,
    parents_healthcare,
    father_highest_education,
    mother_highest_education,
    test_father_highest_education,
    test_mother_highest_education,
    parents_highest_education,
    parent_influenza,
    parent_covid_doses,
    parent_covid_coverage,
    parents_time_since_covvax
  )

# save the transformed independent variable data
saveRDS(
  transformed_independent_variable_data, 
  here(
    "Data", 
    "JACSIS2023", 
    "processed", 
    "transformed_independent_variable_data.RDS"
    )
  )



 #