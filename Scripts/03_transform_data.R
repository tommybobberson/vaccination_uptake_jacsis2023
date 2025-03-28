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


# select relevant columns for data analysis
  # read RAW CLEANED age variables
  age_variables <- readRDS(
    here("Data", "JACSIS2023", "processed", "age_data.RDS")
  )
  
  # create a vector to index redundant columns
  to_remove_age <- colnames(age_variables)
  
  # remove redundant columns
  transformed_age_data <- age_data |>
    select(!all_of(to_remove_age))

  
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
      )
  )

# influenza_coverage_age
# map the degree of coverage of each child based on their age 
# group and doses received
influenza_data <- influenza_data |>
  mutate(
    influenza_coverage_age =
      case_when(
        
        # degree of coverage for those aged 6 months to less than 13y/o
        age_of_interest < 13 & age_of_interest >= 0.5 ~ case_when(
          influenza_coverage_dosage == 0 ~ 0, # no coverage, 0 doses
          influenza_coverage_dosage %in% 1:2 ~ 2 # full coverage, 1 dose
        ),
        
        # degree of coverage for those aged 13y/o to 18y/o
        age_of_interest >= 13 & age_of_interest <= 18 ~ case_when(
          influenza_coverage_dosage == 0 ~ 0, # no coverage, 0 doses
          influenza_coverage_dosage == 2 ~ 1, # partial coverage, 1 dose
          influenza_coverage_dosage == 2 ~ 2 # full coverage, 2 doses
        ),
        
        # exclude those who are aged less than 6 months who are ineligible 
        age_of_interest < 0.5 ~ NA
      ) |>
      
      # factorise results
      factor()
  )

# select prepared columns of data for analysis
  # read RAW CLEANED influenza variables
  influenza_variables <- readRDS(
    here("Data", "JACSIS2023", "processed", "influenza_data.RDS")
  )

  # create a vector to index RAW CLEANED influenza data
  to_remove_influenza <- colnames(influenza_variables)
  
  # remove redundant data columns
  transformed_influenza_data <- influenza_data |>
    select(!all_of(to_remove_influenza))

  
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
        covid_coverage_dosage >= 0 ~ 2, # full coverage
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
      factor()
  )


# select prepared columns for data analysis
  # read RAW CLEANED covid variables
  covid_variables <- readRDS(
    here("Data", "JACSIS2023", "processed", "covid_data.RDS")
  )
  
  # create a vector to index RAW CLEANED covid data
  to_remove_covid <- colnames(covid_variables)
  
  # remove redundant columns
  transformed_covid_data <- covid_data |>
    select(!all_of(to_remove_covid))

  
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
      child_parents = spouses + 1
    )
  
  # replace NA values with 0
  independent_variable_data$child_parents[is.na(independent_variable_data$child_parents)] <- 0


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
  
  sexes <- independent_variable_data[, sex_indexing] # choose the columns containing sex 
  ages <- transformed_age_data[, age_indexing] # choose the corresponding age columns
  sex_indexing_vector <- ages == transformed_age_data$age_of_interest # match location of sex of interest to location of age of interest
  sexes[is.na(sex_indexing_vector) | sex_indexing_vector == FALSE] <- NA # assign NA values to sex values (for children of non interest)
  
  # assign the sex of the child of interest to the relevant column
  independent_variable_data$sex_of_interest <- apply(
    sexes, 1, function(sexes) { # apply function row wise
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
      )
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
        independent_variable_data$sex_of_interest == 1, 
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
        independent_variable_data$sex_of_interest == 1, 
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
    marital_status %in% 1:3 ~ 1, # married
    marital_status %in% 5:7 ~ 2, # bereaved
    marital_status %in% 8:10 ~ 3, # divorced
    marital_status == 4 ~ 4 # unmarried
    ) |>
      
      # factorise variables
      factor()
  )

# parents_sexual_orientation
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_sexual_make_up = case_when(
      sexual_orientation == 1 ~ 0, # different sex
      sexual_orientation == 2 ~ 1, # same sex
    ) |>
      
      # factorise variables
      factor()
  )


# parent_1_sex
# sex of the responding parent/individual
independent_variable_data <- independent_variable_data |>
  mutate(parent_1_sex = case_when(
    respondent_sex == 1 ~ 0, # male (father) respondent
    respondent_sex == 2 ~ 1 # female (mother) respondent
    )
  )


# parent_2_sex
# sex of the other parent
independent_variable_data <-independent_variable_data |>
  mutate(
    parent_2_sex =  case_when(
    
      # unmarried individuals
      parents_marital_status != 1 ~ NA, # no parent_2
      
      # married individuals
      # different sex couples
      parents_marital_status == 1 & parents_sexual_make_up == 0 ~ 
        case_when(
          parent_1_sex == 1 ~ 0, # father as second parent
          parent_1_sex == 0 ~ 1  # mother is second parent
        ),
      
      # same sex couples
      parents_marital_status == 1 & parents_sexual_make_up == 1 ~ parent_1_sex
    )
  )


# parent_1_age
# age of responding parent/individual
independent_variable_data <- independent_variable_data |>
  mutate(parent_1_age = respondent_age)


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


# parent_chronic_illness
# describes whether the parent of the child who  
# responded to the survey has chronic afflictions

independent_variable_data <- independent_variable_data |>
  mutate(
    parent_chronic_illness = case_when(
      
      # filter out individuals who currently have a chronic illness
      if_any(respondent_hypertension:respondent_psychiatric_other, .fns = ~. %in% 3:5) == 1 ~ 3, # current chronic illness
      
      # filter out individuals who currently do not have chronic illnesses
      if_any(respondent_hypertension:respondent_psychiatric_other, .fns = ~. %in% 1:2) ~ case_when(
        if_all(respondent_hypertension:respondent_psychiatric_other, .fns = ~. == 1) ~ 1, # never had a chronic illness
        if_all(respondent_hypertension:respondent_psychiatric_other, .fns = ~. == 2) ~ 2 # have had a chronic illness in the past
      )
    ) |>
      
      # factorise parent_chronic_variables
      factor()
  )


# father_employment_status
# status of father's employment
# considering only heterosexual couples
independent_variable_data <- independent_variable_data |>
  mutate(
    father_employment_status = case_when(
      
      # ignore same sex couples
      parents_sexual_make_up == 1 ~ NA,
      
      # When the respondent is the father
      parent_1_sex == 0 ~ case_when(
        
        # respondent's employment status is father's employment status
        respondent_employment_status %in% c(13:16) ~ 0, # unemployed
        respondent_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        respondent_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        respondent_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      ),
      
      # When the respondent is the mother
      parent_1_sex == 1 ~ case_when(
        
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
      
      # ignore same sex couples
      parents_sexual_make_up == 1 ~ NA,
      
      # When respondent is the mother
      parent_1_sex == 1 ~ case_when(
        
        # respondent's employment status is mother's employment status
        respondent_employment_status %in% c(13:16) ~ 0, # unemployed
        respondent_employment_status %in% c(1, 5, 6) ~ 1, # regular office
        respondent_employment_status %in% c(2:4, 11) ~ 2, # self or family 
        respondent_employment_status %in% c(7:10, 12) ~ 3 # part-time & temp
      ),
      
      # When respondent is the father
      parent_1_sex ==  0 ~ case_when(
        
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
      
      # neither are retired
      # only true if both parents aren't retired
      all(
        
        # check to see if each parent is NOT retired
        c(respondent_employment_status, partner_employment_status) != 14
      ) ~ 0,
      
      # both retired
      all(
        
        # check if each parent is retired
        c(respondent_employment_status, partner_employment_status) == 14
      ) ~ 3,
      
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
      
      # neither are stay home parents
      all(
        c(respondent_employment_status & partner_employment_status) != 15
      ) ~ 0,
      
      # both parents are stay home parents
      all(
        c(respondent_employment_status & partner_employment_status) == 15
      ) ~ 3,
      
      # father is the stay home parent
      parent_1_sex == 0 & respondent_employment_status == 15 ~ 1,
      parent_2_sex == 0 & partner_employment_status == 15 ~ 1,
      
      # mother is the stay home parent
      parent_1_sex == 1 & respondent_employment_status == 15 ~ 2,
      parent_2_sex == 1 & partner_employment_status == 15 ~ 2,
    ) |>
      factor()
  )


# parents_healthcare
# indicates whether the the child's mother or father works in the
# healthcare sector

independent_variable_data <- independent_variable_data |>
  mutate(
    parents_healthcare = case_when(
    
    # seperate individuals who work in healthcare and non-healthcare sectors
    respondent_industry_of_work %in% c(1:14, 17:20) ~ 0, # non-healthcare
    respondent_industry_of_work %in% 15:16 & parent_1_sex ~ 1, # responding father in HC
    respondent_industry_of_work %in% 15:16 & parent_1_sex ~ 2, # responding mother in HC
    ) |>
      factor()
  )


# father_highest_education
# a variable that describes the highest educational attainment of a COI's father
independent_variable_data <- independent_variable_data |>
  mutate(
    father_highest_education = case_when(
      
      # filter out same-sex couples
      parents_sexual_make_up == 2 ~ NA,  
      
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
    ) |>
      factor()
  )


# mother_highest_education
# a variable that describes the highest educational attainment of a COI's mother
independent_variable_data <- independent_variable_data |>
  mutate(
    mother_highest_education = case_when(
      
      # filter out same-sex couples
      parents_sexual_make_up == 2 ~ NA,
      
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
    ) |>
      factor()
  )


# parents_highest_education
# a variable that describes the highest educational attainment between parent(s)

# extract a dataframe that contains the educational statuses
# of the responding parent and their partner (if applicable)
edu <- independent_variable_data |>
  select(c("highest_education", "highest_education_partner"))

# compare educational attainments and extract the highest one
independent_variable_data$edu <- 
  apply(edu, 1, max, na.rm = TRUE)

# assign the highest educational attainment achieved by both parents combined
independent_variable_data <- independent_variable_data |>
  mutate(
    parents_highest_education = case_when(
      edu == 1 ~ 0, # junior high qualification
      edu %in% 2:3 ~ 1, # high school qualification
      edu %in% 4:8 ~ 2, # tertiary qualification
      edu == 9 ~ 3, # post-graduate qualification
      edu %in% 10:11 ~ NA # no response
    ) |>
      
      # factorise educational classes
      factor(),
      
    # drop edu
    .keep = "unused"
  )


# parent_influenza
# variable that indicates whether the child's (responding parent)
# has received the flu vaccine in the past year
independent_variable_data <- independent_variable_data |>
  mutate(
    parent_influenza = case_when(
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
      covid_vaccination_respondent %in% 8:10 ~ 0, # no doses
      covid_vaccination_respondent == 6 ~ 1, # 1 dose
      covid_vaccination_respondent == 5 ~ 2, # 2 doses
      covid_vaccination_respondent == 4 ~ 3, # 3 doss
      covid_vaccination_respondent == 3 ~ 4, # 4 doses
      covid_vaccination_respondent == 2 ~ 5, # 5 doses
      covid_vaccination_respondent == 1 ~ 6, # 6 doses
      covid_vaccination_respondent == 7 ~ NA # ineligible
    )
  )

# parent_covid_coverage
# degree of coverage of parent and their reasons for not wanting to be vaccinated 
independent_variable_data <- independent_variable_data |>
  mutate(
    parent_covid_coverage = case_when(
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
      covid_vaccination_within1y == 2 &
      covid_vaccination_within6m == 2 ~ 0, # more than a year ago
      
      covid_vaccination_within1y == 1 &
      covid_vaccination_within6m == 2 ~ 1, # within a year
      
      covid_vaccination_within6m == 1 ~ 2 # within 6m
    ) |>
      
      # factorise time since parents' last covid vaccine
      factor()
  )
    
######## WORK FROM HERE ################

# parent_covid_exp_time
# the amount of time since the responding parent last had an
# encounter with covid

# extract all column names of the variables concerned with the
# respondent's experience with covid
covid_outcomes <- independent_variable_data|>
  select(
    starts_with("covid") & 
    !contains(c("vaccin", "dosag")) # exclude variables abou covid vaccination
  ) |>
    colnames()

# select the value representing the most recent encounter
# for each respondent
independent_variable_data$covid_outcomes_min <-
apply(
  
  # select cols containing respondent's intereactions with covid
  independent_variable_data[covid_outcomes],
  1,
  min,
  na.rm = TRUE
)

# assign categories to denote their most recent experience with covid
independent_variable_data <- independent_variable_data |>
  mutate(
    parent_covid_exp_time = 
      case_when(
        covid_outcomes_min == 5 ~ 0, # no experience
        covid_outcomes_min == 1 ~ 1, # last 3 months
        covid_outcomes_min == 2 ~ 2, # last 3 - 6 months
        covid_outcomes_min == 3 ~ 3, # last 6m - 1y
        covid_outcomes_min == 4 ~ 4 # more than a year ago
      ) |>
      
        # factorise variables
        factor()
  )
  

  
# parent_covid_exp_relation
# the nature of the relation between the parent and those affected
# by covid

  # select the columns containing the respondent's interactions with covid

  # respondent
  # respondent_outcomes <- 
  #   independent_variable_data |>
  #   select(all_of(covid_outcomes) & contains("respondent"))
  # 
  #     # filter presence of interaction
  #     apply(1, min, na.rm = TRUE)
  #     
  # # family living in the same household
  # family_same_outcomes <-
  #   independent_variable_data |>
  #   select(all_of(covid_outcomes) & contains("family")) |>
  #   
  #     # filter presence of interaction
  #     apply(1, min, na.rm = TRUE)
  # 
  # # family living apart
  # family_apart_outcomes <-
  #   independent_variable_data |>
  #   select(all_of(covid_outcomes) & contains("apart")) |>
  #     
  #     # filter presence of interaction
  #     apply(1, min, na.rm = TRUE)
  # 
  # # work colleagues or bosses
  # work_outcomes <-
  #   independent_variable_data |>
  #   select(all_of(covid_outcomes) & contains("work"))
  #       
  #     # filter presence of interaction
  #     apply(1, min, na.rm = TRUE)
  
  # categorise the interactions with covid by relation
      
      
### find a way to change priority???



# parent_covid_exp_severity
# the severity of their/someone affected's experience with covid



# select all columns that have been derived and prepared for use
  # load original CLEANED RAW data
  independent_variables <- readRDS(
    here("Data", "JACSIS2023", "processed", "independent_variable_data.RDS")
  )

  # obtain a vector of the column names in the CLEANED RAW data 
  # i.e. all columns from independent_variables
  to_remove_independent <- colnames(
    select(
      independent_variables, !starts_with("perception_")
    )
  )
    
  # choose variables that have been derived for use
  transformed_independent_variable_data <- independent_variable_data |>
    select(!all_of(to_remove_independent))
    

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



