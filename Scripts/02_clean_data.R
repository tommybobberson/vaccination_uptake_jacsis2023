# 02_clean_data

# This script creates 3 data frames (2 for depdendent variables and 1 for all the independent variables):

# 1. A data frame that contains the ages of the children, relative to the 
# response time of the survey - age_data
# 2. A data frame that contains the vaccination statuses and dates for the
# influenza vaccine of the respondents' children - influenza_data
# 3. A data frame that contains the vaccination statuses and dates for the covid 
# vaccine of the respondents' children - covid_data
# 4. A data frame that contains all chosen independent variables as 
# specified in the codebook - independent_data


# read raw data
data <- readRDS(
    here("Data", "JACSIS2023", "processed", "Responses Japanese.RDS")
  )


# create age_data ---------------------------------------------------------
# Select the columns of interest

age_data <- data |>
  select(
    # Year of birth of each child (yyyy)
    child_1_DOB_year = Q4.1,
    child_2_DOB_year = Q4.3,
    child_3_DOB_year = Q4.5,
    child_4_DOB_year = Q4.7,
    child_5_DOB_year = Q4.9,
    
    # Month of birth of each child (mm)
    child_1_DOB_month = Q4.2,
    child_2_DOB_month = Q4.4,
    child_3_DOB_month = Q4.6,
    child_4_DOB_month = Q4.8,
    child_5_DOB_month = Q4.10,
    
    # date of response (ISO format)
    response_time = 回答開始日時
  )

# save cleaned age_data
saveRDS(
  age_data, 
  here("Data", "JACSIS2023", "processed", "age_data.RDS")
  )



# create influenza_data ---------------------------------------------------


# Select the columns of interest and combine them into a data frame
# for the influenza vaccine:

influenza_data <- data |>
  select(
    #status of each dose
    influenza_first_dose = Q65.32,
    influenza_second_dose = Q65.33,
    
    #date of each dose
    influenza_first_dose_year = Q65S1.63,
    influenza_first_dose_month = Q65S1.64,
    influenza_second_dose_year = Q65S1.65,
    influenza_second_dose_month = Q65S1.66,
  )

# save cleaned influenza data 
saveRDS(
  influenza_data, 
  here("Data", "JACSIS2023", "processed", "influenza_data.RDS")
  )



# create covid_data -------------------------------------------------------

# select the columns of interest and combine them into a data frame
# for the covid vaccine:

covid_data <- data |>
  select(
        # status of each dose
        covid_first_dose = Q65.34,
        covid_second_dose = Q65.35,
        covid_third_dose = Q65.36,
        
        # date of each dose 
        covid_first_dose_year = Q65S1.67,
        covid_first_dose_month = Q65S1.68,
        covid_second_dose_year = Q65S1.69,
        covid_second_dose_month = Q65S1.70,
        covid_third_dose_year = Q65S1.71,
        covid_third_dose_month = Q65S1.72,
  )

# save cleaned covid data
saveRDS(
  covid_data, 
  here("Data", "JACSIS2023", "processed", "covid_data.RDS")
)



# creating independent_variables ------------------------------------------

# Selecting and renaming columns of interest

independent_variables <- data |>
  select(
    # Sex of each child
    child_1_sex = Q4S1.1,
    child_2_sex = Q4S1.2,
    child_3_sex = Q4S1.3,
    child_4_sex = Q4S1.4,
    child_5_sex = Q4S1.5,
    
    # Total number of individuals in the household
    total_individuals = Q1.1,
    
    # Number of individuals in the household relative to the respondent
    spouses = Q3.1, # number of spouses 
    children_under_18 = Q3.2, # respondents' children < 18y/0
    children_over_18 = Q3.3, # respondents' children >= 18y/o
    parents_in_law = Q3.4, # parents and parents in law of respondent
    grandchildren = Q3.5, # grandchildren of respondent
    grandparents = Q3.6, # siblings of respondent
    siblings = Q3.7, # grandparents of respondent
    other_individuals = Q3.8, #other individuals living with respondent
    bedridden = Q3.9, # individuals who are (close to being) bedridden
    need_ventilation = Q3.10, # individuals who need ventilators or sputum aspiration
    need_feeding = Q3.11, # individuals who need gastric waxing or tube feeding
    
    # respondent's marital Status
    marital_status = Q2,
    
    # respondent's sexual orientation
    sexual_orientation = Q35,
    
    # annual household income
    household_income_annual = Q90.1,
    household_assets = Q90.2,
    household_mortgage = Q90.3,
    household_debt = Q90.4,
    
    # respondents' chronic illness
    respondent_hypertension = Q40.1,
    respondent_diabetes = Q40.2,
    respondent_dyslipidemia = Q40.3,
    respondent_pneumonia = Q40.4,
    respondent_asthma = Q40.5,
    respondent_atopic_dermatitis = Q40.6,
    respondent_allergic_rhinitis = Q40.7,
    respondent_periodontitis = Q40.8,
    respondent_caries = Q40.9,
    respondent_cataract = Q40.10,
    respondent_angina = Q40.11,
    respondent_stroke = Q40.12,
    respondent_COPD = Q40.13,
    respondent_kidney_disease = Q40.14,
    respondent_hepatitis_cirrhosis = Q40.15,
    respondent_immune_abnormalities = Q40.16,
    respondent_cancer = Q40.17,
    respondent_chronic_pain = Q40.18,
    respondent_depression = Q40.19,
    respondent_psychiatric_other = Q40.20,
    
    # respondent's and respondent's partner's work situation
    respondent_employment_status = Q6.1,
    partner_employment_status = Q6.2,
    
    # respondents' industry of work
    respondent_industry_of_work = Q7,
    
    # highest educational attainment
    highest_education = Q21.1, # respondents' highest educational attainment
    highest_education_partner = Q21.2, # the respondent's partner's highest educational attainment
    
    # influenza vaccination status of respondent in the past year
    influenza_vaccination_respondent = Q64.5,
    
    # covid vaccination status of respondent
    covid_vaccination_within6m = Q64.7, # received the covid vax in the last 6 months
    covid_vaccination_within1y = Q64.8, # received the covid vax in the last year
    covid_vaccination_yearly_intention = Q64.9, # plan to receive the covid vax yearly
    covid_vaccination_respondent = Q70,
    
    # experiences with covid for those living together
    covid_respondent_diagnosed = Q67.1,
    covid_respondent_oxygen = Q67.2,
    covid_respondent_hospitalised = Q67.3,
    covid_respondent_ventilator = Q67.4,
    covid_family_diagnosed = Q67.5,
    covid_family_oxygen = Q67.6,
    covid_family_hospitalised = Q67.7,
    
    #experiences with covid in those related but living apart
    covid_apart_diagnosed = Q67.8,
    
    #experiences with covid in others' related by work
    covid_work_diagnosed = Q67.10,
    
    #experiences with the death of loved ones
    covid_death = Q67.9,
    
    # parents' perception of covid vaccines for their chilren
    perception_safety = Q5.1,
    perception_infection_danger = Q5.2,
    perception_immunisation_importance = Q5.3,
    perception_immunisation_benefits = Q5.4,
    perception_collective_immunisation = Q5.5,
    perception_immunisation_sanctions = Q5.6,
    perception_immunisation_adverse = Q5.7,
    perception_immunisation_herd = Q5.8,
    
    # sex of respondent
    respondent_sex = SEX,
    
    # age of respondent
    respondent_age = AGE
  )
  
# save cleaned independent variable data
saveRDS(
  independent_variables, 
  here("Data", "JACSIS2023", "processed", "independent_variable_data.RDS")
  )