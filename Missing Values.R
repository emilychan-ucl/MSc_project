####
  
Title: "MSc Project - Missing Values"

Author: "Emily Chan"

Date: "09/04/2022"

####

# Load libraries
library(mice)

# Check proportion NA's
# sapply(tidy_data_15, function(x){sum(is.na(x))/length(x)*100})

# Transform variables into factors / numeric
main_table <- tidy_data_15 %>%
  select(-c(subject, first_surgery_startDT, first_surgery_stopDT)) %>%
  mutate(surg_length_mins = as.numeric(surg_length_mins),
         outcome_flag = as.factor(outcome_flag),
         Gender = as.factor(Gender),
         Ethnic_Category = as.factor(Ethnic_Category),
         diabetes_type = as.factor(diabetes_type),
         AgeAtTheEvent = as.numeric(AgeAtTheEvent),
         anaesthetic_type = as.factor(anaesthetic_type),
         Case_level = as.factor(Case_level),
         surgical_specialty = as.factor(surgical_specialty),
         Surgical_area = as.factor(Surgical_area),
         Deprivation_score_on_admission = as.factor(Deprivation_score_on_admission),
         lab_glucose = as.numeric(lab_glucose),
         vitals_DBP = as.numeric(vitals_DBP),
         vitals_SBP = as.numeric(vitals_SBP),
         vitals_HR = as.numeric(vitals_HR),
         vitals_RR = as.numeric(vitals_RR),
         vitals_temp = as.numeric(vitals_temp),
         Glucose_Gel = as.factor(Glucose_Gel),
         Injectable  = as.factor(Injectable),
         Insulin = as.factor(Insulin),
         OHA = as.factor(OHA),
         Pain_Relief = as.factor(Pain_Relief),
         Steroids = as.factor(Steroids))

# Inspect main table
str(main_table)
summary(main_table)

# Specify methods for imputing missing values
imp = mice(main_table, maxit= 0)
meth = imp$method
set.seed(123)

meth[c("lab_glucose", 
       "vitals_DBP", 
       "vitals_SBP", 
       "vitals_HR", 
       "vitals_RR", 
       "vitals_temp")] ="norm"
meth[c("anaesthetic_type", 
       "Deprivation_score_on_admission")] ="polyreg"

# Impute missing values using MICE
imputed_main <- mice(main_table,
                     m = 10,
                     method = meth)

## Generate complete dataset for model 
model_data <- complete(imputed_main, 1)
