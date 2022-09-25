####
  
Title: "MSc Project - Calculations"

Author: "Emily Chan"

Date: "09/04/2022"

####

df_surgery %>%
  distinct(subject, SURGERY_START_DT_TM, SURGERY_STOP_DT_TM) %>%
  count()

# Unique patients in cohort
length(unique(df_surgery[["subject"]]))

df_main %>%
  is.na(diabetes_type)

# Summary of Cohort Characteristics tidy_data_15
x <- tidy_data_15 %>% 
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

summary(x)
table(x$diabetes_type)
table(x$Ethnic_Category)
table(x$Gender)
table(x$outcome_flag)
sd(x$vitals_DBP, na.rm = TRUE)
sd(x$surg_length_mins, na.rm = TRUE)

table(x$Deprivation_score_on_admission)
table(x$Case_level)
table(x$anaesthetic_type)
table(x$Surgical_area)
table(x$surgical_specialty)



# Summary of Cohort Characteristics tidy_data_20
x <- tidy_data_20 %>% 
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

summary(x)
table(x$diabetes_type)
table(x$Ethnic_Category)
table(x$Gender)
table(x$outcome_flag)
sd(x$vitals_DBP, na.rm = TRUE)
sd(x$surg_length_mins, na.rm = TRUE)

table(x$Deprivation_score_on_admission)
table(x$Case_level)
table(x$anaesthetic_type)
table(x$Surgical_area)
table(x$surgical_specialty)



# Summary of Cohort Characteristics tidy_data_27.7
x <- tidy_data_27.7 %>% 
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

summary(x)
table(x$diabetes_type)
table(x$Ethnic_Category)
table(x$Gender)
table(x$outcome_flag)
sd(x$vitals_DBP, na.rm = TRUE)
sd(x$surg_length_mins, na.rm = TRUE)

table(x$Deprivation_score_on_admission)
table(x$Case_level)
table(x$anaesthetic_type)
table(x$Surgical_area)
table(x$surgical_specialty)

