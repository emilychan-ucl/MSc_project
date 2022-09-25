
###
  
Title: "MSc Project - Tidy Data (Outcome Threshold = 15mmol/L)"

Author: "Emily Chan"

Date: "09/04/2022"

####

# Load libraries
library(tidyverse)
library(lubridate) # days()

# Load data files
df_demographics <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Demographics - Copy.csv")

df_surgery <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Surgery - Copy.csv")

df_glucosetests <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Glucose Tests.csv")

df_PMH <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Past Medical History.csv")

df_labs <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Laboratory Tests.csv")

df_medications <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Medications.csv")

df_vitals <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Vitals.csv")


# Tidy data

## Filter by first surgical procedure per patient
df_main <- df_surgery %>%
  group_by(subject) %>%
  summarise(first_surgery_startDT = min(SURGERY_START_DT_TM), first_surgery_stopDT = min(SURGERY_STOP_DT_TM)) %>%
  mutate(surg_length_mins = difftime(first_surgery_stopDT, first_surgery_startDT, units = "mins"))

## Clean df_glucosetests   
df_glucosetests <- df_glucosetests %>%
  mutate_at("Result", str_replace, "<", "") %>%
  mutate_at("Result", str_replace, ">", "") %>%
  mutate(Result = as.numeric(Result))

## Remove patients where BG>=15mmol/L prior to first surgical procedure
df_main <- left_join(df_main, df_glucosetests, by = "subject") %>%
  filter(CollectDT < first_surgery_startDT) %>%
  mutate(BG_beforesurg = if_else(Result >= 15, 1, 0)) %>%
  group_by(subject) %>%
  summarise(BG_beforesurg_flag = (if_else(sum(BG_beforesurg) > 2, 1, 0))) %>%
  full_join(df_main, by = "subject") %>%
  filter(BG_beforesurg_flag == 0) %>%
  select(-c(BG_beforesurg_flag))

## Flag Outcome (>=15mmol/L); TRUE = 1, FALSE = 0
df_outcome <- full_join(df_main, df_glucosetests, by = "subject") %>%
  filter(CollectDT > first_surgery_stopDT) %>%
  filter(CollectDT < (first_surgery_stopDT + days(14))) %>%
  mutate(outcome = if_else(Result >= 15, 1, 0)) %>%
  group_by(subject) %>%
  summarise(outcome_flag = (if_else(sum(outcome) > 2, 1, 0)))

df_main <- left_join(df_main, df_outcome, by = "subject")

## Demographic variables

### Load lkp table
df_ethnicitylkp <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Ethnicity Lkp.csv")

df_demographics <- df_demographics %>% left_join(df_ethnicitylkp, by = "Ethnicity")

df_main <- left_join(df_main, df_demographics %>% select(subject, Gender, Ethnic_Category), by = "subject") %>%
  distinct(subject, .keep_all = TRUE)


## Diabetes diagnosis type
### Filter duplicate rows
df_main <- left_join(df_main, df_PMH %>% select(subject, diabetes_type), by = "subject") %>%
  distinct(subject, .keep_all = TRUE)

## Surgical procedure variables
## Categorise Surgical Specialties
df_surgerylkp <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Surgery Lkp.csv")

df_surgery <- df_surgery %>% left_join(df_surgerylkp, by = "Surgical_specialty")

### Filter duplicate rows, take the first row where anaesthetic_type is not NULL
df_main <- left_join(df_main, df_surgery %>% select(subject, 
                                                    AgeAtTheEvent, 
                                                    SURGERY_STOP_DT_TM, 
                                                    anaesthetic_type, 
                                                    Case_level, 
                                                    surgical_specialty, 
                                                    Surgical_area,
                                                    Deprivation_score_on_admission), 
                     by = c("subject" = "subject", "first_surgery_stopDT" = "SURGERY_STOP_DT_TM")) %>%
  arrange(subject, is.na(anaesthetic_type)) %>%
  distinct(subject, .keep_all = TRUE) %>%
  mutate(surgical_specialty = tolower(surgical_specialty))


## Labs + Vitals Results

### Average lab_glucose
df_main <- full_join(df_main, df_glucosetests, by = "subject") %>%
  filter(CollectDT < first_surgery_startDT) %>%
  group_by(subject) %>%
  summarise(lab_glucose = mean(Result)) %>%
  full_join(df_main, by = "subject")

# ### Flag BG>7mmol/L
# df_main <- full_join(df_main, df_glucosetests, by = "subject") %>%
#   filter(CollectDT < first_surgery_startDT) %>%
#   mutate(lab_glucose = if_else(Result > 7, 1, 0)) %>%
#   group_by(subject) %>%
#   summarise(lab_glucose = (if_else(sum(lab_glucose) > 1, 1, 0))) %>%
#   full_join(df_main, by = "subject")

### Average vitals_DBP
df_main <- full_join(df_main, df_vitals, by = "subject") %>%
  filter(Event_Description == "Diastolic Blood Pressure Cuff") %>%
  filter(Event_dt_tm < first_surgery_startDT) %>%
  mutate(Result = as.numeric(Event_result)) %>%
  group_by(subject) %>%
  summarise(vitals_DBP = mean(Result)) %>%
  full_join(df_main, by = "subject")

### Average vitals_SBP
df_main <- full_join(df_main, df_vitals, by = "subject") %>%
  filter(Event_Description == "Systolic Blood Pressure Cuff") %>%
  filter(Event_dt_tm < first_surgery_startDT) %>%
  mutate(Result = as.numeric(Event_result)) %>%
  group_by(subject) %>%
  summarise(vitals_SBP = mean(Result)) %>%
  full_join(df_main, by = "subject")

### Average vitals_HR
df_main <- full_join(df_main, df_vitals, by = "subject") %>%
  filter(Event_Description == "Heart Rate") %>%
  filter(Event_dt_tm < first_surgery_startDT) %>%
  mutate(Result = as.numeric(Event_result)) %>%
  group_by(subject) %>%
  summarise(vitals_HR = mean(Result)) %>%
  full_join(df_main, by = "subject")

### Average vitals_RR
df_main <- full_join(df_main, df_vitals, by = "subject") %>%
  filter(Event_Description == "Respiratory Rate") %>%
  filter(Event_dt_tm < first_surgery_startDT) %>%
  mutate(Result = as.numeric(Event_result)) %>%
  group_by(subject) %>%
  summarise(vitals_RR = mean(Result)) %>%
  full_join(df_main, by = "subject")

### Average vitals_temp
df_main <- full_join(df_main, df_vitals, by = "subject") %>%
  filter(Event_Description == "Temperature") %>%
  filter(Event_dt_tm < first_surgery_startDT) %>%
  mutate(Result = as.numeric(Event_result)) %>%
  group_by(subject) %>%
  summarise(vitals_temp = mean(Result)) %>%
  full_join(df_main, by = "subject")


## Medications

### Load lkp table
df_medications_lkp <- read_csv(file = "S:/Business Intelligence - Covid Analytics Project/NIBDAPC Diabtetes Hyperglycaemia Project (105)/MSc Project/Data/Medications Lkp.csv")

### Clean df_medications
df_medications <- df_medications %>% 
  mutate(Medication_Name = tolower(Medication_Name))

df_medications_lkp <- df_medications_lkp %>% 
  mutate(Medication = tolower(Medication))

df_medcat <- df_medications %>%
  left_join(df_medications_lkp, by = c("Medication_Name" = "Medication"))

## Flag medication type
df_meds <- full_join(df_main, df_medcat, by = "subject") %>%
  filter(Administration_Datetime < first_surgery_startDT) %>%
  count(subject, Medication_Category) %>%
  mutate(n = +(n > 0)) %>%
  pivot_wider(names_from = Medication_Category, 
              values_from = n, 
              values_fill = list(n = 0))

df_main <- left_join(df_main, df_meds %>% select("subject", 
                                                 "Glucose_Gel", 
                                                 "Injectable", 
                                                 "Insulin", 
                                                 "OHA", 
                                                 "Pain_Relief", 
                                                 "Steroids"), 
                     by = "subject")

df_main[c("Glucose_Gel", 
          "Injectable", 
          "Insulin", 
          "OHA", 
          "Pain_Relief", 
          "Steroids")][is.na(df_main[c("Glucose_Gel", 
                                       "Injectable", 
                                       "Insulin", 
                                       "OHA", 
                                       "Pain_Relief", 
                                       "Steroids")])] <- 0


# Remove NA's from outcome_flag
tidy_data_15 <- df_main[!is.na(df_main$outcome_flag), ]

