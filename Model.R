####
  
Title: "MSc Project - Model1 Selection and Performance"

Author: "Emily Chan"

Date: "29/05/2022"

####

library(MASS) # stepAIC()
library(caret) # crossvalidation and calibration
library(e1071)
library(proxy)
library(pROC)
library(predtools)
library(gtools)

# Model selection using Stepwise Selection

# ## Check for NA's
# sapply(model_data, function(x){sum(is.na(x))/length(x)*100})
# 
# ## Fit model with all variables
# fit_model <- glm(outcome_flag ~ ., data = model_data, family = binomial)
# 
# ## Fit NULL model
# fit_model_null <- glm(outcome_flag ~ 1, data = model_data, family = binomial)
# 
# ## Stepwise selection of model variables
# stepAIC(fit_model_null,
#         scope = list(lower = fit_model_null, upper = fit_model),
#         direction = "both")

# Replace outcome_flag levels 0=No, 1=Yes
levels(model_data$outcome_flag) <- c("No", "Yes")

# Internal validation using cross-validation (CV)

## Set seed for reproducibility
set.seed(123)

## Define training control
trcontrol <- trainControl(method = "cv", number = 10, 
                          savePredictions = "all", 
                          classProbs = TRUE, 
                          summaryFunction = twoClassSummary)

## Train and test Model 1 using CV
model1 <- train(outcome_flag ~ lab_glucose + Insulin + surg_length_mins + 
                  surgical_specialty + diabetes_type + AgeAtTheEvent + Steroids + 
                  Pain_Relief + vitals_temp + Surgical_area,
               data = model_data, 
               method = "glm",
               family = "binomial",
               trControl = trcontrol)


# Performance for Model1
model1$pred
print(model1)
summary(model1)

caret::confusionMatrix(table((model1$pred)$pred, (model1$pred)$obs), positive = "Yes")

## Plot relative importance
plot(varImp(model1), top = 10)

## Plot ROC and calculate AUC
roc(predictor = model1$pred$Yes, response = model1$pred$obs, levels = c("No", "Yes"))
plot(roc(predictor = model1$pred$Yes, response = model1$pred$obs, levels = c("No", "Yes")), 
     legacy.axes = TRUE,
     main = "ROC (BG >= 15mmol/L)",
     xlab = "Specificity")


# Calibration
## Replace outcome_flag levels 0=No, 1=Yes
levels(model1$pred$obs) <- c(No = 0, Yes = 1)
model1$pred <- model1$pred %>% mutate(obs = as.numeric(as.character(obs)))
model1$pred

## Calibration plot
calibration_plot(data = model1$pred, 
                 obs = "obs", 
                 pred = "Yes", 
                 title = "Calibration plot >=15mmol/L")

## Calibration intercept
glm(model1$pred$obs ~ offset(logit(model1$pred$Yes)),
                     family = "binomial")

## Calibration slope
glm(model1$pred$obs ~ logit(model1$pred$Yes),
                 family = "binomial")



