
## Script Structure ###########################################################

## A. Replication #############################################################

# 1. Library Packages
# 2. Prepare Data
# 3. ...
# 4. ...
# 5. ...
# 6. ...
# 7. ...
# 8. Plots and Graphs

## B. Extension ###############################################################

# 1. Library Packages
# 2. Prepare Data
# 3. Split Data
# 4. Fit Causal Forest Model
# 5. Predict Treatment Effects
# 6. Calculate Average Treatment Effects
# 7. Evaluate Model
# 8. Plots and Graphs

## A. Replication #############################################################

## 1. Library Packages ########################################################

library(tidyverse) # Data Manipulation Package
library(stargazer) # Latex Tables Packages
library(grf) # Fitting Causal Forest Package
library(caret) # Machine Learning Package
library(sandwich)
library(lmtest)
## 2. Prepare Data ############################################################

# Load Data
data <- read_csv(file.path("1_Build//C_Output//Cleaned_Data.csv"))

# Copy Data to Inputs
write_csv(data, "2_Analysis//A_Input//Cleaned_Data.csv")

## 3. Check Structure #########################################################

# Check Data Structure
str(data)

# Convert Date
data <- data %>% 
  mutate(IntervDate = as.Date(IntervDate, format = "%B %d, %Y"),
         Dateinterview2 = as.Date(Dateinterview2, format = "%B %d, %Y"))

# Convert Factors
data <- data %>% 
  mutate(across(where(is.character),as_factor))

# Convert Factors
data <- data %>% 
  mutate(across(where(is.character),as_factor))

# Summary Statistics

summary(data)

## 4.Randomisation ############################################################


  # Treatment Summary Statistics
treatment_count <- data %>%
  group_by(Randomization1) %>%
  summarise(Observations = n(),
            Iddirs = n_distinct(iddir))

  # Balancing Tests

    # Socio-Economic Balance


## 5. ... ############################################################
## 6. ... ############################################################
## 7. ... ############################################################
## 8. Plots and Graphs ########################################################

# Chapter 3 Treatments

treatment_count <- data %>%
  group_by(Randomization1) %>%
  summarise(Observations = n(),
            Iddirs = n_distinct(iddir))

# Chapter 3: Socio-Economic Balance, Table 1

# specify "Standard Insurance (coops)" as the reference level
data$Randomization1 <- factor(data$Randomization1, levels = c("standard insurance through the usual channel (coops)", 
                                                              "Standard insurance through iddirs", 
                                                              "IOU insurance through iddirs with BC",
                                                              "IOU insurance through iddirs without BC",
                                                              "IOU insurance through the usual channel with BC",
                                                              "IOU insurance through usual channel without BC"))

# Create Dummy Variables. "Standard Insurance (coops)" set as 0
treatment_dummy <- model.matrix(~ Randomization1 - 1, data = data)

str(treatment_dummy)

# Combine Data and Dummies
data <- cbind(data, treatment_dummy)

# Rename Dummies
data <- data %>%
  rename(Dum_Insrnce_Stndrd_Iddr = "Randomization1standard insurance through the usual channel (coops)", 
         Dum_Insrnce_Iddr = "Randomization1Standard insurance through iddirs",
         Dum_IOU_Iddr_BC = "Randomization1IOU insurance through iddirs with BC",
         Dum_IOU_Iddr = "Randomization1IOU insurance through iddirs without BC",
         Dum_IOU_BC = "Randomization1IOU insurance through the usual channel with BC",
         Dum_IOU = "Randomization1IOU insurance through usual channel without BC")

# Define the outcome variable
predictor_var <- c("Dum_Insrnce_Stndrd_Iddr", "Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC", "Dum_IOU_Iddr", "Dum_IOU_BC", "Dum_IOU")

# Define the predictor variables
outcome_vars <- c("Age", "Sex", "Mstatus", "Education", "Famsize", "TincomelastMnth", "droughtdummy", "buyIBIdummy")



# Chapter 3: Production and Savings, Table 1
results <- list()
for (y in y_list) {
  # Fit the regression model
  model <- lm(y ~ predictor_var)
  
  # Store the regression results
  results[[y]] <- summary(model)
}


## B. Extension ###############################################################

## 1. Library Packages ########################################################

library(tidyverse) # Data Manipulation Package
library(grf) # Fitting Causal Forest Package
library(caret) # Machine Learning Package

## 2. Prepare Data ############################################################

# Load Data
data <- read_csv(file.path("1_Build//C_Output//Cleaned_Data.csv"))

# Copy Data to Inputs
write_csv(data, "2_Analysis//A_Input//Cleaned_Data.csv")


# Set Treatment Variable
treatment <- "treatment_variable_name"

# Set Outcome Variable
outcome <- "outcome_variable_name"

# Set covariates
covariates <- c("covariate1", "covariate2", "covariate3")

## 3. Split Data ##############################################################

# Set Seed for Reproducibility
set.seed(123)

# Split the Data
data_split <- trainTestSplit(data, testFraction = 0.3)
train_data <- data_split$train
test_data <- data_split$test

## 4. Fit Causal Forest Model #################################################

# Fit the Causal Forest Model
cf <- causal_forest(X = train_data[, covariates],
                    Y = train_data[, outcome],
                    W = train_data[, treatment])

## 6. Calculate Average Treatment Effects #####################################

# Predict Treatment Effects
treatment_effects <- predict(cf, newdata = test_data[, covariates])

## 7. Evaluate Model ##########################################################

# Calculate Average Treatment Effect
ate <- mean(treatment_effects)

## 8. Plots and Graphs ########################################################

## Script End #################################################################