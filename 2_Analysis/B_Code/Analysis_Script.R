
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
library(sandwich) # Standard Error Adjustment Package
library(lmtest) # Regression Model Testing Package
library(car) # 
library(MASS)

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

# Specify "Standard Insurance (coops)" as the reference level (=0)
data$Randomization1 <- factor(data$Randomization1, levels = c("standard insurance through the usual channel (coops)", 
                                                              "Standard insurance through iddirs", 
                                                              "IOU insurance through iddirs with BC",
                                                              "IOU insurance through iddirs without BC",
                                                              "IOU insurance through the usual channel with BC",
                                                              "IOU insurance through usual channel without BC"))

# Dummy Gender
data$sex_dummy <- ifelse(data$Sex == "Male", 1,0)

# Dummy Marriage
data$marriage_dummy <- ifelse(data$Mstatus == "Married", 1,0)

# Dummy Saving
data$saving_dummy <- ifelse(data$HaveSaving12_a == "Yes", 1,0)

# Numeric Education
data$Education <- as.numeric(as.character(data$Education))

# Numeric Farm Size
data$Famsize <- as.numeric(as.character(data$Famsize))

# Create Treatment Dummies  "Standard Insurance (coops)" set as 0
treatment_dummy <- model.matrix(~ Randomization1 - 1, data = data)

# Combine Data and Dummies
data <- cbind(data, treatment_dummy)

# Rename Treatment Dummies
data <- data %>%
  rename(Dum_Insrnce_Stndrd = "Randomization1standard insurance through the usual channel (coops)", 
         Dum_Insrnce_Iddr = "Randomization1Standard insurance through iddirs",
         Dum_IOU_Iddr_BC = "Randomization1IOU insurance through iddirs with BC",
         Dum_IOU_Iddr = "Randomization1IOU insurance through iddirs without BC",
         Dum_IOU_BC = "Randomization1IOU insurance through the usual channel with BC",
         Dum_IOU = "Randomization1IOU insurance through usual channel without BC")

# Summary Statistics

summary(data)

## 4.Randomisation ############################################################


  # Treatment Summary Statistics - Correct
treatment_count <- data %>%
  group_by(Randomization1) %>%
  summarise(Observations = n(),
            Iddirs = n_distinct(iddir))

  # Balancing Tests

    # Socio-Economic Balance

## 5. Balance Test 1 ##########################################################

# Balance Test 1a 

# Regress Socio-Economic Variable on Treatment - Correct
out_bal_1_var_1 <- lm(Age ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_2 <- lm(sex_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_3 <- lm(marriage_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_4 <- lm(Education ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_5 <- lm(Famsize ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_6 <- lm(TincomelastMnth ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_7 <- lm(FSevdrought ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1_var_8 <- lm(buyIBIdummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)

# Robust Cluster rrors by Iddir - Correct
se_1a_1 <- sqrt(diag(vcovCL(out_bal_1_var_1, cluster = data$iddir)))
se_1a_2 <- sqrt(diag(vcovCL(out_bal_1_var_2, cluster = data$iddir)))
se_1a_3 <- sqrt(diag(vcovCL(out_bal_1_var_3, cluster = data$iddir)))
se_1a_4 <- sqrt(diag(vcovCL(out_bal_1_var_4, cluster = data$iddir)))
se_1a_5 <- sqrt(diag(vcovCL(out_bal_1_var_5, cluster = data$iddir)))
se_1a_6 <- sqrt(diag(vcovCL(out_bal_1_var_6, cluster = data$iddir)))
se_1a_7 <- sqrt(diag(vcovCL(out_bal_1_var_7, cluster = data$iddir)))
se_1a_8 <- sqrt(diag(vcovCL(out_bal_1_var_8, cluster = data$iddir)))

# Wald Test - Not Correct
out_bal_1b_var_1 <- lm(Age ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_2 <- lm(sex_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_3 <- lm(marriage_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_4 <- lm(Education ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_5 <- lm(Famsize ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_6 <- lm(TincomelastMnth ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_7 <- lm(FSevdrought ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_8 <- lm(buyIBIdummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)



## 6. Balance Test 2 ##########################################################

# Balance Test 2a

# Regress Socio-Economic Variable on Treatment - Correct
out_bal_2_var_1 <- lm(maizeqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_2 <- lm(HaricotQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_3 <- lm(Teffqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_4 <- lm(SorghumQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_5 <- lm(Wheatqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_6 <- lm(Barelyqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data) # Spelling mistake on variable
out_bal_2_var_7 <- lm(Cultlandsize10_a ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_2_var_8 <- lm(saving_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)

# Robust Cluster Errors by Iddir - Correct
se_2a_1 <- sqrt(diag(vcovCL(out_bal_2_var_1, cluster = data$iddir)))
se_2a_2 <- sqrt(diag(vcovCL(out_bal_2_var_2, cluster = data$iddir)))
se_2a_3 <- sqrt(diag(vcovCL(out_bal_2_var_3, cluster = data$iddir)))
se_2a_4 <- sqrt(diag(vcovCL(out_bal_2_var_4, cluster = data$iddir)))
se_2a_5 <- sqrt(diag(vcovCL(out_bal_2_var_5, cluster = data$iddir)))
se_2a_6 <- sqrt(diag(vcovCL(out_bal_2_var_6, cluster = data$iddir)))
se_2a_7 <- sqrt(diag(vcovCL(out_bal_2_var_7, cluster = data$iddir)))
se_2a_8 <- sqrt(diag(vcovCL(out_bal_2_var_8, cluster = data$iddir)))

# Wald Test- Not Correct
out_bal_1b_var_1 <- lm(maizeqty  ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_2 <- lm(sHaricotQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_3 <- lm(Teffqty  ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_4 <- lm(SorghumQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_5 <- lm(Wheatqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_6 <- lm(TBarelyqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_7 <- lm(Cultlandsize10_a ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_8 <- lm(saving_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)

## 7. Insurance Uptake Rates ##################################################

  # Parsimonious Model 
prsmns_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
prsmns_se <- sqrt(diag(vcovCL(prsmns_mdl, cluster = data$iddir)))

  # Additional Model 
addtnl_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data)
addtnl_se <- sqrt(diag(vcovCL(addtnl_mdl, cluster = data$iddir)))
summary(addtnl_mdl)

  # Excluding Dalota Mati
  # Uncomplete!!
Excl_DltMt_mdl  <-  data %>% filter(data, Kebele == !"Dalota Mati") %>% lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data)
addtnl_se <- sqrt(diag(vcovCL(addtnl_mdl, cluster = data$iddir)))
summary(addtnl_mdl)
## 8. Plots and Graphs ########################################################

# Chapter 3 Treatments

treatment_count <- data %>%
  group_by(Randomization1) %>%
  summarise(Observations = n(),
            Iddirs = n_distinct(iddir))
        

 
# Compare Dummies to Paper
Treatment_check <- data %>%
  count(Randomization1, randomgr1_1, randomgr1_2, randomgr1_3, randomgr1_4, randomgr1_5, randomgr1_6) 

# Chapter 3: Socio-Economic Balance, Table 1 & 2

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
outcome_household <- c("Age", "Sex", "Mstatus", "Education", "Famsize", "TincomelastMnth", "droughtdummy", "buyIBIdummy")
outcome_farming <- c("Maze","Haricot","Teff","Sorghum","Wheat","Barley","Land","Savings")


# Chapter 3: Production and Savings, Table 1 & 2
results_household <- list()
for (y in outcome_household) {
  # Fit the regression model
  model <- lm(y ~ predictor_var)
  
  # Store the regression results
  results_household[[y]] <- summary(model)
}

results_farming <- list()
for (y in outcome_farming) {
  # Fit the regression model
  model <- lm(y ~ predictor_var)
  
  # Store the regression results
  results_farming[[y]] <- summary(model)
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