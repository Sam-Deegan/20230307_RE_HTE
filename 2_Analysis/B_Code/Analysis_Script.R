
## Script Structure ###########################################################

  # A. Replication 
  # B. Extension 

## A. Replication #############################################################

  # 1. Library Packages
  # 2. Prepare Data
  # 3. Check Structure..
  # 4. Randomisation
  # 5. Balance Test 1
  # 6. Balance Test 2
  # 7. Insurance Uptake Rate
  # 8. Default Rates
  # 9. Plots and Graphs

## 1. Library Packages ########################################################

library(tidyverse) # Data Manipulation Package
library(stargazer) # Latex Tables Packages
library(grf) # Fitting Causal Forest Package
library(caret) # Machine Learning Package
library(sandwich) # Standard Error Adjustment Package
library(lmtest) # Regression Model Testing Package


## 2. Prepare Data ############################################################

# Load Data
data <- readRDS(file.path("1_Build//C_Output//Cleaned_Data.rds"))

# Copy Data to Inputs
write_csv(data, "2_Analysis//A_Input//Cleaned_Data.csv")

## 3. Check Structure #########################################################

# Consider moving formatting to Build file to tidy up analysis. 

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

summary(data$Iddir)

## 4.Randomisation ############################################################

# To Do:
  # Observation count by contract correct. 
    # Iddir numbers are high relative to paper.>24
    # Not Kebele, number to low, =12

# Table Results

  # Treatment Summary Statistics - Correct
treatment_count <- data %>%
  group_by(Randomization1) %>%
  summarise(Observations = n(),
            Iddirs = n_distinct(iddir))

  # Balancing Tests

    # Socio-Economic Balance

## 5. Balance Test 1 ##########################################################

# To Do:
# Wald Tests
# Table Results

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

# To Do:
# Wald Tests
# Table Results

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
out_bal_1b_var_2 <- lm(HaricotQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_3 <- lm(Teffqty  ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_4 <- lm(SorghumQty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_5 <- lm(Wheatqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_6 <- lm(Barelyqty ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_7 <- lm(Cultlandsize10_a ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
out_bal_1b_var_8 <- lm(saving_dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)

## 7. Insurance Uptake Rates ##################################################

# To Do:
  # Wald Tests
  # Table Results

  # Parsimonious Model 
prsmns_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
prsmns_se <- sqrt(diag(vcovCL(prsmns_mdl, cluster = data$iddir)))
summary(prsmns_mdl)

  # Additional Model 
addtnl_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data)
addtnl_se <- sqrt(diag(vcovCL(addtnl_mdl, cluster = data$iddir)))
summary(addtnl_mdl)

  # Excluding Daloti #Mati

data_filtered <- data %>% filter(Kebele != "Dalota Mati")

  # Additional Model  
excl_DltMt_mdl <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data)
excl_DltMt_se <- sqrt(diag(vcovCL(addtnl_mdl, cluster = data$iddir)))
summary(excl_DltMt_mdl)



## 8. Default Uptake Rates ####################################################

## 9. Plots and Graphs ########################################################


## B. Extension ###############################################################

  # 1. Library Packages
  # 2. Prepare Data
  # 3. Split Data
  # 4. Fit Causal Forest Model
  # 5. Predict Treatment Effects
  # 6. Calculate Average Treatment Effects
  # 7. Evaluate Model
  # 8. Plots and Graphs

## 1. Prepare Data ############################################################

  # Relevant Variables
relevant_columns <- c("Identifier",
                      "Randomization1",
                      "Uptake1dummy",
                      "Dum_Insrnce_Stndrd",
                      "Dum_Insrnce_Iddr",
                      "Dum_IOU_Iddr_BC",
                      "Dum_IOU_Iddr",
                      "Dum_IOU_BC",
                      "Dum_IOU",
                      "Age",
                      "sex_dummy",
                      "marriage_dummy",
                      "saving_dummy",
                      "Education",
                      "Famsize",
                      "TincomelastMnth",
                      "FSevdrought",
                      "buyIBIdummy",
                      "maizeqty",
                      "HaricotQty",
                      "Teffqty",
                      "SorghumQty",
                      "Wheatqty",
                      "Barelyqty",
                      "Cultlandsize10_a",
                      "Iddir") 

  # Separate Dataset
cf_data <- dplyr::select(data, relevant_columns)

  # Format Outcome
outcome <- as.matrix(cf_data$Uptake1dummy)

  # Format Treatment
treatment <- as.factor(cf_data$Randomization1)

  # Format Covariates
covariates <- as.matrix(data[,c("Age",
                                "sex_dummy",
                                "marriage_dummy",
                                "saving_dummy",
                                "Education",
                                "Famsize",
                                "FSevdrought",
                                "buyIBIdummy",
                                "maizeqty",
                                "HaricotQty",
                                "Teffqty",
                                "SorghumQty",
                                "Wheatqty",
                                "Barelyqty",
                                "Cultlandsize10_a")])

  # Format Iddir Cluster as 
cf_data$Iddir <- as.numeric(cf_data$Iddir)

## 2. Fit Model  ##############################################################

  # Run Multi-Arm Causal Model - Handles Multiple Treatments
cf_multi <- multi_arm_causal_forest(covariates, outcome, treatment, cluster= cf_data$Iddir,num.trees = 2000, seed =123)

  # Return Datapoints to Fit Model
cf_predict <- predict(cf_multi)


## 3. Evaluate Model ##########################################################

# Calculate Average Treatment Effect
# Estimate Average Treatment Effects and Error
average_treatment_effect(cf_multi, method = "AIPW",) # AIPW is for doubly robust errors

# Summary 
summary(cf_multi)

## 4. Plots and Graphs ########################################################

## Script End #################################################################

