
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
library(kableExtra) # Create LaTeX tables
library(broom)

options(scipen = 999)
## 2. Prepare Data ############################################################

# Load Data
data <- readRDS(file.path("1_Build//C_Output//Cleaned_Data.rds"))


# Copy Data to Inputs
write_csv(data, "2_Analysis//A_Input//Cleaned_Data.csv")

## 3. Check Structure #########################################################

treatment_count_2 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(Iddir)) 

# Consider moving formatting to Build file to tidy up analysis. 

treatment_count_3 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(Iddir, na.rm=TRUE))
# Check Data Structure
str(data)

# Convert Date
data <- data %>% 
  mutate(IntervDate = as.Date(IntervDate, format = "%B %d, %Y"),
         Dateinterview2 = as.Date(Dateinterview2, format = "%B %d, %Y"))

# Convert Factors
data <- data %>% 
  mutate(across(where(is.character),as_factor))

levels(data$Randomization1)

treatment_count_3 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(Iddir))

# Specify "Standard Insurance (coops)" as the reference level (=0)
data$Randomization1 <- factor(data$Randomization1, levels = c("standard insurance through the usual channel (coops)", 
                                                              "Standard insurance through iddirs", 
                                                              "IOU insurance through iddirs with BC",
                                                              "IOU insurance through iddirs without BC",
                                                              "IOU insurance through the usual channel with BC",
                                                              "IOU insurance through usual channel without BC"))

treatment_count_3 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(Iddir))

levels(data$Randomization1)

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


treatment_count_4 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(iddir)) 

## 4.Randomisation ############################################################

# To Do:
  # Replication Problem 1: Iddir numbers per category do not match with the Section 3 Randomisation
    # Observation numbers correct

# Table Results

  # Treatment Summary Statistics - Correct
treatment_count <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(Iddir))

  # Check distinct observations by column to try find alternatives catrgorisatioon
data %>% summarize(across(everything(), n_distinct))

  # Uptake observations too low, Iddir numbers closer to true
treatment_count <- data %>% 
  group_by(Uptake1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(iddir))


## 5. Balance Test 1 ##########################################################

# To Do:
# Wald Tests
# Table Results

# Balance Test 1a 

# Regress Socio-Economic Variable on Treatment - Correct

outcomes <- c("Age", "sex_dummy", "marriage_dummy", "Education", "Famsize", "TincomelastMnth", "FSevdrought", "buyIBIdummy")

covariates <- c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC", "Dum_IOU_Iddr", "Dum_IOU_BC", "Dum_IOU")

wald_combinations <- list(c("Dum_Insrnce_Iddr", "Dum_IOU"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_BC"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU", "Dum_IOU_BC"),
                          c("Dum_IOU", "Dum_IOU_Iddr"),
                          c("Dum_IOU", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))

balancing_test_1a <- list()

wald_test_1a <- list()

# Create an empty list to store the model summaries
model_summaries <- list()

# Loop through the outcome variable variables using purrr and map regression result to list 
balancing_test_1a <- map(outcomes, function(outcomes) {
  # Define your formula
  formula <- paste(outcomes, "~", paste(covariates, collapse = "+"))
  
  # Run the regression model
  model <- lm(formula, data = data)
  
  # Run wald test
  wald_test_1a <- map(wald_combinations, function(wald_combinations) {
    
    wald_test <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), paste(wald_combinations))
    
    w_coefficients <- na.omit(wald_test$"Pr(>F)")
    
    w_coefficients_df <- as.data.frame(w_coefficients)
    
    w_coefficients_df$wald_combinations <- list(paste(wald_combinations, collapse = "_"))
    
    w_coefficients_df$outcomes <- outcomes
    
    return(w_coefficients_df)                                  
  })
  
  # Save the wald test result for this outcome in wald_test_1a list
  w_results_df <- bind_rows(wald_test_1a)
  
  # Extract the summary of the model
  model_summary <- summary(model)
  
  # Extract the coefficients from the summary
  coefficients <- model_summary$coefficients
  
  # Add the coefficients to a data frame
  coefficients_df <- as.data.frame(coefficients)
  
  # Add the y variable name to the data frame
  coefficients_df$outcomes <- outcomes
  
  # Return the data frames
  list(model_summary = model_summary, w_results_df = w_results_df)
})



# Combine the model summaries and wald test results into separate lists
model_summaries_1a <- map(balancing_test_1a, function(x) x$model_summary)
w_results_list_1a <- map(balancing_test_1a, function(x) x$w_results_df)

# Combine the wald test results into a single data frame
model_1a <- bind_rows(lapply(model_summaries_1a, tidy), .id= "model.id")
w_results_1a <- bind_rows(w_results_list_1a)


rm(model_summaries_1a,model_summaries, wald_test_1a, w_results_list_1a)


head(w_results_1a)

# Assuming the data frame is called w_results_df
table_1ab <- w_results_1a %>%
  select(wald_combinations, everything()) %>% 
  pivot_wider(names_from = outcomes, values_from = w_coefficients) %>% 
  select(-wald_combinations, starts_with("coefficients_")) %>% 
  round(2)




























# Loop through the outcome variable variables using purrr and map regression result to list 
balancing_test_1a <- map(outcomes, function(outcomes) {
  # Define your formula
  formula <- paste(outcomes, "~", paste(covariates, collapse = "+"))
  
  # Run the regression model
  model <- lm(formula, data = data)
  
  # Extract the summary of the model
  model_summary <- summary(model)
  
  # Extract the coefficients from the summary
  coefficients <- model_summary$coefficients
  
  # Add the coefficients to a data frame
  coefficients_df <- as.data.frame(coefficients)
  
  # Add the y variable name to the data frame
  coefficients_df$outcomes <- outcomes
  
  # Run wald test
  wald_test_1a <- map(wald_combinations, function(wald_combinations) {
    
    wald_test <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c(wald_combinations))
    
    w_coefficients <- na.omit(wald_test$"Pr(>F)")
    
    w_coefficients_df <- as.data.frame(w_coefficients)
      
    w_coefficients_df$wald_combinations <- list(paste(wald_combinations, collapse = "_"))
    
    w_coefficients_df$outcomes <- outcomes
    
    return(w_coefficients_df)                                  
  })
  
  # Save the wald test result for this outcome in wald_test_1a list
  w_results_df <- bind_rows(wald_test_1a)
  
  return(w_results_df)
  
  # Return the data frame
  return(coefficients_df)
})

# Combine the results into a single data frame
results_df <- bind_rows(balancing_test_1a)

head(balancing_test_1a)
head(results_df)
# Apply Wald test to each model from previous loop

wald_test_1a <- map(balancing_test_1a, function(bal_test) {
  map(wald_combinations, function(wald_comb) {
    # Extract the formula and data from bal_test
    formula <- bal_test$formula
    data <- bal_test$data
    
    # Fit the model
    model <- lm(formula, data)
    
    # Apply the Wald test
    wald_test <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), wald_comb)
    wald_summary <- summary(wald_test)
    coefficients <- wald_summary$coefficients
    return(coefficients)
  })
})


wald_combinations <- list(c("Dum_Insrnce_Iddr", "Dum_IOU"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_BC"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU", "Dum_IOU_BC"),
                          c("Dum_IOU", "Dum_IOU_Iddr"),
                          c("Dum_IOU", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))


  
out_bal_1_var_1 <- lm(Age ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)

# Run wald test
wald_test_1a <- map(wald_combinations, function(wald_combinations) {
  
  wald_test <- waldtest(out_bal_1_var_1 , vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c(wald_combinations))
  
  w_coefficients <- wald_test$"Pr(>F)"
  
  wald_test$out_bal_1_var_1 <- paste(wald_combinations, collapse = "_")

  return(w_coefficients)                                  
})
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


# List Wald Test Combinations
wald_combinations <- list(c("Dum_Insrnce_Iddr", "Dum_IOU"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_BC"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"),
                          c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU", "Dum_IOU_BC"),
                          c("Dum_IOU", "Dum_IOU_Iddr"),
                          c("Dum_IOU", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr"),
                          c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"),
                          c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))

test_results <- list()
for (vec in wald_combinations) {
  test <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), vec)
  name <- paste(vec, collapse = "_")
  
  # Create a new list for the test results of this combination
  this_result <- list(name = name, test = test)
  
  # Append the new list to the test_results list
  test_results <- c(test_results, list(this_result))
}
wald_combinations$
Wld_test_1b_var_1_wld1 <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU"))
Wld_test_1b_var_1_wld2 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_BC"))
Wld_test_1b_var_1_wld3 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld4 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld5 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_BC"))
Wld_test_1b_var_1_wld6 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld7 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld8 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld9 <- waldtest(model, vcov = vcovHC(omodel, cluster = "iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld10 <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))

# Wald Test - 1a Column 2
Wld_test_1b_var_1_wld1 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU"))
Wld_test_1b_var_1_wld2 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_BC"))
Wld_test_1b_var_1_wld3 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld4 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld5 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_BC"))
Wld_test_1b_var_1_wld6 <- waldtest(out_bal_1_var_2, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld7 <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld8 <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr"))
Wld_test_1b_var_1_wld9 <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"))
Wld_test_1b_var_1_wld10 <- waldtest(out_bal_1_var_1, vcov = vcovHC(out_bal_1_var_1, cluster = "iddir"), c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))


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

cf_predict_CATE <- predict(cf_multi, estimate_cate = TRUE)$predictions

## 3. Evaluate Model ##########################################################

# Calculate Average Treatment Effect
  # Estimate Average Treatment Effects and Error
average_treatment_effect(cf_multi, method = "AIPW",) # AIPW is for doubly robust errors

tune_pa(covariates, outcome, treatment, cluster= cf_data$Iddir)
  # Summary 
summary(cf_multi)


## 4. Model Evaluation ########################################################

  # Residual plot

  # Variable Importance Plot

  # Conditional Effect Plot

## 5. Plots and Graphs ########################################################

## Script End #################################################################

