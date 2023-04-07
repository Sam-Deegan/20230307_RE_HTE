
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

levels(data$Randomization1)


# Specify "Standard Insurance (coops)" as the reference level (=0)
data$Randomization1 <- factor(data$Randomization1, levels = c("standard insurance through the usual channel (coops)", 
                                                              "Standard insurance through iddirs", 
                                                              "IOU insurance through iddirs with BC",
                                                              "IOU insurance through iddirs without BC",
                                                              "IOU insurance through the usual channel with BC",
                                                              "IOU insurance through usual channel without BC"))

# Specify factors levels to match Randomization
data$Uptake1 <- factor(data$Uptake1, levels = c("standard insurance through the usual channel (coops)", 
                                                              "Standard insurance through iddirs", 
                                                              "IOU insurance through iddirs with BC",
                                                              "IOU insurance through iddirs without BC",
                                                              "IOU insurance through the usual channel (coops) with BC",
                                                              "IOU insurance through usual channel (coops) without BC",
                                                              "Non-buyer"))
# Adjust names to match Randomization 1
levels(data$Uptake1) <- c("standard insurance through the usual channel (coops)",
                          "Standard insurance through iddirs", 
                          "IOU insurance through iddirs with BC",
                          "IOU insurance through iddirs without BC",
                          "IOU insurance through the usual channel with BC",
                          "IOU insurance through usual channel without BC",
                          "Non-buyer")

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

levels(data$Uptake1)
uptake_dummy <- model.matrix(~ Uptake1 - 1, data = data)

# Combine Data and Dummies
data <- cbind(data, treatment_dummy)
data <- cbind(data, uptake_dummy)

# Rename Randomization Dummies
data <- data %>%
  rename(Dum_Insrnce_Stndrd = "Randomization1standard insurance through the usual channel (coops)", 
         Dum_Insrnce_Iddr = "Randomization1Standard insurance through iddirs",
         Dum_IOU_Iddr_BC = "Randomization1IOU insurance through iddirs with BC",
         Dum_IOU_Iddr = "Randomization1IOU insurance through iddirs without BC",
         Dum_IOU_BC = "Randomization1IOU insurance through the usual channel with BC",
         Dum_IOU = "Randomization1IOU insurance through usual channel without BC")

# Rename Treatment Dummies
data <- data %>%
  rename(Dum_Trt_Insrnce_Stndrd = "Uptake1standard insurance through the usual channel (coops)", 
         Dum_Trt_Insrnce_Iddr = "Uptake1Standard insurance through iddirs",
         Dum_Trt_IOU_Iddr_BC = "Uptake1IOU insurance through iddirs with BC",
         Dum_Trt_IOU_Iddr = "Uptake1IOU insurance through iddirs without BC",
         Dum_Trt_IOU_BC = "Uptake1IOU insurance through the usual channel with BC",
         Dum_Trt_IOU = "Uptake1IOU insurance through usual channel without BC",
         Dum_Trt_NB = "Uptake1Non-buyer")

# Summary Statistics
treatment_count <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(iddir)) 

## 4.Randomisation ############################################################

# To Do:
  # Replication Problem 1: Iddir numbers per category do not match with the Section 3 Randomization
    # Observation numbers correct
    # Iddir and iddir are off slightly. Should match exactly
    # Randomization should align with Iddir level. 1 per Iddir not the case
    # Checked analysis script not jumbling factor levels. 
    # Checked cleaning script nothing jumbling the data
    # Checked input data, problem is in the dataset
    # Downloaded data again, problem persists. 
    # Downloaded data from different source. Same problem
    # Contacted author to see if we can get a clean copy

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

  # List y variable for purr
outcomes <- c("Age", "sex_dummy", "marriage_dummy", "Education", "Famsize", "TincomelastMnth", "FSevdrought", "buyIBIdummy")

  # List covariates for purr
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

  # Create balancing table list
balancing_test_1a <- list()

  # Create Wald test list
wald_test_1a <- list()

# Create an empty list to store the model summaries
model_summaries <- list()

# Loop through the outcome variable variables using purrr and map regression result to list 
balancing_test_1a <- map(outcomes, function(outcomes) {
  # Define your formula
  formula <- paste(outcomes, "~", paste(covariates, collapse = "+"))
  
  # Run the regression model
  model <- lm(formula, data = data)

  
  # Loop through wald combinations using purr and map wald results to list
  wald_test_1a <- map(wald_combinations, function(wald_combinations) {
    
    # Wald test
    wald_test <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), paste(wald_combinations))
    
    # Extract F-value
    w_coefficients <- na.omit(wald_test$"Pr(>F)")
    
    # Save coefficients as dataframe
    w_coefficients_df <- as.data.frame(w_coefficients)

    # Save Wald combinations as column   
    w_coefficients_df$wald_combinations <- list(paste(wald_combinations, collapse = "_"))
    
    # Save Column reference as column
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


  #Clean up redundant objects
rm(model_summaries_1a,model_summaries, wald_test_1a, w_results_list_1a)

head(w_results_1a)

## Balancing Tables

# Balancing Table 1a
table_1aa <- model_1a %>%
  select(-c(std.error, statistic)) %>%
  pivot_longer(cols = c(estimate, p.value), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = "model.id", values_from = "value") %>%
  mutate_at(vars(-c(term, variable)), ~round(., digits = 3)) %>% 
  rename_with(~outcomes, 3:10)
 
# Balancing Table 1b
table_1ab <- w_results_1a %>%
  select(wald_combinations, everything()) %>% 
  pivot_wider(names_from = outcomes, values_from = w_coefficients) %>% 
  select(-wald_combinations, starts_with("coefficients_")) %>% 
  round(2)

## 6. Balance Test 1b ##########################################################

# To Do:
# Wald Tests
# Table Results

# Balance Test 1b
colnames(data)

# List of outcome variables for purr
outcomes <- c("maizeqty", "HaricotQty", "Teffqty", "SorghumQty", "Wheatqty", "Barelyqty", "Cultlandsize10_a", "HaveSaving12_a")

# Loop through the list of outcomes and convert corresponding columns in df to numeric
data[outcomes] <- map_dfc(data[outcomes], as.numeric)

# Create balancing table list
balancing_test_1a <- list()

# Create Wald test list
wald_test_1a <- list()

# Create an empty list to store the model summaries
model_summaries <- list()

# Loop through the outcome variable variables using purrr and map regression result to list 
balancing_test_1b <- map(outcomes, function(outcomes) {
  # Define your formula
  formula <- paste(outcomes, "~", paste(covariates, collapse = "+"))
  
  # Run the regression model
  model <- lm(formula, data = data)
  
  
  # Loop through wald combinations using purr and map wald results to list
  wald_test_1b <- map(wald_combinations, function(wald_combinations) {
    
    # Wald test
    wald_test <- waldtest(model, vcov = vcovHC(model, cluster = "iddir"), paste(wald_combinations))
    
    # Extract F-value
    w_coefficients <- na.omit(wald_test$"Pr(>F)")
    
    # Save coefficients as dataframe
    w_coefficients_df <- as.data.frame(w_coefficients)
    
    # Save Wald combinations as column   
    w_coefficients_df$wald_combinations <- list(paste(wald_combinations, collapse = "_"))
    
    # Save Column reference as column
    w_coefficients_df$outcomes <- outcomes
    
    return(w_coefficients_df)                                  
  })
  
  # Save the wald test result for this outcome in wald_test_1b list
  w_results_df <- bind_rows(wald_test_1b)
  
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
model_summaries_1b <- map(balancing_test_1b, function(x) x$model_summary)
w_results_list_1b <- map(balancing_test_1b, function(x) x$w_results_df)

# Combine the wald test results into a single data frame
model_1b <- bind_rows(lapply(model_summaries_1b, tidy), .id= "model.id")
w_results_1b <- bind_rows(w_results_list_1b)


#Clean up redundant objects
rm(model_summaries_1b,model_summaries, wald_test_1b, w_results_list_1b)

## Balancing Tables

  # Balancing Table 2a
table_1ba <- model_1b %>%
  select(-c(std.error, statistic)) %>%
  pivot_longer(cols = c(estimate, p.value), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = "model.id", values_from = "value") %>%
  mutate_at(vars(-c(term, variable)), ~round(., digits = 3)) %>% 
  rename_with(~outcomes, 3:10)

  # Balancing Table 2b
table_1bb <- w_results_1b %>%
  select(wald_combinations, everything()) %>% 
  pivot_wider(names_from = outcomes, values_from = w_coefficients) %>% 
  select(-wald_combinations, starts_with("coefficients_")) %>% 
  round(2)


## 7. Insurance Uptake Rates ##################################################


  # Uptake regressions
uptk_rt_1 <- lm(Dum_Trt_Insrnce_Stndrd ~ Dum_Insrnce_Stndrd, data)
uptk_rt_2 <- lm(Dum_Trt_Insrnce_Iddr ~ Dum_Insrnce_Iddr, data)
uptk_rt_3 <- lm(Dum_Trt_IOU_Iddr_BC ~ Dum_IOU_Iddr_BC, data)
uptk_rt_4 <- lm(Dum_Trt_IOU_Iddr ~ Dum_IOU_Iddr, data)
uptk_rt_5 <- lm(Dum_Trt_IOU_BC ~ Dum_IOU_BC, data)
uptk_rt_6 <- lm(Dum_Trt_IOU ~ Dum_IOU, data)

# Compute robust clustered errors
uptk_rt_c1 <- vcovHC(uptk_rt_1, cluster="Iddir")
uptk_rt_c2 <- vcovHC(uptk_rt_2, cluster="Iddir")
uptk_rt_c3 <- vcovHC(uptk_rt_3, cluster="Iddir")
uptk_rt_c4 <- vcovHC(uptk_rt_4, cluster="Iddir")
uptk_rt_c5 <- vcovHC(uptk_rt_5, cluster="Iddir")
uptk_rt_c6 <- vcovHC(uptk_rt_6, cluster="Iddir")

# Extract coefficient estimates and confidence intervals for the first coefficient of each model
uptk_rt_1a <- confint(coeftest(uptk_rt_1, uptk_rt_c1))[2,]
uptk_rt_2a <- confint(coeftest(uptk_rt_2, uptk_rt_c2))[2,]
uptk_rt_3a <- confint(coeftest(uptk_rt_3, uptk_rt_c3))[2,]
uptk_rt_4a <- confint(coeftest(uptk_rt_4, uptk_rt_c4))[2,]
uptk_rt_5a <- confint(coeftest(uptk_rt_5, uptk_rt_c5))[2,]
uptk_rt_6a <- confint(coeftest(uptk_rt_6, uptk_rt_c6))[2,]

# Create a data frame with coefficient estimates and confidence intervals for each regression model
figure_2_df <- data.frame(Model=c("Index\nInsurance", "Index Insurance\nvia Iddir", "IOU via Iddir\nwith Contract", "IOU via\nIddir", "IOU with\nContract", "IOU"),
                 Estimate=c(coef(uptk_rt_1)[2], coef(uptk_rt_2)[2], coef(uptk_rt_3)[2], coef(uptk_rt_4)[2], coef(uptk_rt_5)[2], coef(uptk_rt_6)[2]),
                 LowerCI=c(uptk_rt_1a[2], uptk_rt_2a[2], uptk_rt_3a[2], uptk_rt_4a[2], uptk_rt_5a[2], uptk_rt_6a[2]),
                 UpperCI=c(uptk_rt_1a[1], uptk_rt_2a[1], uptk_rt_3a[1], uptk_rt_4a[1], uptk_rt_5a[1], uptk_rt_6a[1]))

# Plot bar graph with error bars
figure_2 <- ggplot(figure_2_df, aes(x=Model, y=Estimate)) +
  geom_bar(stat="identity", fill="gray50") +
  geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), width=.2, position=position_dodge(.9)) +
  labs(x="Randomisation", y="Coefficient Estimate (%)", 
       title="Mean of the First Coefficient with 95% Confidence Intervals") + 
  theme_classic() 

  # Cross-Tabulation of Treatment-uptake. Compilers on diagonal 
cross_tab <- data %>% 
  select(Randomization1, Uptake1) %>% 
  count(Randomization1, Uptake1) %>% 
  pivot_wider(names_from =Randomization1, values_from = n, values_fill= 0) %>%
  arrange(Uptake1)

# Identify common levels between Randomization1 and Uptake1
common_levels <- intersect(levels(data$Randomization1), levels(data$Uptake1))

data %>% 
  select(Randomization1, Uptake1, Iddir) %>% 
  group_by(Randomization1) %>%
  summarise(match_pct = sum(as.character(Randomization1) == as.character(Uptake1)) / n() * 100) %>%
  ungroup() 

# Models

  # Parsimonious Model 
prsmns_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU, data = data)
prsmns_se <- vcovCL(prsmns_mdl, cluster = data$Iddir)
excl_DltMt_mdl_2 <- coeftest(prsmns_mdl, prsmns_se)

  # Additional Model 
addtnl_mdl  <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data)
addtnl_se <- vcovCL(addtnl_mdl, cluster = data$Iddir)
addtnl_mdl_2  <- coeftest(addtnl_mdl, addtnl_se)

  # Excluding Daloti #Mati

data_filtered <- data %>% filter(Kebele != "Dalota Mati")
excl_DltMt_mdl <- lm(Uptake1dummy ~ Dum_Insrnce_Iddr + Dum_IOU_Iddr_BC + Dum_IOU_Iddr + Dum_IOU_BC + Dum_IOU + Age + sex_dummy + marriage_dummy + Education + Famsize + TincomelastMnth + FSevdrought + buyIBIdummy + maizeqty + HaricotQty + Teffqty + SorghumQty + Wheatqty + Barelyqty + Cultlandsize10_a + saving_dummy + factor(Kebele), data = data_filtered)
excl_DltMt_se <-vcovCL(excl_DltMt_mdl, cluster = data_filtered$Iddir)
excl_DltMt_mdl_2  <- coeftest(excl_DltMt_mdl, excl_DltMt_se)



# Wald Tests Model prsmns_mdl
mdl_1_w1 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU"))
mdl_1_w2 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_BC"))
mdl_1_w3 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"))
mdl_1_w4 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"))
mdl_1_w5 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_BC"))
mdl_1_w6 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr"))
mdl_1_w7 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr_BC"))
mdl_1_w8 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr"))
mdl_1_w9 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"))
mdl_1_w10 <- waldtest(prsmns_mdl, vcov = vcovHC(prsmns_mdl, cluster = "Iddir"), c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))

# Wald Tests Model addtnl_mdl
md2_1_w1 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU"))
md2_1_w2 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_BC"))
md2_1_w3 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"))
md2_1_w4 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"))
md2_1_w5 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_BC"))
md2_1_w6 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr"))
md2_1_w7 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr_BC"))
md2_1_w8 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr"))
md2_1_w9 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"))
md2_1_w10 <- waldtest(addtnl_mdl, vcov = vcovHC(addtnl_mdl, cluster = "Iddir"), c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))

# Wald Tests Model excl_DltMt_mdl
md3_1_w1 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU"))
md3_1_w2 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_BC"))
md3_1_w3 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr"))
md3_1_w4 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_Insrnce_Iddr", "Dum_IOU_Iddr_BC"))
md3_1_w5 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_BC"))
md3_1_w6 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr"))
md3_1_w7 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU", "Dum_IOU_Iddr_BC"))
md3_1_w8 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr"))
md3_1_w9 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU_BC", "Dum_IOU_Iddr_BC"))
md3_1_w10 <- waldtest(excl_DltMt_mdl, vcov = vcovHC(excl_DltMt_mdl, cluster = "Iddir"), c("Dum_IOU_Iddr", "Dum_IOU_Iddr_BC"))


## 8. Default Uptake Rates ####################################################

## 9. Plots and Graphs ########################################################

# Table 0 Observation and Iddirs
treatment_count

# Table 1a Balancing Test 1: Regressions 
table_1aa

# Table 1b Balancing Test 1: Wald
table_1ab

# Table 2a Balancing Test 2: Regressions 
table_1ba 

# Table 2b Balancing Test 2: Wald
table_1bb

# Additional: Cross-Tabulation of Treatment-uptake. Compliers on diagonal
cross_tab 

# Figure 2
figure_2




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

