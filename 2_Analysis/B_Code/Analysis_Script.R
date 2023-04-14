
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
library(broom) # Convert statistical models into data frames
library(cobalt)
library(gridExtra)

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

# Log TIncomeLastMnth
data$LTincomelastMnth <- log(data$TincomelastMnth+1)

# Numeric Education
data$Education <- as.numeric(as.character(data$Education))

# Numeric Farm Size
data$Famsize <- as.numeric(as.character(data$Famsize))

# Define Income Quintiles
thirds <- quantile(data$LTincomelastMnth, probs = seq(0, 1, 0.5), duplicates.ok = TRUE)


# create a new column indicating the quintile of each observation
data$Incm_mdn <- cut(data$LTincomelastMnth, breaks = thirds, labels = FALSE, duplicates.ok = TRUE)

quantile <- quantile(data$Cultlandsize10_a, probs = seq(0, 1, 0.2), duplicates.ok = TRUE)

data$clt_lnd_qntl <- cut(data$Cultlandsize10_a, breaks = quantile , labels = FALSE, duplicates.ok = TRUE)

plot(data$Cultlandsize10_a)
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
prsmns_mdl_2 <- coeftest(prsmns_mdl, prsmns_se)

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
kable(table_1aa, "latex") %>%
  kable_styling(bootstrap_options = NULL)

# Table 1b Balancing Test 1: Wald
table_1ab
kable(table_1ab, "latex") %>%
  kable_styling(bootstrap_options = NULL)

# Table 2a Balancing Test 2: Regressions 
table_1ba
kable(table_1ba, "latex") %>%
  kable_styling(bootstrap_options = NULL)

# Table 2b Balancing Test 2: Wald
table_1bb
kable(table_1bb, "latex") %>%
  kable_styling(bootstrap_options = NULL)

# Additional: Cross-Tabulation of Treatment-uptake. Compliers on diagonal
cross_tab 

# Figure 2
figure_2

# Tables

# Probably needs to be composed manually. 




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


# create a new column indicating the quintile of each observation
data$Incm_qntl <- cut(data$LTincomelastMnth, breaks = thirds, labels = FALSE, duplicates.ok = TRUE)

quantile <- quantile(data$Cultlandsize10_a, probs = seq(0, 1, 0.2), duplicates.ok = TRUE)

data$clt_lnd_qntl <- cut(data$Cultlandsize10_a, breaks = quantile , labels = FALSE, duplicates.ok = TRUE)

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
                      "Education",
                      "Mstatus",
                      "Famsize",
                      "LTincomelastMnth",
                      "saving_dummy",
                      "Iddir") 

cf_data <- data %>% select(relevant_columns)
  # Separate Dataset
levels(cf_data$Randomization1) <- c("Standrd", "Standard Iddir","IOU Iddir Contract", "IOU Iddir", "IOU Contract", "IOU")

  # Format Outcome
outcome <- as.matrix(cf_data$Uptake1)

treatment <- as.factor(cf_data$Randomization1)

  # Format Covariates
covariates <- as.matrix(data[,c("Age",
                                "Education",
                                "marriage_dummy",
                                "Famsize",
                                "LTincomelastMnth",
                                "saving_dummy")])


covariate_names <- c("Age",
                     "Education",
                     "marriage_dummy",
                     "Famsize",
                     "LTincomelastMnth",
                     "saving_dummy")

  # Format Iddir Cluster as numeric
cf_data$Iddir <- as.numeric(cf_data$Iddir)

## 2. Fit Model  ##############################################################

# Causal Forest Main Attempt
multi_cf_1 <- multi_arm_causal_forest(covariates, #Covariates
                                      outcome, # Outcomes
                                      treatment, #Treatments
                                      cluster= cf_data$Iddir, # cluster error by Iddir
                                      num.trees = 2000, # standard tree number
                                      honesty = TRUE, #Honesty on 
                                      seed =123) # reproducible seed
summary(multi_cf_1)
  # Don't look at distribution of predictions. Unreliable. See 4.2.3: https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/hte-i-binary-treatment.html

# See how often a variable was used to split a tree

  # Split by covariate
multi_cf_1_vi <- c(variable_importance(multi_cf_1))

  # Names covariate importance
names(multi_cf_1_vi) <- covariate_names

  # Sort covariates by importance
multi_cf_1_vi <- sort(multi_cf_1_vi, decreasing = TRUE)

  # Print importance
print(multi_cf_1_vi)

# Get predictions from fitted forest
multi_cf_1_tau <- predict(multi_cf_1, estimate.variance = T)

# Plot CATES. This is a very bad way of coding. However the data table is very messy and I want to preserve the order to allow us to plot against covariates.

  # Pull prediction data 

    # pull raw prediction
predictions <- as.data.frame(multi_cf_1_tau$predictions) 

      # rename treatments for simplicity
colnames(predictions) <- c("Standard Iddir","IOU Iddir Contract", "IOU Iddir", "IOU Contract", "IOU")

      # apply sequence to the data so I can retain correct order for plots
predictions <-  predictions %>% mutate(seq_number = row_number())

      # Pivot data long for merge
predictions <- predictions %>% pivot_longer(cols = c("Standard Iddir","IOU Iddir Contract", "IOU Iddir", "IOU Contract", "IOU"), names_to = "treatment", values_to = "Prediction") 

  # Pull variance data

    # Pull raw variance estimates
variance_est <- as.data.frame(multi_cf_1_tau$variance.estimates) 

    # Rename variance for simplicity
colnames(variance_est) <- c("Standard Iddir","IOU Iddir Contract", "IOU Iddir", "IOU Contract", "IOU") 

    # apply sequence to the data so I can retain correct order for plots
variance_est <-  variance_est %>% mutate(seq_number = row_number()) 

    # Pivot data long for merge
variance_est <- variance_est %>% pivot_longer(cols = c("Standard Iddir","IOU Iddir Contract", "IOU Iddir", "IOU Contract", "IOU"), names_to = "treatment", values_to = "variance") 

  # Merge prediction and variance data.

    # Bind variance to predictions
predictions <- cbind(predictions, variance_est$variance)

    # Rename columns
colnames(predictions) <- cbind("seq_number","treatment", "prediction", "variance") 
head(predictions)

  # Calculate confidence Intervals

    # Calculate the lower and upper confidence intervals for the predictions by group
predictions <- predictions %>%
  group_by(treatment) %>%
  mutate(lower_ci = prediction - 1.96 * sqrt(variance),
         upper_ci = prediction + 1.96 * sqrt(variance)) %>%
  ungroup()

covariates <-  covariates %>% as.data.frame() %>% mutate(seq_number = row_number()) 
predictions <- left_join(predictions, as_tibble(covariates), by = "seq_number")
    
head(predictions)


# Create an empty list to store the plots
Income_plot_list <- list()

for (t in unique(predictions$treatment)) {
  # Subset the data for the current treatment group
  subset_df <- predictions[predictions$treatment == t, ]
  
  # Create the plot using ggplot
  p <- ggplot(subset_df, aes(x = LTincomelastMnth, y = prediction)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, color = "blue") +
    scale_x_continuous() +
    scale_y_continuous(limits = c(-0.2, .60)) +
    labs(title = paste("CATE by Income", t),
         x = "Log Income Previous Month",
         y = "CATE") +
    theme_classic()
  
  # Create a unique name for the plot based on treatment and LTincomelastMnth
  lt_income <- unique(subset_df$LTincomelastMnth)
  plot_name <- paste0(t)
  
  # Create a sequence number to make the file name unique for each iteration
  seq_num <- 1
  while (file.exists(paste0(plot_name, "_", seq_num, ".png"))) {
    seq_num <- seq_num + 1
  }
  plot_name <- paste0(plot_name, "_", seq_num, ".png")
  
  # Save the plot with a unique name
  ggsave(filename = plot_name, plot = p, device = "png", path = "2_Analysis//C_Output//")
  
  # Capture the output of the print function to a variable
  p_output <- capture.output(print(p))
  
  # Assign the plot to a variable name and add it to the plot list
  plot_var_name <- paste0("plot_", t)
  Income_plot_list[[plot_var_name]] <- p
}

# Create an empty list to store the plots
Saving_plot_list <- list()

for (t in unique(predictions$treatment)) {
  # Subset the data for the current treatment group
  subset_df <- predictions[predictions$treatment == t, ]
  
  # Create the plot using ggplot
  p <- ggplot(subset_df, aes(x = LTincomelastMnth, y = prediction)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, color = "blue") +
    scale_x_continuous() +
    scale_y_continuous(limits = c(-0.2, .60)) +
    labs(title = paste("CATE by Savings", t),
         x = "Savings",
         y = "CATE") +
    theme_classic()
  
  # Create a unique name for the plot based on treatment and LTincomelastMnth
  lt_income <- unique(subset_df$LTincomelastMnth)
  plot_name <- paste0(t)
  
  # Create a sequence number to make the file name unique for each iteration
  seq_num <- 1
  while (file.exists(paste0(plot_name, "_", seq_num, ".png"))) {
    seq_num <- seq_num + 1
  }
  plot_name <- paste0(plot_name, "_", seq_num, ".png")
  
  # Save the plot with a unique name
  ggsave(filename = plot_name, plot = p, device = "png", path = "2_Analysis//C_Output//")
  
  # Capture the output of the print function to a variable
  p_output <- capture.output(print(p))
  
  # Assign the plot to a variable name and add it to the plot list
  plot_var_name <- paste0("plot_", t)
  Saving_plot_list[[plot_var_name]] <- p
}

# Income CATE plots
Income_plots <- grid.arrange(grobs = Income_plot_list, ncol = 3)

# Savings CATE plots
savings_plots <- grid.arrange(grobs = Saving_plot_list , ncol = 3)

# ATE
multi_ate <-average_treatment_effect(multi_cf_1)


# Calculate Heterogeneous Treatment Effect 

## Old Code Rewriting #####################################################

# Get a warning that the treatment propensities are close to 0 or 1. This means treatment effects are not well identified
  # Issue is present in most treatment groups. minimum treatment propensities in each arm are close to 0.
  # Reasons:
    # (Possible, lots of variables) High Dimensional Covariates: This can lead extreme treatment propensities due to overfitting. Section 3.4 of "Propensity Score Analysis: Statistical Methods and Applications" by Guo and Fraser (2015).
    # (Possible, treatment arms differ significantly in size) Imbalanced Treatment Assignment: Strong bias in treatment where one arm is much larger than the others. Section 2.2 of "Causal Forests: An Alternative to Randomized Controlled Trials" by Wager and Athey (2018)
    # (Possible, ) Model mispecification Section 2.2 of "Propensity Score Methods for Bias Reduction in the Comparison of a Treatment to a Non-Randomized Control Group" by Rosenbaum and Rubin (1983).
    # (Less likely, data is logged or binary) Non-linear relationships between covariates and treatment assignment Section 2.3 of "Causal Inference in Statistics: An Overview" by Pearl (2016).

 
  # Problem solving 1: Reduce Covariates - No solution

# cf_multi indicates that production quantities have a variable importance exceeding .1. However, "maizeqty", "Teffqty", "Wheatqty" exceed 0

# Format Covariates
#covariates <- as.matrix(data[,c("maizeqty",
#                                "HaricotQty",
#                                "Teffqty",
#                                "SorghumQty",
#                                "Wheatqty",
#                                "Barelyqty")])

# Run Multi-Arm Causal Model - Handles Multiple Treatments
#cf_multi <- multi_arm_causal_forest(covariates, outcome, treatment, cluster= cf_data$Iddir,num.trees = 2000, seed =123)

# Potential addition: 
#dbl_rbst_scr <- get_scores(cf_multi) # Improves result but issue persists, low propensity scores. Removing further was not helpful

# Return Datapoints to Fit Model
#cf_predict <- predict(cf_multi)

# Estimate CATE
#cf_predict_CATE <- predict(cf_multi, estimate_cate = TRUE)$predictions

# Problem solving 2: Imbalanced Treatment Assignment, No Solution

  # Format Covariates
#covariates <- as.matrix(data[,c("Age",
#                                "sex_dummy",
#                              "marriage_dummy",
#                                "saving_dummy",
#                                "Education",
#                                "Famsize",
#                                "FSevdrought",
#                                "buyIBIdummy",
#                                "maizeqty",
#                                "HaricotQty",
#                                "Teffqty",
#                                "SorghumQty",
#                                "Wheatqty",
#                                "Barelyqty",
#                                "Cultlandsize10_a")])

  # Check imbalanced treatment assignment
#smds <- cobalt::bal.tab(covariates, treatment, s.d.denom = "standard insurance through the usual channel (coops)")
  
  # Data is imbalanced (exceeds)
# summary(smds$Balance.Across.Pairs) # Max.Diff.Un is very close to the 0.2 threshold, but just below.

# Problem solving 3: Model mispecification. Non randomised control group. Not sure we can resolve this. Data issue

# Problem solving 4: Non linear relationship between control group and covariates. Issue persists despite reduducing no of variables to logged quantities

# Format Covariates
#covariates <- as.matrix(data[,c("LTincomelastMnth")])

# cf_multi <- multi_arm_causal_forest(covariates, outcome, treatment, cluster= cf_data$Iddir,num.trees = 2000, seed =123)

# Potential addition: 
#dbl_rbst_scr <- get_scores(cf_multi)

# Check imbalanced treatment assignment
# smds <- cobalt::bal.tab(covariates, treatment, s.d.denom = "standard insurance through the usual channel (coops)")

## 3. Simple Causal Model  ##########################################################

treatment <- as.matrix(as.numeric(data$xIDDIR))

# Format Covariates
covariates <- as.matrix(data[,c("Incm_mdn",
                                "saving_dummy")])

data <- data %>% mutate(uptake_any= if_else(uptake1gr1 + uptake1gr1 + uptake1gr1 + uptake1gr1 + uptake1gr1 + uptake1gr1 >= 1, 1, 0))

outcome <- as.matrix(data$uptake_any)

# Causal Forest Main Attempt
cf_1 <- causal_forest(covariates, #Covariates
                      outcome, # Outcomes
                      treatment, #Treatments
                      cluster= cf_data$Iddir, # cluster error by Iddir
                      num.trees = 2000, # standard tree number
                      honesty = TRUE, #Honesty on
                      seed =123) # reproducible seed

cf_import <- variable_importance(cf_1)

rowwnames <-  c("Incm_mdn", #important
                  "saving_dummy") #important

cf_plot$tau_hat <- predict(cf_1, covariates, estimate.variance = T)$predictions
cf_plot$sigma_hat <- predict(cf_1, covariates, estimate.variance = T)$variance.estimates
cfplot <- cf_plot %>% mutate(upper = tau_hat + 1.96 * sigma_hat,
                             lower = tau_hat - 1.96 * sigma_hat)
# Create a data frame with predicted values, lower and upper bounds of the 95% confidence interval, and covariates
cf_plot$Incm_mdn <- covariates[, 1]
cf_plot$saving_dummy <- covariates[, 2]

# Plot the predicted values as a line
median_inc_plot <- ggplot(cf_plot, aes(x = factor(Incm_mdn), y = tau_hat)) +
  geom_point() +
  # Add shaded area for the 95% confidence interval
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  # Add a horizontal line at y=0
  # Add axis labels
  scale_x_discrete(labels= c("Below Median", "Above Median")) +
  xlab("Income") +
  ylab("tau_hat")+
  theme_classic()
# Print summary of variables


  # Plot the predicted values as a line
  median_inc_plot <- ggplot(cf_plot, aes(x = saving_dummy, y = tau_hat)) +
    geom_point() +
    # Add shaded area for the 95% confidence interval
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    # Add a horizontal line at y=0
    # Add axis labels
    scale_x_discrete(labels= c("No Savings", "Savings")) + 
  xlab("Savings") +
    ylab("tau_hat")
  # Print summary of variables


# CATE on full sample
cf_cate <- average_treatment_effect(cf_1, target.sample = "all")

# CATT on full sample
cf_catt <- average_treatment_effect(cf_1, target.sample = "treated")

covariates_imp <- covariates %>% 
  as.data.frame() %>% 
  select(c("Incm_mdn", "saving_dummy")) %>% as.matrix()

cace_predict <- best_linear_projection(cf_1, covariates_imp, vcov.type = "HC3")
summary(cace_predict)

plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], tau.hat$predictions + 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], tau.hat$predictions - 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 1)

# Calculate Average Treatment Effect
  # Estimate Average Treatment Effects and Error
#average_treatment_effect(cf_multi, method = "AIPW",) # AIPW is for doubly robust errors

#tune_pa(covariates, outcome, treatment, cluster= cf_data$Iddir)

  # Summary 
#summary(cf_multi)


## 4. Model Evaluation ########################################################

# Multi_Arm Model

  # variable importance
print(multi_cf_1_vi)

  # CATE Plots against Income
Income_plots 

  # CATE Plots against Savings
savings_plots 

  # ATE of each treatment on insurance uptake 
multi_ate

  # Could not calculate heterogenous effects, coding limitations 
# Plot data by CATE instead. cannot doubly robust resulst to obtain HTE

#Simple causal model - on Iddir impact 
cf_import

  # ATE
cf_ate 

  # HTE
cace_predict 

  # Residual plot

  # Variable Importance Plot

  # Conditional Effect Plot

## 5. Plots and Graphs ########################################################

## Script End #################################################################

