
## Script Structure ###########################################################

# 1. Library Packages
# 2. Prepare Data
# 3. Check Structure
# 4. Missing Data
# 5. Duplicate Data
# 6. Outliers
# 7. Additional Formatting
# 8. Export Cleaned Data

## 1. Library Packages ########################################################

library(tidyverse) # Data Manipulation Package
library(haven)

## 2. Prepare Data ############################################################

# Load Data

data <- read_dta(file.path("1_Build//A_Input//data.dta")) #https://data.mendeley.com/datasets/cxpwbr3sjb/1

data <- read_csv(file.path("1_Build//A_Input//1-s2.0-S0304387818313154-mmc1.csv"))

## 3. Check Structure #########################################################

data[data == " "] <- NA

# Check Data Structure
str(data)
  
# Identify Relevant Variables
relevant_columns <- c("Identifier",
                      "Randomization1",
                      "Sex",
                      "Mstatus",
                      "HaveSaving12_a",
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
                      "HaveSaving12_a",
                      "iddir",
                      "Uptake1dummy") 

# Convert Date
data <- data %>% 
  mutate(IntervDate = as.Date(IntervDate, format = "%B %d, %Y"),
         Dateinterview2 = as.Date(Dateinterview2, format = "%B %d, %Y"))

str(data$Randomization1)
# Convert Factors
data <- data %>% 
  mutate(across(where(is.character),as_factor))

# Summary Statistics

summary(data)

## 4. Missing Data ############################################################

# Check All Missing Data
na_counts_all <- data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) # Missing values in data set

  # Missing All Data Count
na_counts_all <- na_counts_all %>% select_if(~ any(.!= 0)) # Missing values confined to mostly unused variables

# Check Relevant Variables - None
na_counts_rel <- data %>% select(all_of(relevant_columns)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) # Missing Values not relevant to replication

# Missing Relevant Data Count - None
na_counts_rel <- na_counts_rel %>% select_if(~any(.!= 0)) # Confirm missing values not relevant to missing data

# Handle Missing Data

  # No action required for missing data confined to irrelevant data


## 5. Duplicate Data ##########################################################

# Check Duplicates

dup_rows <- data %>%
  filter(duplicated(.)) # No duplicated data rows

# Handle Duplicates

# No action required on duplicate data

## 6 . Outliers ###############################################################

# Check for Outliers

  # Select Relevant Numerical Columns
data_num <- data %>% dplyr::select(all_of(relevant_columns)) %>%
  select_if(is.numeric) # 14 relevant variables

  # Convert to Character
data_num$Identifier <- as.character(data_num$Identifier) # Allows use of where(is.numeric) function

  # Calculate Z-Scores
data_num_zscr <- data_num %>% 
  mutate(across(where(is.numeric), ~ (.-mean(.))/sd(.))) # Outliers present

  # Count number of outliers by row
outlier_counts_col <- data_num_zscr %>%
  summarize(across(where(is.numeric), ~ sum(. > 3 | . < -3, na.rm = TRUE))) # Outliers counts by variable up to 299

# Missing Relevant Data Count - None
outlier_list <- data_num_zscr %>% 
  filter_at(vars(2:14), any_vars(abs(.) > 3)) %>%
  select(Identifier, everything()) # Lots of outliers but none seem outlandish with the exception of 1236. But may be high due to non-farming income


# Handle Outliers

  # No Action. Replicating results are consistent with paper. May warrant assessment with reduced sample 

## 7. Additional Formatting ###################################################

treatment_count_1 <- data %>% 
  group_by(Randomization1) %>%
  summarise(Observations = n_distinct(Identifier),
            Iddir = n_distinct(iddir))

## 8. Export Cleaned Data #####################################################

  # Write RDS Data 
saveRDS(data, "1_Build//C_Output//Cleaned_Data.rds") # Retains formatting

  # Write Excel Copy
write_csv(data, "1_Build//C_Output//Cleaned_Data.csv") # External Checks

## Script End #################################################################
