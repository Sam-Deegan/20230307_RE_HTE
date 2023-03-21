
## Script Structure ###########################################################

# 1. Library Packages
# 2. Prepare Data
# 3. Check Structure
# 4. Missing Data
# 5. Duplicate Data
# 6. Outliers
# 7. Export Cleaned Data

## 1. Library Packages ########################################################

library(tidyverse) # Data Manipulation Package

## 2. Prepare Data ############################################################

# Load Data
data <- read_csv(file.path("1_Build//A_Input//1-s2.0-S0304387818313154-mmc1.csv"))

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

# Summary Statistics

summary(data)

## 4. Missing Data ############################################################

# Check Missing Data
na_counts <- data %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

  # Missing Data
na_counts <- na_counts %>% select(which(colSums(. == 0) == 0))

# Handle Missing Data

## No Action - Na's appear to be limited to non-essential data

## 5. Duplicate Data ##########################################################

# Check Duplicates
dup_rows <- data %>%
  filter(duplicated(.))

# Handle Duplicates

## No Action - No duplicate rows

## 6 . Outliers ###############################################################

# Check for Outliers

  # Select Numerical Columns
data_num <- data %>%
  select_if(is.numeric)

  # Calculate Z-Scores
data_num_zscr <- data_num %>% 
  mutate(across(where(is.numeric), ~ (.-mean(.))/sd(.)))

  # Count number of outliers by row
outlier_counts_col <- data_num_zscr %>%
  summarize(across(where(is.numeric), ~ sum(. > 3 | . < -3, na.rm = TRUE)))

# Handle Duplicates


  # No action yet. Too many outliers to consider. Need to limit focus to relevant outliers. Consult paper. 


## 7. Export Cleaned Data #####################################################

  # Write Data
write_csv(data, "1_Build//C_Output//Cleaned_Data.csv")


## Script End #################################################################