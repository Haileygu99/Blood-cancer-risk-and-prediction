


################################ Load packages ################################ 

library(tidyverse)
library(dplyr)
library(table1)
library(MatchIt)
library(pacman)

setwd('/rds/general/project/hda-22-23/live/TDS/Group5') 

############################### Cases ###################################### 

# Load cases 
df_leuk = read.csv('Outputs_imputation/imputed_df_leuk.csv')
df_lymph = read.csv('Outputs_imputation/imputed_df_lymph.csv')

# Get rid of counter column
df_lymph = df_lymph[-1]
df_leuk = df_leuk[-1]

# Join leuk and lymph
columns = colnames(df_leuk)
columns = columns[-12]
cases = full_join(df_leuk, df_lymph, by = columns)

# Add case status
cases$case_status = 1

# Set NAs to zero
cases$case_leuk = ifelse(is.na(cases$case_leuk), 0, 1)
cases$case_lymph = ifelse(is.na(cases$case_lymph), 0, 1)

# Delete redundant tables
rm(df_leuk)
rm(df_lymph)

############################### Controls ##################################### 

# Load controls 
controls = read.csv('Outputs_imputation/imputed_df_control.csv')
controls = controls[-1]
controls = controls[-12]
controls$case_leuk = 0 
controls$case_lymph = 0 
controls$case_status = 0 


###################### Combine case and controls ########################## 

columns = colnames(cases)
matched = full_join(cases, controls, by = columns)

############################## Matching ############################# 

set.seed(1234)
# Ratio = 3 means 3:1 matching for control:cases
matched = matchit(case_status ~ Age.at.recruitment.0.0 + Sex.0.0 , data = matched, method = "nearest", ratio = 3, caliper = 0.2)
matched_data = match.data(matched)
summary(matched)

# The distributions of propensity scores can be visualized using the plot-function which is part of the MatchIt package
plot(matched, type = 'jitter', interactive = FALSE)

plot(matched, type = "density", interactive = FALSE,
     cancer_type = ~ Age.at.recruitment.0.0 + Sex.0.0)


# Eventually, we can check whether the differences in the level of distress between both samples are still significan


pacman::p_load(tableone)
tableone <- CreateTableOne( 
  data = matched_test, 
  factorVars = 'Sex.0.0', 
  strata = 'cancer_type')
tableone <- print(table4, 
                  printToggle = FALSE, 
                  noSpaces = TRUE)



############################## Save data ############################# 

write.csv(matched_data, "matching/matched_data")     
View(matched_data)

