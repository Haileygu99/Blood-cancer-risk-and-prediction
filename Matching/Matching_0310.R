################################ Load packages ################################ 
library(tidyverse)
library(dplyr)
library(table1)
library(MatchIt)
library(pacman)

################################ Load imputated dataset #######################
setwd('/rds/general/project/hda-22-23/live/TDS/Group5') 
imputed_data <- read.csv('imp_train/train_imp_df.csv') #Change to the new imputate data

# Matching with a ratio = 3 means 3:1 matching for control:cases
matched <- matchit(case_status ~ Age.at.recruitment.0.0 + Sex.0.0 + BMI.0.0, data = imputed_data, method = "nearest", ratio = 3, caliper = 0.2)
matched_data = match.data(matched)
summary(matched)

# The distributions of propensity scores can be visualized using the plot-function which is part of the MatchIt package
plot(matched, type = 'jitter', interactive = FALSE)

plot(matched, type = "density", interactive = FALSE,
     case_status = ~ Age.at.recruitment.0.0 + Sex.0.0 + BMI.0.0)

############################## Save data ############################# 
write.csv(matched_data, "matching/matched_df")     


