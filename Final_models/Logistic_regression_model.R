# Loading datasets --------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tableone)
library(ggplot2)
library(caTools)
library(GGally)
library(ROCR)
library(sjPlot)
library(sjlabelled)
library(e1071)
library(pROC)

# Cleaning dataset  --------------------------------------------------------
# loading dataset - change the directory to own clustered dataset 
setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
df_train_case = read.csv("matching/matched_df.csv")
df_test_case = read.csv("imp_test/test_imputed_df.csv")
train_df_3km = read.csv("Models/train_df_3kmeans.csv")
train_df_6km = read.csv("Models/train_df_6kmeans.csv")
train_df_7km = read.csv("Models/train_df_7kmeans.csv")
train_df_gmm = read.csv("Models/train_df_gmm.csv")
test_df_3km = read.csv("Models/test_df_3kmeans.csv")
test_df_6km = read.csv("Models/test_df_6kmeans.csv")
test_df_7km = read.csv("Models/test_df_7kmeans.csv")
test_df_gmm = read.csv("Models/test_df_gmm.csv")
test_df_8gmm = read.csv("Models/test_df_8gmm.csv")
train_df_8gmm = read.csv("Models/train_df_8gmm.csv")


# creating a full dataframe for training set 
train_df_3km$X <- rownames(train_df_3km)
train_df_6km$X <- rownames(train_df_6km)
train_df_7km$X <- rownames(train_df_7km)
df_train_case <- df_train_case[, c(1, 26, 27, 28)]
df_train_3km <- merge(df_train_case, train_df_3km, by = "X")
df_train_6km <- merge(df_train_case, train_df_6km, by = "X")
df_train_7km <- merge(df_train_case, train_df_7km, by = "X")
train_df_gmm$X <- rownames(train_df_gmm)
df_train_gmm <- merge(df_train_case, train_df_gmm, by = "X")
train_df_8gmm$X <- rownames(train_df_8gmm)
df_train_8gmm <- merge(df_train_case, train_df_8gmm, by = "X")


# creating a full dataframe for test set 
df_test_case <- df_test_case[, c(1, 25, 26, 27)]
df_test_3km <- merge(df_test_case, test_df_3km, by = "X")
df_test_6km <- merge(df_test_case, test_df_6km, by = "X")
df_test_7km <- merge(df_test_case, test_df_7km, by = "X")
df_test_gmm <- merge(df_test_case, test_df_gmm, by = "X")
df_test_8gmm <- merge(df_test_case, test_df_8gmm, by = "X")

# Renaming columns - Sex
df_train_3km <- df_train_3km %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
df_test_3km <- df_test_3km %>% rename(Sex1 = Sex.0.0_1, Mood_swings = Mood_swings.0.0_1)
 
df_train_6km <- df_train_6km %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
df_test_6km <- df_test_6km %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
 
df_train_7km <- df_train_7km %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
df_test_7km <- df_test_7km %>% rename(Sex1 = Sex.0.0_1, Mood_swings = Mood_swings.0.0_1)

df_train_gmm <- df_train_gmm %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
df_test_gmm <- df_test_gmm %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)

df_train_8gmm <- df_train_8gmm %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)
df_test_8gmm <- df_test_8gmm %>% rename(Sex1 = Sex.0.0_1,Mood_swings = Mood_swings.0.0_1)

df_train_3km$Alcohol_status <- ifelse(df_train_3km$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(df_train_3km$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(df_train_3km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_train_3km$Smoking_status <- ifelse(df_train_3km$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(df_train_3km$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(df_train_3km$Smoking_status.0.0_0 == 1, "Never", NA)))
df_test_3km$Alcohol_status <- ifelse(df_test_3km$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(df_test_3km$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(df_test_3km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_test_3km$Smoking_status <- ifelse(df_test_3km$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(df_test_3km$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(df_test_3km$Smoking_status.0.0_0 == 1, "Never", NA)))

df_train_6km$Alcohol_status <- ifelse(df_train_6km$Alc_drinker_status.0.0_1== 1, "Past",
                                      ifelse(df_train_6km$Alc_drinker_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_6km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_train_6km$Smoking_status <- ifelse(df_train_6km$Smoking_status.0.0_1 == 1, "Past",
                                      ifelse(df_train_6km$Smoking_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_6km$Smoking_status.0.0_0 == 1, "Never", NA)))
df_test_6km$Alcohol_status <- ifelse(df_test_6km$Alc_drinker_status.0.0_1== 1, "Past",
                                     ifelse(df_test_6km$Alc_drinker_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_6km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_test_6km$Smoking_status <- ifelse(df_test_6km$Smoking_status.0.0_1 == 1, "Past",
                                     ifelse(df_test_6km$Smoking_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_6km$Smoking_status.0.0_0 == 1, "Never", NA)))

df_train_7km$Alcohol_status <- ifelse(df_train_7km$Alc_drinker_status.0.0_1== 1, "Past",
                                      ifelse(df_train_7km$Alc_drinker_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_7km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_train_7km$Smoking_status <- ifelse(df_train_7km$Smoking_status.0.0_1 == 1, "Past",
                                      ifelse(df_train_7km$Smoking_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_7km$Smoking_status.0.0_0 == 1, "Never", NA)))
df_test_7km$Alcohol_status <- ifelse(df_test_7km$Alc_drinker_status.0.0_1== 1, "Past",
                                     ifelse(df_test_7km$Alc_drinker_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_7km$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_test_7km$Smoking_status <- ifelse(df_test_7km$Smoking_status.0.0_1 == 1, "Past",
                                     ifelse(df_test_7km$Smoking_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_7km$Smoking_status.0.0_0 == 1, "Never", NA)))

df_train_gmm$Alcohol_status <- ifelse(df_train_gmm$Alc_drinker_status.0.0_1== 1, "Past",
                                      ifelse(df_train_gmm$Alc_drinker_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_gmm$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_train_gmm$Smoking_status <- ifelse(df_train_gmm$Smoking_status.0.0_1 == 1, "Past",
                                      ifelse(df_train_gmm$Smoking_status.0.0_2 == 1, "Current",
                                             ifelse(df_train_gmm$Smoking_status.0.0_0 == 1, "Never", NA)))
df_test_gmm$Alcohol_status <- ifelse(df_test_gmm$Alc_drinker_status.0.0_1== 1, "Past",
                                     ifelse(df_test_gmm$Alc_drinker_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_gmm$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
df_test_gmm$Smoking_status <- ifelse(df_test_gmm$Smoking_status.0.0_1 == 1, "Past",
                                     ifelse(df_test_gmm$Smoking_status.0.0_2 == 1, "Current",
                                            ifelse(df_test_gmm$Smoking_status.0.0_0 == 1, "Never", NA)))
df_train_8gmm$Alcohol_status <- ifelse(df_train_8gmm$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(df_train_8gmm$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
df_train_8gmm$Smoking_status <- ifelse(df_train_8gmm$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(df_train_8gmm$Smoking_status.0.0_2 == 1, "Current", "Never"))
df_test_8gmm$Alcohol_status <- ifelse(df_test_8gmm$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(df_test_8gmm$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
df_test_8gmm$Smoking_status <- ifelse(df_test_8gmm$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(df_test_8gmm$Smoking_status.0.0_2 == 1, "Current", "Never"))



df_train_3km <- df_train_3km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)
df_test_3km <- df_test_3km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                          Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                          RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                          Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                          Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                          C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

df_train_6km <- df_train_6km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                        Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                        RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                        Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                        Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                        C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)
df_test_6km <- df_test_6km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                      Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                      RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                      Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                      Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                      C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

df_train_7km <- df_train_7km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                        Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                        RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                        Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                        Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                        C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)
df_test_7km <- df_test_7km %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                      Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                      RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                      Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                      Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                      C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

df_train_gmm <- df_train_gmm %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                        Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                        RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                        Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                        Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                        C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)
df_test_gmm <- df_test_gmm %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                      Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                      RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                      Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                      Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                      C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)
df_test_8gmm <- df_test_8gmm %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

df_train_8gmm <- df_train_8gmm %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)



#delete unnecessary columns 
df_test_3km <- select(df_test_3km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_train_3km <- select(df_train_3km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_train_6km <- select(df_train_6km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_test_6km <- select(df_test_6km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_train_7km <- select(df_train_7km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_test_7km <- select(df_test_7km, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
df_train_gmm <- select(df_train_gmm, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2,-cluster_ncomponents_1)
df_test_gmm <- select(df_test_gmm, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2,-cluster_ncomponents_1)
df_train_8gmm <- select(df_train_8gmm,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)
df_test_8gmm <- select(df_test_8gmm,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)


# --------------------Model-------------------------------------------
### Fundamental model - Covariates  (BMI,sex,age,smoking status)
# creating subset for fundamental model 
case_train_cov <- df_train_3km[, c("case_status","BMI","Age","Sex1","Smoking_status")]
case_test_cov <- df_test_3km[,c("case_status","BMI","Age","Sex1","Smoking_status")]

# change categorical to factors 
case_train_cov$Smoking_status <- factor(case_train_cov$Smoking_status)
case_train_cov$Smoking_status <- factor(case_train_cov$Smoking_status)

case_train_cov$Smoking_status <- relevel(case_train_cov$Smoking_status, ref="Never")
case_train_cov$Smoking_status <- relevel(case_train_cov$Smoking_status, ref="Never")

log_cov_case = glm(case_status ~ ., data=case_train_cov, family=binomial)
summary(log_cov_case)
plot_log_cov_case <- plot_model(log_cov_case, sort.est = TRUE, show.valuesvalue.offset = .3,
                               show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Covariate Model: Odds Ratios for Blood Cancer", ci_method=wald)
plot_log_cov_case

# Calculate the odds ratio
exp(coef(log_cov_case))
exp(cbind(coef(log_cov_case), confint(log_cov_case)))

# predict on the test set
predict_log_cov_case = predict(log_cov_case, type="response", newdata=case_test_cov)
cm_log_cov_case = table(case_test_cov$case_status, predict_log_cov_case > 0.5)
print(cm_log_cov_case)

summary(predict_log_cov_case)

# Recall
print((diag(cm_log_cov_case) / colSums(cm_log_cov_case))[2])

# Precision 
print((diag(cm_log_cov_case) / rowSums(cm_log_cov_case))[2])
ROCRpred_log_cov_case = prediction(predict_log_cov_case, case_test_cov$case_status)
as.numeric(performance(ROCRpred_log_cov_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_cov_case <- performance(ROCRpred_log_cov_case, "tpr","fpr")
plot(perf_log_cov_case ,colorize=TRUE,main = "Cov Model: ROC Curve for Blood Cancer")
performance(ROCRpred_log_cov_case,"prec","rec")

# Training
log_cov_pred_train <- predict(log_cov_case, type = "response", newdata = case_train_cov)
cm_log_cov_pred_train = table(case_train_cov$case_status, predict_log_cov_pred_train > 0.5)
print(cm_log_cov_pred_train)
# Recall
print((diag(cm_log_cov_pred_train) / colSums(cm_log_cov_pred_train))[2])
# Precision 
print((diag(cm_log_cov_pred_train) / rowSums(cm_log_cov_pred_train))[2])
ROCRpred_log_cov_case_train = prediction(log_cov_pred_train, case_train_cov$case_status)
as.numeric(performance(ROCRpred_log_cov_case_train, "auc")@y.values)

# ---------------------------------------------------------------------------------
# K-means model Cluster membership + Covariates (BMI,sex,age,smoking status)
# K-means = 3
# creating subset for K-means model 
case_train_km3 <- df_train_3km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]
case_test_km3 <- df_train_3km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]

# change categorical to factors 
case_train_km3$Smoking_status <- factor(case_train_km3$Smoking_status)
case_train_km3$Smoking_status <- factor(case_train_km3$Smoking_status)

case_train_km3$Smoking_status <- relevel(case_train_km3$Smoking_status, ref="Never")
case_train_km3$Smoking_status <- relevel(case_train_km3$Smoking_status, ref="Never")

#model
log_km3_case =glm(case_status ~ ., data=case_train_km3, family=binomial)
summary(log_km3_case)
plot_log_km3_case <- plot_model(log_km3_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE,
                               wrap.title = 50, title = "K-means Model: Odds Ratios for Blood Cancer (k=3)", ci_method=wald)
plot_log_km3_case

# Calculate the odds ratio
exp(coef(log_km3_case))
exp(cbind(coef(log_km3_case), confint(log_km3_case)))

# predict on the test set
predict_log_km3_case = predict(log_km3_case, type="response", newdata=case_test_km3)
cm_log_km3_case = table(case_test_km3$case_status, predict_log_km3_case > 0.5)
print(cm_log_km3_case)

# Recall
print((diag(cm_log_km3_case) / colSums(cm_log_km3_case))[2])

# Precision 
print((diag(cm_log_km3_case) / rowSums(cm_log_km3_case))[2])

ROCRpred_log_km3_case = prediction(predict_log_km3_case, case_test_km3$case_status)
as.numeric(performance(ROCRpred_log_km3_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km3_case <- performance(ROCRpred_log_km3_case, "tpr","fpr")
plot(perf_log_km3_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=3)")
performance(ROCRpred_log_km3_case,"prec","rec")

# ----- kmeans = 3
# Training
log_km3_case_train_pred <- predict(log_km3_case, type = "response", newdata = case_train_km3)
cm_log_km3_case_train_pred = table(case_train_km3$case_status, log_km3_case_train_pred > 0.5)
print(cm_log_km3_case_train_pred)
# Recall
print((diag(cm_log_km3_case_train_pred) / colSums(cm_log_km3_case_train_pred))[2])
# Precision 
print((diag(cm_log_km3_case_train_pred) / rowSums(cm_log_km3_case_train_pred))[2])
ROCRlog_km3_case_train_pred = prediction(log_km3_case_train_pred, case_train_km3$case_status)
as.numeric(performance(ROCRlog_km3_case_train_pred, "auc")@y.values)

# ---------------------------------------------------------------------------------
# K-means model Cluster membership + Covariates (BMI,sex,age,smoking status)
# K-means = 6
# creating subset for K-means model 
case_train_km6 <- df_train_6km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]
case_test_km6 <- df_train_6km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]


# change categorical to factors 
case_train_km6$Smoking_status <- factor(case_train_km6$Smoking_status)
case_train_km6$Smoking_status <- factor(case_train_km6$Smoking_status)

case_train_km6$Smoking_status <- relevel(case_train_km6$Smoking_status, ref="Never")
case_train_km6$Smoking_status <- relevel(case_train_km6$Smoking_status, ref="Never")

#model
log_km6_case =glm(case_status ~ ., data=case_train_km6, family=binomial)
summary(log_km6_case)
plot_log_km6_case <- plot_model(log_km6_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE,
                                wrap.title = 50, title = "K-means Model: Odds Ratios for Blood Cancer (k=6)", ci_method=wald)
plot_log_km6_case

# Calculate the odds ratio
exp(coef(log_km6_case))
exp(cbind(coef(log_km6_case), confint(log_km6_case)))

# predict on the test set
predict_log_km6_case = predict(log_km6_case, type="response", newdata=case_test_km6)
cm_log_km6_case = table(case_test_km6$case_status, predict_log_km6_case > 0.5)
print(cm_log_km6_case)

# Recall
print((diag(cm_log_km6_case) / colSums(cm_log_km6_case))[2])

# Precision 
print((diag(cm_log_km6_case) / rowSums(cm_log_km6_case))[2])

ROCRpred_log_km6_case = prediction(predict_log_km6_case, case_test_km6$case_status)
as.numeric(performance(ROCRpred_log_km6_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km6_case <- performance(ROCRpred_log_km6_case, "tpr","fpr")
plot(perf_log_km6_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=6)")
performance(ROCRpred_log_km6_case,"prec","rec")

# TRAINING km=6 
train_predict_log_km6_case = predict(log_km6_case, type="response", newdata=case_train_km6)
cm_train_predict_log_km6_case = table(case_train_km6$case_status, train_predict_log_km6_case > 0.5)
# Recall
print((diag(cm_train_predict_log_km6_case) / colSums(cm_train_predict_log_km6_case))[2])
# Precision 
print((diag(cm_train_predict_log_km6_case) / rowSums(cm_train_predict_log_km6_case))[2])
ROCRtrain_predict_log_km6_case = prediction(train_predict_log_km6_case, case_train_km6$case_status)
as.numeric(performance(ROCRtrain_predict_log_km6_case, "auc")@y.values)

# ---------------------------------------------------------------------------------
# K-means model Cluster membership + Covariates (BMI,sex,age,smoking status)
# K-means = 7
# creating subset for K-means model 
case_train_km7 <- df_train_7km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5")]
case_test_km7 <- df_train_7km[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5")]


# change categorical to factors 
case_train_km7$Smoking_status <- factor(case_train_km7$Smoking_status)
case_train_km7$Smoking_status <- factor(case_train_km7$Smoking_status)

case_train_km7$Smoking_status <- relevel(case_train_km7$Smoking_status, ref="Never")
case_train_km7$Smoking_status <- relevel(case_train_km7$Smoking_status, ref="Never")

#model
log_km7_case =glm(case_status ~ ., data=case_train_km7, family=binomial)
summary(log_km7_case)
plot_log_km7_case <- plot_model(log_km7_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE,
                                wrap.title = 50, title = "K-means Model: Odds Ratios for Blood Cancer (k=7)", ci_method=wald)
plot_log_km7_case

# Calculate the odds ratio
exp(coef(log_km7_case))
exp(cbind(coef(log_km7_case), confint(log_km7_case)))

# predict on the test set
predict_log_km7_case = predict(log_km7_case, type="response", newdata=case_test_km7)
cm_log_km7_case = table(case_test_km7$case_status, predict_log_km7_case > 0.5)
print(cm_log_km7_case)

# Recall
print((diag(cm_log_km7_case) / colSums(cm_log_km7_case))[2])

# Precision 
print((diag(cm_log_km7_case) / rowSums(cm_log_km7_case))[2])

ROCRpred_log_km7_case = prediction(predict_log_km7_case, case_test_km7$case_status)
as.numeric(performance(ROCRpred_log_km7_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km7_case <- performance(ROCRpred_log_km7_case, "tpr","fpr")
plot(perf_log_km7_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=7)")
performance(ROCRpred_log_km7_case,"prec","rec")

# TRAINING km=7
train_predict_log_km7_case = predict(log_km7_case, type="response", newdata=case_train_km7)
cm_train_predict_log_km7_case = table(case_train_km7$case_status, train_predict_log_km7_case > 0.5)
# Recall
print((diag(cm_train_predict_log_km7_case) / colSums(cm_train_predict_log_km7_case))[2])
# Precision 
print((diag(cm_train_predict_log_km7_case) / rowSums(cm_train_predict_log_km7_case))[2])
ROCRtrain_predict_log_km7_case = prediction(train_predict_log_km7_case, case_train_km7$case_status)
as.numeric(performance(ROCRtrain_predict_log_km7_case, "auc")@y.values)


## ---------------------------- GMM model --------------------------------------------------------------
## GMM model: Cluster membership + Covariates (BMI,sex,age,smoking status)
# creating subset for gmm model 
case_train_gmm <- df_train_gmm[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]
case_test_gmm<- df_test_gmm[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]

# change categorical to factors 
case_train_gmm$Smoking_status <- factor(case_train_gmm$Smoking_status)
case_test_gmm$Smoking_status <- factor(case_test_gmm$Smoking_status)

case_train_gmm$Smoking_status <- relevel(case_train_gmm$Smoking_status, ref="Never")
case_test_gmm$Smoking_status <- relevel(case_test_gmm$Smoking_status, ref="Never")

log_gmm_case =glm(case_status ~ ., data=case_train_gmm, family=binomial)
summary(log_gmm_case)
plot_log_gmm_case <- plot_model(log_gmm_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "GMM Model: Odds Ratios for Blood Cancer (k=2)",ci_method=wald)
plot_log_gmm_case

# Calculate the odds ratio
exp(coef(log_gmm_case))
exp(cbind(coef(log_gmm_case), confint(log_gmm_case)))

# predict on the test set
predict_log_gmm_case = predict(log_gmm_case, type="response", newdata=case_test_gmm)
cm_log_gmm_case = table(case_test_gmm$case_status, predict_log_gmm_case > 0.5)
print(cm_log_gmm_case)

# Recall
print((diag(cm_log_gmm_case) / colSums(cm_log_gmm_case))[2])

# Precision 
print((diag(cm_log_gmm_case) / rowSums(cm_log_gmm_case))[2])
ROCRpred_log_gmm_case = prediction(predict_log_gmm_case, case_test_gmm$case_status)
as.numeric(performance(ROCRpred_log_gmm_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_gmm_case <- performance(ROCRpred_log_gmm_case, "tpr","fpr")
plot(perf_log_gmm_case ,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=2)")
performance(ROCRpred_log_gmm_case,"prec","rec")

# training set gmm 
train_predict_log_gmm_case = predict(log_gmm_case, type="response", newdata=case_train_gmm)
cm_train_predict_log_gmm_case = table(case_train_gmm$case_status, train_predict_log_gmm_case > 0.5)
# Recall
print((diag(cm_train_predict_log_gmm_case) / colSums(cm_train_predict_log_gmm_case))[2])
# Precision 
print((diag(cm_train_predict_log_gmm_case) / rowSums(cm_train_predict_log_gmm_case))[2])
ROCRtrain_predict_log_gmm_case = prediction(train_predict_log_gmm_case, case_train_gmm$case_status)
as.numeric(performance(ROCRtrain_predict_log_gmm_case, "auc")@y.values)

###-----------------------------------------------------------------------
# gmm=8
# creating subset for gmm model 
case_train_8gmm <- df_train_8gmm[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]
case_test_8gmm <- df_test_8gmm[, c("case_status","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]

# change categorical to factors 
case_train_8gmm$Smoking_status <- factor(case_train_8gmm$Smoking_status)
case_test_8gmm$Smoking_status <- factor(case_test_8gmm$Smoking_status)

case_train_8gmm$Smoking_status <- relevel(case_train_8gmm$Smoking_status, ref="Never")
case_test_8gmm$Smoking_status <- relevel(case_test_8gmm$Smoking_status, ref="Never")

log_8gmm_case =glm(case_status ~ ., data=case_train_8gmm, family=binomial)
summary(log_8gmm_case)
plot_log_8gmm_case <- plot_model(log_8gmm_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "GMM Model: Odds Ratios for Blood Cancer (k=8)",ci_method=wald)
plot_log_8gmm_case

# Calculate the odds ratio
exp(coef(log_8gmm_case))
exp(cbind(coef(log_8gmm_case), confint(log_8gmm_case)))

# predict on the test set
predict_log_8gmm_case = predict(log_8gmm_case, type="response", newdata=case_test_8gmm)
cm_log_8gmm_case = table(case_test_8gmm$case_status, predict_log_8gmm_case > 0.5)
print(cm_log_8gmm_case)

# Recall
print((diag(cm_log_8gmm_case) / colSums(cm_log_8gmm_case))[2])

# Precision 
print((diag(cm_log_8gmm_case) / rowSums(cm_log_8gmm_case))[2])
ROCRpred_log_8gmm_case = prediction(predict_log_8gmm_case, case_test_8gmm$case_status)
as.numeric(performance(ROCRpred_log_8gmm_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_8gmm_case <- performance(ROCRpred_log_8gmm_case, "tpr","fpr")
plot(perf_log_8gmm_case ,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=8)")
performance(ROCRpred_log_8gmm_case,"prec","rec")

# training set gmm 
train_predict_log_8gmm_case = predict(log_8gmm_case, type="response", newdata=case_train_8gmm)
cm_train_predict_log_8gmm_case = table(case_train_8gmm$case_status, train_predict_log_8gmm_case > 0.5)
# Recall
print((diag(cm_train_predict_log_8gmm_case) / colSums(cm_train_predict_log_8gmm_case))[2])
# Precision 
print((diag(cm_train_predict_log_8gmm_case) / rowSums(cm_train_predict_log_8gmm_case))[2])
ROCRtrain_predict_log_8gmm_case = prediction(train_predict_log_8gmm_case, case_train_8gmm$case_status)
as.numeric(performance(ROCRtrain_predict_log_8gmm_case, "auc")@y.values)





###-----------------------------------------------------------------------
## Model: cov + blood counts 
case_train_bc <- df_train_7km[, c(4:19, 23,32)]
case_test_bc <- df_test_7km[,c(4:19, 23,32)]

case_train_bc$Smoking_status <- factor(case_train_bc$Smoking_status)
case_test_bc$Smoking_status <- factor(case_test_bc$Smoking_status)

case_train_bc$Smoking_status <- relevel(case_train_bc$Smoking_status, ref="Never")
case_test_bc$Smoking_status <- relevel(case_test_bc$Smoking_status, ref="Never")

log_bc_case =glm(case_status ~ ., data=case_train_bc, family=binomial)
summary(log_bc_case)
plot_log_bc_case <- plot_model(log_bc_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Blood Counts Model: Odds Ratios",ci_method=wald)
plot_log_bc_case

# Calculate the odds ratio
exp(coef(log_bc_case))
exp(cbind(coef(log_bc_case), confint(log_bc_case)))

# predict on the test set
predict_log_bc_case = predict(log_bc_case, type="response", newdata=case_test_bc)
cm_log_bc_case = table(case_test_bc$case_status, predict_log_bc_case > 0.5)
print(cm_log_bc_base)

# Recall
print((diag(cm_log_bc_case) / colSums(cm_log_bc_case))[2])

# Precision 
print((diag(cm_log_bc_case) / rowSums(cm_log_bc_case))[2])
ROCRpred_log_bc_case = prediction(predict_log_bc_case, case_test_bc$case_status)
as.numeric(performance(ROCRpred_log_bc_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_bc_case <- performance(ROCRpred_log_bc_case, "tpr","fpr")
plot(perf_log_bc_case ,colorize=TRUE,main = "ROC Curve using Blood Counts")
performance(ROCRpred_log_bc_case,"prec","rec")

# training model - bc 
train_predict_log_bc_case = predict(log_bc_case, type="response", newdata=case_train_bc)
cm_train_predict_log_bc_case = table(case_train_bc$case_status, train_predict_log_bc_case > 0.5)
# Recall
print((diag(cm_train_predict_log_bc_case) / colSums(cm_train_predict_log_bc_case))[2])
# Precision 
print((diag(cm_train_predict_log_bc_case) / rowSums(cm_train_predict_log_bc_case))[2])
ROCRtrain_predict_log_bc_case = prediction(train_predict_log_bc_case, case_train_bc$case_status)
as.numeric(performance(ROCRtrain_predict_log_bc_case, "auc")@y.values)

#----------------------- Complicated model
case_train_comp <- df_train_3km[, c(4:6,20:29)]
case_test_comp <- df_test_3km[,c(4:6,20:29)]

case_train_comp$Smoking_status <- factor(case_train_comp$Smoking_status)
case_test_comp$Smoking_status <- factor(case_test_comp$Smoking_status)
case_train_comp$Smoking_status <- relevel(case_train_comp$Smoking_status, ref="Never")
case_test_comp$Smoking_status <- relevel(case_test_comp$Smoking_status, ref="Never")

case_train_comp$Alcohol_status <- factor(case_train_comp$Alcohol_status )
case_test_comp$Alcohol_status  <- factor(case_test_comp$Alcohol_status )
case_train_comp$Alcohol_status  <- relevel(case_train_comp$Alcohol_status , ref="Never")
case_test_comp$Alcohol_status  <- relevel(case_test_comp$Alcohol_status, ref="Never")


log_comp_case =glm(case_status ~ ., data=case_train_comp, family=binomial)
summary(log_comp_case)
plot_log_comp_case <- plot_model(log_comp_case, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 40, title = "K-means Model with Risk Factors: Odds Ratios (k=7)",ci_method=wald)
plot_log_comp_case

# Calculate the odds ratio
exp(coef(log_comp_case))
exp(cbind(coef(log_comp_case), confint(log_comp_case)))

# predict on the test set
predict_log_comp_case = predict(log_comp_case, type="response", newdata=case_test_comp)
cm_log_comp_case = table(case_test_comp$case_status, predict_log_comp_case > 0.5)
print(cm_log_comp_case)

# Recall
print((diag(cm_log_comp_case) / colSums(cm_log_comp_case))[2])

# Precision 
print((diag(cm_log_comp_case) / rowSums(cm_log_comp_case))[2])
ROCRpred_log_comp_case = prediction(predict_log_comp_case, case_test_comp$case_status)
as.numeric(performance(ROCRpred_log_comp_case, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_comp_case <- performance(ROCRpred_log_comp_case, "tpr","fpr")
plot(perf_log_comp_case ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Blood Cancer(k=7)")
performance(ROCRpred_log_comp_case,"prec","rec")

#training set - complicated 
train_predict_log_comp_case = predict(log_comp_case, type="response", newdata=case_train_comp)
cm_train_predict_log_comp_case = table(case_train_comp$case_status, train_predict_log_comp_case > 0.5)
print(cm_train_predict_log_comp_case)
# Recall
print((diag(cm_train_predict_log_comp_case) / colSums(cm_train_predict_log_comp_case))[2])
# Precision 
print((diag(cm_train_predict_log_comp_case) / rowSums(cm_train_predict_log_comp_case))[2])
ROCRtrain_predict_log_comp_case = prediction(train_predict_log_comp_case, case_train_comp$case_status)
as.numeric(performance(ROCRtrain_predict_log_comp_case, "auc")@y.values)

# ---------------PLOTS SECTION 
# plotting all AUC 
pdf("AUC_plots_log_case_325.pdf")
plot(perf_log_cov_case ,colorize=TRUE,main = "Cov Model: ROC Curve for Blood Cancer")
plot(perf_log_km3_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=3)")
plot(perf_log_km6_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=6)")
plot(perf_log_km7_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=7)")
plot(perf_log_gmm_case ,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=2)")
plot(perf_log_8gmm_case,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=8)")
plot(perf_log_bc_case ,colorize=TRUE,main = "ROC Curve using Blood Counts")
plot(perf_log_comp_case ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Blood Cancer(k=7)")
dev.off()

# plotting all odds ratio 
pdf("log_case_325.pdf")
plot_log_cov_case
plot_log_km3_case
plot_log_km6_case
plot_log_km7_case
plot_log_gmm_case
plot_log_8gmm_case
plot_log_bc_case
plot_log_comp_case
dev.off()
plot(perf_log_cov_case, colorize=TRUE, main="ROC Curves for Blood Cancer")

# Add the other ROC curves to the plot
plot(perf_log_km3_case, colorize=TRUE, add=TRUE)
plot(perf_log_km6_case, colorize=TRUE, add=TRUE)
plot(perf_log_km7_case, colorize=TRUE, add=TRUE)
plot(perf_log_gmm_case, colorize=TRUE, add=TRUE)
plot(perf_log_8gmm_case, colorize=TRUE, add=TRUE)
plot(perf_log_bc_case, colorize=TRUE, add=TRUE)
plot(perf_log_comp_case, colorize=TRUE, add=TRUE)


library(pROC)

# Calculate ROC curves for each model
roc_cov <- roc(cov_labels, cov_preds, levels=c("Negative", "Positive"), direction=">")
roc_km3 <- roc(km3_labels, km3_preds, levels=c("Negative", "Positive"), direction=">")
roc_km6 <- roc(km6_labels, km6_preds, levels=c("Negative", "Positive"), direction=">")
roc_km7 <- roc(km7_labels, km7_preds, levels=c("Negative", "Positive"), direction=">")
roc_gmm <- roc(gmm_labels, gmm_preds, levels=c("Negative", "Positive"), direction=">")
roc_8gmm <- roc(gmm8_labels, gmm8_preds, levels=c("Negative", "Positive"), direction=">")
roc_bc <- roc(bc_labels, bc_preds, levels=c("Negative", "Positive"), direction=">")
roc_comp_k7 <- roc(comp_k7_labels, comp_k7_preds, levels=c("Negative", "Positive"), direction=">")



auc1 <- roc(case_test_cov$case_status, predict_log_cov_case)
auc2 <- roc(case_test_km3$case_status, predict_log_km3_case)
auc3 <- roc(case_test_km6$case_status, predict_log_km6_case)
auc4 <- roc(case_test_km7$case_status, predict_log_km7_case)
auc5 <- roc(case_test_8gmm$case_status, predict_log_8gmm_case)
auc6 <- roc(case_test_bc$case_status, predict_log_bc_case)
auc7 <- roc(case_test_comp$case_status, predict_log_comp_case)

pdf("combined.pdf")
plot(auc1, col = "blue")
lines(auc2, col = "red")
lines(auc3, col = "green")
lines(auc4, col = "black")
lines(auc5, col = "pink")
lines(auc6, col = "grey")
lines(auc7, col = "brown")
legend ("bottomright",legend = c("cov model - AUC = 0.71", 
                                 "kmean=3 model - AUC = 0.58", 
                                 "kmean=6 model - AUC = 0.61",
                                 "kmean=7 model - AUC = 0.61",
                                 "gmm=8 model - AUC = 0.53",
                                 "blood count model - AUC = 0.66",
                                 "cov + km3 + risk factor model - AUC = 0.70"),
        col = c("blue", "red", "green","black","pink","grey","brown"), lty = 1,cex = 0.5)
dev.off()


# ---------------
# plotting all AUC 
pdf("AUC_plots_log_case_325.pdf")
plot(perf_log_cov_case ,colorize=TRUE,main = "Cov Model: ROC Curve for Blood Cancer")
plot(perf_log_km3_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=3)")
plot(perf_log_km6_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=6)")
plot(perf_log_km7_case ,colorize=TRUE,main = "K-means Model:ROC Curve for Blood Cancer(k=7)")
plot(perf_log_gmm_case ,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=2)")
plot(perf_log_8gmm_case,colorize=TRUE,main = "GMM model: ROC Curve for Blood Cancer (k=8)")
plot(perf_log_bc_case ,colorize=TRUE,main = "ROC Curve using Blood Counts")
plot(perf_log_comp_case ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Blood Cancer(k=7)")
dev.off()

# ---------------- SUBGROUP ANALYSIS
# loading dataset 
setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
df_train_case = read.csv("matching/matched_df.csv")
df_test_case = read.csv("imp_test/test_imputed_df.csv")
train_df_3km = read.csv("Models/train_df_3kmeans.csv")
train_df_6km = read.csv("Models/train_df_6kmeans.csv")
train_df_7km = read.csv("Models/train_df_7kmeans.csv")
train_df_gmm = read.csv("Models/train_df_gmm.csv")
test_df_3km = read.csv("Models/test_df_3kmeans.csv")
test_df_6km = read.csv("Models/test_df_6kmeans.csv")
test_df_7km = read.csv("Models/test_df_7kmeans.csv")
test_df_gmm = read.csv("Models/test_df_gmm.csv")
test_df_8gmm = read.csv("Models/test_df_8gmm.csv")
train_df_8gmm = read.csv("Models/train_df_8gmm.csv")


# creating a full dataframe for training set 
train_df_3km$X <- rownames(train_df_3km)
train_df_6km$X <- rownames(train_df_6km)
train_df_7km$X <- rownames(train_df_7km)
df_train_case <- df_train_case[, c(1, 26, 27, 28)]
df_train_3km <- merge(df_train_case, train_df_3km, by = "X")
df_train_6km <- merge(df_train_case, train_df_6km, by = "X")
df_train_7km <- merge(df_train_case, train_df_7km, by = "X")

train_df_gmm$X <- rownames(train_df_gmm)
df_train_gmm <- merge(df_train_case, train_df_gmm, by = "X")

train_df_8gmm$X <- rownames(train_df_8gmm)
df_train_8gmm <- merge(df_train_case, train_df_8gmm, by = "X")

# creating a full dataframe for test set 
df_test_case <- df_test_case[, c(1, 25, 26, 27)]
df_test_3km <- merge(df_test_case, test_df_3km, by = "X")
df_test_6km <- merge(df_test_case, test_df_6km, by = "X")
df_test_7km <- merge(df_test_case, test_df_7km, by = "X")
df_test_gmm <- merge(df_test_case, test_df_gmm, by = "X")
df_test_8gmm <- merge(df_test_case, test_df_8gmm, by = "X")

# creating leukemia df 
leuk_km3_train <- subset(df_train_3km, case_leuk == 1 | case_status == 0) #km=6
leuk_km3_test <- subset(df_test_3km, case_leuk == 1 | case_status == 0)

leuk_km6_train <- subset(df_train_6km, case_leuk == 1 | case_status == 0) #km=6
leuk_km6_test <- subset(df_test_6km, case_leuk == 1 | case_status == 0)

leuk_km7_train <- subset(df_train_7km, case_leuk == 1 | case_status == 0) #km=7
leuk_km7_test <- subset(df_test_7km, case_leuk == 1 | case_status == 0)

leuk_gmm_train <- subset(df_train_gmm, case_leuk == 1 | case_status == 0) #gmm 
leuk_gmm_test <- subset(df_test_gmm, case_leuk == 1 | case_status == 0)

leuk_gmm8_train <- subset(df_train_8gmm, case_leuk == 1 | case_status == 0) #gmm=8
leuk_gmm8_test <- subset(df_test_8gmm, case_leuk == 1 | case_status == 0)

# renaming sex 1 column
leuk_km3_train <- leuk_km3_train %>% rename(Sex1 = Sex.0.0_1)
leuk_km3_test <- leuk_km3_test %>% rename(Sex1 = Sex.0.0_1)

leuk_km6_train <- leuk_km6_train %>% rename(Sex1 = Sex.0.0_1)
leuk_km6_test <- leuk_km6_test %>% rename(Sex1 = Sex.0.0_1)

leuk_km7_train <- leuk_km7_train %>% rename(Sex1 = Sex.0.0_1)
leuk_km7_test <- leuk_km7_test %>% rename(Sex1 = Sex.0.0_1)

leuk_gmm_train <- leuk_gmm_train %>% rename(Sex1 = Sex.0.0_1)
leuk_gmm_test <- leuk_gmm_test %>% rename(Sex1 = Sex.0.0_1)

leuk_gmm8_train <- leuk_gmm8_train %>% rename(Sex1 = Sex.0.0_1)
leuk_gmm8_test <- leuk_gmm8_test %>% rename(Sex1 = Sex.0.0_1)


# renaming whether pts having mood swing
leuk_km3_train <- leuk_km3_train %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_km3_test <- leuk_km3_test %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_km6_train <- leuk_km6_train %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_km6_test <- leuk_km6_test %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_km7_train <- leuk_km7_train %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_km7_test <- leuk_km7_test %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_gmm_train <- leuk_gmm_train %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_gmm_test <- leuk_gmm_test %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_gmm8_train <- leuk_gmm8_train %>% rename(Mood_swings = Mood_swings.0.0_1)
leuk_gmm8_test <- leuk_gmm8_test %>% rename(Mood_swings = Mood_swings.0.0_1)

# rename hard-coded columns to categorical col - smoking / drinking 
leuk_km3_train$Alcohol_status <- ifelse(leuk_km3_train$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(leuk_km3_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km3_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km3_train$Smoking_status <- ifelse(leuk_km3_train$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(leuk_km3_train$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km3_train$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_km3_test$Alcohol_status <- ifelse(leuk_km3_test$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_km3_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km3_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km3_test$Smoking_status <- ifelse(leuk_km3_test$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_km3_test$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km3_test$Smoking_status.0.0_0 == 1, "Never", NA)))

leuk_km6_train$Alcohol_status <- ifelse(leuk_km6_train$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(leuk_km6_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km6_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km6_train$Smoking_status <- ifelse(leuk_km6_train$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(leuk_km6_train$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km6_train$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_km6_test$Alcohol_status <- ifelse(leuk_km6_test$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_km6_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km6_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km6_test$Smoking_status <- ifelse(leuk_km6_test$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_km6_test$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km6_test$Smoking_status.0.0_0 == 1, "Never", NA)))

leuk_km7_train$Alcohol_status <- ifelse(leuk_km7_train$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(leuk_km7_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km7_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km7_train$Smoking_status <- ifelse(leuk_km7_train$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(leuk_km7_train$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(leuk_km7_train$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_km7_test$Alcohol_status <- ifelse(leuk_km7_test$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_km7_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km7_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km7_test$Smoking_status <- ifelse(leuk_km7_test$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_km7_test$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km7_test$Smoking_status.0.0_0 == 1, "Never", NA)))

leuk_gmm_test$Alcohol_status <- ifelse(leuk_gmm_test$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_gmm_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_gmm_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_gmm_test$Smoking_status <- ifelse(leuk_gmm_test$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_gmm_test$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_gmm_test$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_gmm8_train$Alcohol_status <- ifelse(leuk_gmm8_train$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(leuk_gmm8_train$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
leuk_gmm8_train$Smoking_status <- ifelse(leuk_gmm8_train$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(leuk_gmm8_train$Smoking_status.0.0_2 == 1, "Current", "Never"))
leuk_gmm8_test$Alcohol_status <- ifelse(leuk_gmm8_test$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(leuk_gmm8_train$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
leuk_gmm8_test$Smoking_status <- ifelse(leuk_gmm8_test$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(leuk_gmm8_train$Smoking_status.0.0_2 == 1, "Current", "Never"))

leuk_km3_train <- leuk_km3_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_km3_test <- leuk_km3_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                          Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                          RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                          Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                          Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                          C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_km6_train <- leuk_km6_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_km6_test <- leuk_km6_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                          Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                          RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                          Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                          Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                          C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_km7_train <- leuk_km7_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_km7_test <- leuk_km7_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                          Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                          RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                          Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                          Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                          C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_gmm_train <- leuk_gmm_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_gmm_test <- leuk_gmm_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                          Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                          RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                          Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                          Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                          C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_gmm8_train <- leuk_gmm8_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

leuk_gmm8_test <- leuk_gmm8_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

# Renaming other columns for clear view
leuk_km3_test <- select(leuk_km3_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km3_train <- select(leuk_km3_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km6_test <- select(leuk_km6_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km6_train <- select(leuk_km6_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km7_test <- select(leuk_km7_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km7_train <- select(leuk_km7_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_gmm_test <- select(leuk_gmm_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_gmm_train <- select(leuk_gmm_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_gmm8_test <- select(leuk_gmm8_test,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)
leuk_gmm8_train <- select(leuk_gmm8_train,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)



## lymphoma dataframe cleaning 
# creating lymphoma df 
lymph_km3_train <- subset(df_train_3km, case_lymph == 1 | case_status == 0) #km=3
lymph_km3_test <- subset(df_test_3km, case_lymph == 1 | case_status == 0)
lymph_km6_train <- subset(df_train_6km, case_lymph == 1 | case_status == 0) #km=6
lymph_km6_test <- subset(df_test_6km, case_lymph == 1 | case_status == 0)
lymph_km7_train <- subset(df_train_7km, case_lymph == 1 | case_status == 0) #km=7
lymph_km7_test <- subset(df_test_7km, case_lymph == 1 | case_status == 0)
lymph_gmm_train <- subset(df_train_gmm, case_lymph == 1 | case_status == 0) #gmm 
lymph_gmm_test <- subset(df_test_gmm, case_lymph == 1 | case_status == 0)
lymph_gmm8_train <- subset(df_train_8gmm, case_lymph == 1 | case_status == 0) #gmm=8
lymph_gmm8_test <- subset(df_test_8gmm, case_lymph == 1 | case_status == 0)

# renaming sex and mood swing col
lymph_km3_train <- lymph_km3_train %>% rename(Sex1 = Sex.0.0_1)
lymph_km3_test <- lymph_km3_test %>% rename(Sex1 = Sex.0.0_1)
lymph_km6_train <- lymph_km6_train %>% rename(Sex1 = Sex.0.0_1)
lymph_km6_test <- lymph_km6_test %>% rename(Sex1 = Sex.0.0_1)
lymph_km7_train <- lymph_km7_train %>% rename(Sex1 = Sex.0.0_1)
lymph_km7_test <- lymph_km7_test %>% rename(Sex1 = Sex.0.0_1)
lymph_gmm_train <- lymph_gmm_train %>% rename(Sex1 = Sex.0.0_1)
lymph_gmm_test <- lymph_gmm_test %>% rename(Sex1 = Sex.0.0_1)
lymph_gmm8_train <- lymph_gmm8_train %>% rename(Sex1 = Sex.0.0_1)
lymph_gmm8_test <- lymph_gmm8_test %>% rename(Sex1 = Sex.0.0_1)

lymph_km3_train <- lymph_km3_train %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_km3_test <- lymph_km3_test %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_km6_train <- lymph_km6_train %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_km6_test <- lymph_km6_test %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_km7_train <- lymph_km7_train %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_km7_test <- lymph_km7_test %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_gmm_train <- lymph_gmm_train %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_gmm_test <- lymph_gmm_test %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_gmm8_train <- lymph_gmm8_train %>% rename(Mood_swings = Mood_swings.0.0_1)
lymph_gmm8_test <- lymph_gmm8_test %>% rename(Mood_swings = Mood_swings.0.0_1)

# rename hard-coded columns to categorical col - smoking / drinking 
lymph_km3_train$Alcohol_status <- ifelse(lymph_km3_train$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(lymph_km3_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km3_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km3_train$Smoking_status <- ifelse(lymph_km3_train$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(lymph_km3_train$Smoking_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km3_train$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_km3_test$Alcohol_status <- ifelse(lymph_km3_test$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(lymph_km3_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km3_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km3_test$Smoking_status <- ifelse(lymph_km3_test$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(lymph_km3_test$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km3_test$Smoking_status.0.0_0 == 1, "Never", NA)))

lymph_km6_train$Alcohol_status <- ifelse(lymph_km6_train$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(lymph_km6_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km6_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km6_train$Smoking_status <- ifelse(lymph_km6_train$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(lymph_km6_train$Smoking_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km6_train$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_km6_test$Alcohol_status <- ifelse(lymph_km6_test$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(lymph_km6_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km6_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km6_test$Smoking_status <- ifelse(lymph_km6_test$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(lymph_km6_test$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km6_test$Smoking_status.0.0_0 == 1, "Never", NA)))

lymph_km7_train$Alcohol_status <- ifelse(lymph_km7_train$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(lymph_km7_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km7_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km7_train$Smoking_status <- ifelse(lymph_km7_train$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(lymph_km7_train$Smoking_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_km7_train$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_km7_test$Alcohol_status <- ifelse(lymph_km7_test$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(lymph_km7_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km7_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km7_test$Smoking_status <- ifelse(lymph_km7_test$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(lymph_km7_test$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_km7_test$Smoking_status.0.0_0 == 1, "Never", NA)))


lymph_gmm_test$Alcohol_status <- ifelse(lymph_gmm_test$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(lymph_gmm_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_gmm_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_gmm_test$Smoking_status <- ifelse(lymph_gmm_test$Smoking_status.0.0_1 == 1, "Past",
                                        ifelse(lymph_gmm_test$Smoking_status.0.0_2 == 1, "Current",
                                               ifelse(lymph_gmm_test$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_gmm_train$Alcohol_status <- ifelse(lymph_gmm_train$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(lymph_gmm_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_gmm_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_gmm_train$Smoking_status <- ifelse(lymph_gmm_train$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(lymph_gmm_train$Smoking_status.0.0_2 == 1, "Current",
                                                ifelse(lymph_gmm_train$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_gmm8_train$Alcohol_status <- ifelse(lymph_gmm8_train$Alc_drinker_status.0.0_1== 1, "Past",
                                          ifelse(lymph_gmm8_train$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
lymph_gmm8_train$Smoking_status <- ifelse(lymph_gmm8_train$Smoking_status.0.0_1 == 1, "Past",
                                          ifelse(lymph_gmm8_train$Smoking_status.0.0_2 == 1, "Current", "Never"))
lymph_gmm8_test$Alcohol_status <- ifelse(lymph_gmm8_test$Alc_drinker_status.0.0_1== 1, "Past",
                                         ifelse(lymph_gmm8_test$Alc_drinker_status.0.0_2 == 1, "Current","Never"))
lymph_gmm8_test$Smoking_status <- ifelse(lymph_gmm8_test$Smoking_status.0.0_1 == 1, "Past",
                                         ifelse(lymph_gmm8_test$Smoking_status.0.0_2 == 1, "Current", "Never"))

# Renaming for clear view
lymph_km3_train <- lymph_km3_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_km3_test <- lymph_km3_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_km6_train <- lymph_km6_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_km6_test <- lymph_km6_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_km7_train <- lymph_km7_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_km7_test <- lymph_km7_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_gmm_train <- lymph_gmm_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_gmm_test <- lymph_gmm_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                            Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                            RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                            Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                            Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                            C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_gmm8_train <- lymph_gmm8_train %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                                Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                                RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                                Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                                Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                                C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

lymph_gmm8_test <- lymph_gmm8_test %>% rename(BMI = BMI.0.0, Age = Age_recr.0.0, Lymphocyte_count = Lymphocyte_count.0.0,
                                              Monocyte_count = Monocyte_count.0.0, Reti_count = Reti_count.0.0, WBC_count = WBC_count.0.0,
                                              RBC_count = RBC_count.0.0, Hgb_conc = Hgb_conc.0.0, Haematocrit_percent = Haematocrit_perc.0.0,
                                              Platelet_count = Platelet_count.0.0, Basophil_count = Basophil_count.0.0, Eosinophil_count = Eosinophil_count.0.0,
                                              Neutrophil_count = Neutrophil_count.0.0, Immature_ret_fraction = Immature_ret_fraction.0.0, High_light_scatter_reti_count = High_light_scatter_reti_count.0.0,
                                              C_reactive_protein = C_reactive_protein.0.0, Traffic_intensity = Traff_int_major_rd.0.0, Housing_score = housing_score)

# Remove unnecessary varaibles 
lymph_km3_test <- select(lymph_km3_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km3_train <- select(lymph_km3_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km6_test <- select(lymph_km6_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km6_train <- select(lymph_km6_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km7_test <- select(lymph_km7_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km7_train <- select(lymph_km7_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_gmm_test <- select(lymph_gmm_test, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_gmm_train <- select(lymph_gmm_train, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_gmm8_test <- select(lymph_gmm8_test,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)
lymph_gmm8_train <- select(lymph_gmm8_train,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_2)

#-------------------------------------------------------------------------------
### Fundamental model - Covariates  (BMI,sex,age,smoking status)
## leukemia 
# creating subset for fundamental model 
leuk_train_cov <- leuk_km3_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status")]
leuk_test_cov <- leuk_km3_test[,c("case_leuk","BMI","Age","Sex1","Smoking_status")]

# change categorical to factors 
leuk_train_cov$Smoking_status <- factor(leuk_train_cov$Smoking_status)
leuk_test_cov$Smoking_status <- factor(leuk_test_cov$Smoking_status)

leuk_train_cov$Smoking_status <- relevel(leuk_train_cov$Smoking_status, ref="Never")
leuk_test_cov$Smoking_status <- relevel(leuk_test_cov$Smoking_status, ref="Never")

log_cov_leu = glm(case_leuk ~ ., data=leuk_train_cov, family=binomial)
summary(log_cov_leu)
plot_log_cov_leu <- plot_model(log_cov_leu, sort.est = TRUE, show.valuesvalue.offset = .3,
                               show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Covariate Model: Odds Ratios for Leukemia", ci_method=wald)
plot_log_cov_leu

# Calculate the odds ratio
exp(coef(log_cov_leu))
exp(cbind(coef(log_cov_leu), confint(log_cov_leu)))

# predict on the test set
predict_log_cov_leu = predict(log_cov_leu, type="response", newdata=leuk_test_cov)
cm_log_cov_leu = table(leuk_test_cov$case_leuk, predict_log_cov_leu > 0.5)
print(cm_log_cov_leu)

summary(predict_log_cov_leu)

# Recall
print((diag(cm_log_cov_leu) / colSums(cm_log_cov_leu))[2])

# Precision 
print((diag(cm_log_cov_leu) / rowSums(cm_log_cov_leu))[2])
ROCRpred_log_cov_leu = prediction(predict_log_cov_leu, leuk_test_cov$case_leuk)
as.numeric(performance(ROCRpred_log_cov_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_cov_leu <- performance(ROCRpred_log_cov_leu, "tpr","fpr")
plot(perf_log_cov_leu ,colorize=TRUE,main = "Cov Model: ROC Curve for Leukemia")
performance(ROCRpred_log_cov_leu,"prec","rec")

#TRAINING -leukemia 
train_predict_log_cov_leu = predict(log_cov_leu, type="response", newdata=leuk_train_cov)
cm_train_predict_log_cov_leu = table(leuk_train_cov$case_leuk, train_predict_log_cov_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_cov_leu) / colSums(cm_train_predict_log_cov_leu))[2])
# Precision 
print((diag(cm_train_predict_log_cov_leu) / rowSums(cm_train_predict_log_cov_leu))[2])
ROCRtrain_predict_log_cov_leu = prediction(train_predict_log_cov_leu, leuk_train_cov$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_cov_leu, "auc")@y.values)


## lymphoma
# creating subset for fundamental model 
lymph_train_cov <- lymph_km3_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status")]
lymph_test_cov <- lymph_km3_test[,c("case_lymph","BMI","Age","Sex1","Smoking_status")]


# change categorical to factors 
lymph_train_cov$Smoking_status <- factor(lymph_train_cov$Smoking_status)
lymph_test_cov$Smoking_status <- factor(lymph_test_cov$Smoking_status)

lymph_train_cov$Smoking_status <- relevel(lymph_train_cov$Smoking_status, ref="Never")
lymph_test_cov$Smoking_status <- relevel(lymph_test_cov$Smoking_status, ref="Never")

# Log model
log_cov_lymph = glm(case_lymph ~ ., data=lymph_train_cov, family=binomial)
summary(log_cov_lymph)
plot_log_cov_lymph <- plot_model(log_cov_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,
                                 show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Covariate Model: Odds Ratios for Lymphoma", ci_method=wald)
plot_log_cov_lymph

# Calculate the odds ratio
exp(coef(log_cov_lymph))
exp(cbind(coef(log_cov_lymph), confint(log_cov_lymph)))

# predict on the test set
predict_log_cov_lymph = predict(log_cov_lymph, type="response", newdata=lymph_test_cov)
cm_log_cov_lymph = table(lymph_test_cov$case_lymph, predict_log_cov_lymph > 0.5)
print(cm_log_cov_lymph)

# Recall
print((diag(cm_log_cov_lymph) / colSums(cm_log_cov_lymph))[2])

# Precision 
print((diag(cm_log_cov_lymph) / rowSums(cm_log_cov_lymph))[2])
ROCRpred_log_cov_lymph = prediction(predict_log_cov_lymph, lymph_test_cov$case_lymph)
as.numeric(performance(ROCRpred_log_cov_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_cov_lymph <- performance(ROCRpred_log_cov_lymph, "tpr","fpr")
plot(perf_log_cov_lymph ,colorize=TRUE,main = "Cov Model:ROC:Curve for Lymphoma")
performance(ROCRpred_log_cov_lymph,"prec","rec")

#TRAINING -lymphoma 
train_predict_log_cov_lymph = predict(log_cov_lymph, type="response", newdata=lymph_train_cov)
cm_train_predict_log_cov_lymph = table(lymph_train_cov$case_lymph, train_predict_log_cov_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_cov_lymph) / colSums(cm_train_predict_log_cov_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_cov_lymph) / rowSums(cm_train_predict_log_cov_lymph))[2])
ROCRtrain_predict_log_cov_lymph = prediction(train_predict_log_cov_lymph, lymph_train_cov$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_cov_lymph, "auc")@y.values)

# ---------------------------------------------------------------------------------
# K-means model Cluster membership + Covariates (BMI,sex,age,smoking status)
# K-means = 3
# leukemia 
# creating subset for K-means model 
leuk_train_km3 <- leuk_km3_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]
leuk_test_km3 <- leuk_km3_test[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]

# change categorical to factors 
leuk_train_km3$Smoking_status <- factor(leuk_train_km3$Smoking_status)
leuk_test_km3$Smoking_status <- factor(leuk_test_km3$Smoking_status)

leuk_train_km3$Smoking_status <- relevel(leuk_train_km3$Smoking_status, ref="Never")
leuk_test_km3$Smoking_status <- relevel(leuk_test_km3$Smoking_status, ref="Never")

#model
log_km3_leu =glm(case_leuk ~ ., data=leuk_train_km3, family=binomial)
summary(log_km3_leu)
plot_log_km3_leu <- plot_model(log_km3_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Leukemia (k=3)", ci_method=wald)
plot_log_km3_leu

# Calculate the odds ratio
exp(coef(log_km3_leu))
exp(cbind(coef(log_km3_leu), confint(log_km3_leu)))

# predict on the test set
predict_log_km3_leu = predict(log_km3_leu, type="response", newdata=leuk_test_km3)
cm_log_km3_leu = table(leuk_test_km3$case_leuk, predict_log_km3_leu > 0.5)
print(cm_log_km3_leu)

# Recall
print((diag(cm_log_km3_leu) / colSums(cm_log_km3_leu))[2])

# Precision 
print((diag(cm_log_km3_leu) / rowSums(cm_log_km3_leu))[2])

ROCRpred_log_km3_leu = prediction(predict_log_km3_leu, leuk_test_km3$case_leuk)
as.numeric(performance(ROCRpred_log_km3_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km3_leu <- performance(ROCRpred_log_km3_leu, "tpr","fpr")
plot(perf_log_km3_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=3)")
performance(ROCRpred_log_km3_leu,"prec","rec")

# Training set - leukemia 
train_predict_log_km3_leu = predict(log_km3_leu, type="response", newdata=leuk_train_km3)
cm_train_predict_log_km3_leu = table(leuk_train_km3$case_leuk, train_predict_log_km3_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_km3_leu) / colSums(cm_train_predict_log_km3_leu))[2])
# Precision 
print((diag(cm_train_predict_log_km3_leu) / rowSums(cm_train_predict_log_km3_leu))[2])
ROCRtrain_predict_log_km3_leu = prediction(train_predict_log_km3_leu, leuk_train_km3$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_km3_leu, "auc")@y.values)

# lymphoma
# creating subset for K-means model - because we do not have cluster 2 
lymph_train_km3 <- lymph_km3_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]
lymph_test_km3 <- lymph_km3_test[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1")]

# change categorical to factors 
lymph_train_km3$Smoking_status <- factor(lymph_train_km3$Smoking_status)
lymph_test_km3$Smoking_status <- factor(lymph_test_km3$Smoking_status)

lymph_train_km3$Smoking_status <- relevel(lymph_train_km3$Smoking_status, ref="Never")
lymph_test_km3$Smoking_status <- relevel(lymph_test_km3$Smoking_status, ref="Never")


log_km3_lymph =glm(case_lymph ~ ., data=lymph_train_km3, family=binomial)
summary(log_km3_lymph)
plot_log_km3_lymph <- plot_model(log_km3_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Lymphoma (k=3)",ci_method=wald)
plot_log_km3_lymph

# Calculate the odds ratio
exp(coef(log_km3_lymph))
exp(cbind(coef(log_km3_lymph), confint(log_km3_lymph)))

# predict on the test set
predict_log_km3_lymph = predict(log_km3_lymph, type="response", newdata=lymph_test_km3)
cm_log_km3_lymph = table(lymph_test_km3$case_lymph, predict_log_km3_lymph > 0.5)
print(cm_log_km3_lymph)

# Recall
print((diag(cm_log_km3_lymph) / colSums(cm_log_km3_lymph))[2])

# Precision 
print((diag(cm_log_km3_lymph) / rowSums(cm_log_km3_lymph))[2])
ROCRpred_log_km3_lymph = prediction(predict_log_km3_lymph, lymph_test_km3$case_lymph)
as.numeric(performance(ROCRpred_log_km3_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km3_lymph <- performance(ROCRpred_log_km3_lymph, "tpr","fpr")
plot(perf_log_km3_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=3)")
performance(ROCRpred_log_km3_lymph,"prec","rec")

# Training set 
train_predict_log_km3_lymph = predict(log_km3_lymph, type="response", newdata=lymph_train_km3)
cm_train_predict_log_km3_lymph = table(lymph_train_km3$case_lymph, train_predict_log_km3_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_km3_lymph) / colSums(cm_train_predict_log_km3_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_km3_lymph) / rowSums(cm_train_predict_log_km3_lymph))[2])
ROCRtrain_predict_log_km3_lymph = prediction(train_predict_log_km3_lymph, lymph_train_km3$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_km3_lymph, "auc")@y.values)


# ---------------------------------------------------------------------------------
# K-means = 6
# leukemia 
# creating subset for K-means model 
leuk_train_km6 <- leuk_km6_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5")]
leuk_test_km6 <- leuk_km6_test[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5")]

# change categorical to factors 
leuk_train_km6$Smoking_status <- factor(leuk_train_km6$Smoking_status)
leuk_test_km6$Smoking_status <- factor(leuk_test_km6$Smoking_status)
leuk_train_km6$Smoking_status <- relevel(leuk_train_km6$Smoking_status, ref="Never")
leuk_test_km6$Smoking_status <- relevel(leuk_test_km6$Smoking_status, ref="Never")

#model
log_km6_leu =glm(case_leuk ~ ., data=leuk_train_km6, family=binomial)
summary(log_km6_leu)
plot_log_km6_leu <- plot_model(log_km6_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Leukemia (k=6)", ci_method=wald)
plot_log_km6_leu

# Calculate the odds ratio
exp(coef(log_km6_leu))
exp(cbind(coef(log_km6_leu), confint(log_km6_leu)))

# predict on the test set
predict_log_km6_leu = predict(log_km6_leu, type="response", newdata=leuk_test_km6)
cm_log_km6_leu = table(leuk_test_km6$case_leuk, predict_log_km6_leu > 0.5)
print(cm_log_km6_leu)

# Recall
print((diag(cm_log_km6_leu) / colSums(cm_log_km6_leu))[2])

# Precision 
print((diag(cm_log_km6_leu) / rowSums(cm_log_km6_leu))[2])

ROCRpred_log_km6_leu = prediction(predict_log_km6_leu, leuk_test_km6$case_leuk)
as.numeric(performance(ROCRpred_log_km6_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km6_leu <- performance(ROCRpred_log_km6_leu, "tpr","fpr")
plot(perf_log_km6_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=6)")
performance(ROCRpred_log_km6_leu,"prec","rec")

# training - leuk
train_predict_log_km6_leu = predict(log_km6_leu, type="response", newdata=leuk_train_km6)
cm_train_predict_log_km6_leu = table(leuk_train_km6$case_leuk, train_predict_log_km6_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_km6_leu) / colSums(cm_train_predict_log_km6_leu))[2])
# Precision 
print((diag(cm_train_predict_log_km6_leu) / rowSums(cm_train_predict_log_km6_leu))[2])
ROCRtrain_predict_log_km6_leu = prediction(train_predict_log_km6_leu, leuk_train_km6$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_km6_leu, "auc")@y.values)


# lymphoma
# creating subset for K-means model - because we do not have cluster 2 
lymph_train_km6 <- lymph_km6_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]
lymph_test_km6 <- lymph_km6_test[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]

# change categorical to factors 
lymph_train_km6$Smoking_status <- factor(lymph_train_km6$Smoking_status)
lymph_test_km6$Smoking_status <- factor(lymph_test_km6$Smoking_status)

lymph_train_km6$Smoking_status <- relevel(lymph_train_km6$Smoking_status, ref="Never")
lymph_test_km6$Smoking_status <- relevel(lymph_test_km6$Smoking_status, ref="Never")


log_km6_lymph =glm(case_lymph ~ ., data=lymph_train_km6, family=binomial)
summary(log_km6_lymph)
plot_log_km6_lymph <- plot_model(log_km6_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Lymphoma (k=6)",ci_method=wald)
plot_log_km6_lymph

# Calculate the odds ratio
exp(coef(log_km6_lymph))
exp(cbind(coef(log_km6_lymph), confint(log_km6_lymph)))

# predict on the test set
predict_log_km6_lymph = predict(log_km6_lymph, type="response", newdata=lymph_test_km6)
cm_log_km6_lymph = table(lymph_test_km6$case_lymph, predict_log_km6_lymph > 0.5)
print(cm_log_km6_lymph)

# Recall
print((diag(cm_log_km6_lymph) / colSums(cm_log_km6_lymph))[2])

# Precision 
print((diag(cm_log_km6_lymph) / rowSums(cm_log_km6_lymph))[2])
ROCRpred_log_km6_lymph = prediction(predict_log_km6_lymph, lymph_test_km6$case_lymph)
as.numeric(performance(ROCRpred_log_km6_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km6_lymph <- performance(ROCRpred_log_km6_lymph, "tpr","fpr")
plot(perf_log_km6_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=6)")
performance(ROCRpred_log_km6_lymph,"prec","rec")

# Training set 
train_predict_log_km6_lymph = predict(log_km6_lymph, type="response", newdata=lymph_train_km6)
cm_train_predict_log_km6_lymph = table(lymph_train_km6$case_lymph, train_predict_log_km6_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_km6_lymph) / colSums(cm_train_predict_log_km6_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_km6_lymph) / rowSums(cm_train_predict_log_km6_lymph))[2])
ROCRtrain_predict_log_km6_lymph = prediction(train_predict_log_km6_lymph, lymph_train_km6$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_km6_lymph, "auc")@y.values)

# ---------------------------------------------------------------------------------
# K-means = 7
# leukemia 
# creating subset for K-means model 
leuk_train_km7 <- leuk_km7_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5","cluster_kmeans_6")]
leuk_test_km7 <- leuk_km7_test[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5","cluster_kmeans_6")]

# change categorical to factors 
leuk_train_km7$Smoking_status <- factor(leuk_train_km7$Smoking_status)
leuk_test_km7$Smoking_status <- factor(leuk_test_km7$Smoking_status)
leuk_train_km7$Smoking_status <- relevel(leuk_train_km7$Smoking_status, ref="Never")
leuk_test_km7$Smoking_status <- relevel(leuk_test_km7$Smoking_status, ref="Never")

#model
log_km7_leu =glm(case_leuk ~ ., data=leuk_train_km7, family=binomial)
summary(log_km7_leu)
plot_log_km7_leu <- plot_model(log_km7_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Leukemia (k=7)", ci_method=wald)
plot_log_km7_leu

# Calculate the odds ratio
exp(coef(log_km7_leu))
exp(cbind(coef(log_km7_leu), confint(log_km7_leu)))

# predict on the test set
predict_log_km7_leu = predict(log_km7_leu, type="response", newdata=leuk_test_km7)
cm_log_km7_leu = table(leuk_test_km7$case_leuk, predict_log_km7_leu > 0.5)
print(cm_log_km7_leu)

# Recall
print((diag(cm_log_km7_leu) / colSums(cm_log_km7_leu))[2])

# Precision 
print((diag(cm_log_km7_leu) / rowSums(cm_log_km7_leu))[2])

ROCRpred_log_km7_leu = prediction(predict_log_km7_leu, leuk_test_km7$case_leuk)
as.numeric(performance(ROCRpred_log_km7_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km7_leu <- performance(ROCRpred_log_km7_leu, "tpr","fpr")
plot(perf_log_km7_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=7)")
performance(ROCRpred_log_km7_leu,"prec","rec")

# Training set - leukemia 
train_predict_log_km7_leu = predict(log_km7_leu, type="response", newdata=leuk_train_km7)
cm_train_predict_log_km7_leu = table(leuk_train_km7$case_leuk, train_predict_log_km7_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_km7_leu) / colSums(cm_train_predict_log_km7_leu))[2])
# Precision 
print((diag(cm_train_predict_log_km7_leu) / rowSums(cm_train_predict_log_km7_leu))[2])
ROCRtrain_predict_log_km7_leu = prediction(train_predict_log_km7_leu, leuk_train_km7$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_km7_leu, "auc")@y.values)

# lymphoma
# creating subset for K-means model - because we do not have cluster 2 
lymph_train_km7 <- lymph_km7_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5","cluster_kmeans_6")]
lymph_test_km7 <- lymph_km7_test[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4","cluster_kmeans_5","cluster_kmeans_6")]

# change categorical to factors 
lymph_train_km7$Smoking_status <- factor(lymph_train_km7$Smoking_status)
lymph_test_km7$Smoking_status <- factor(lymph_test_km7$Smoking_status)

lymph_train_km7$Smoking_status <- relevel(lymph_train_km7$Smoking_status, ref="Never")
lymph_test_km7$Smoking_status <- relevel(lymph_test_km7$Smoking_status, ref="Never")


log_km7_lymph =glm(case_lymph ~ ., data=lymph_train_km7, family=binomial)
summary(log_km7_lymph)
plot_log_km7_lymph <- plot_model(log_km7_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "K-means Model: Odds Ratios for Lymphoma (k=7)",ci_method=wald)
plot_log_km7_lymph

# Calculate the odds ratio
exp(coef(log_km7_lymph))
exp(cbind(coef(log_km7_lymph), confint(log_km7_lymph)))

# predict on the test set
predict_log_km7_lymph = predict(log_km7_lymph, type="response", newdata=lymph_test_km7)
cm_log_km7_lymph = table(lymph_test_km7$case_lymph, predict_log_km7_lymph > 0.5)
print(cm_log_km7_lymph)

# Recall
print((diag(cm_log_km7_lymph) / colSums(cm_log_km7_lymph))[2])

# Precision 
print((diag(cm_log_km7_lymph) / rowSums(cm_log_km7_lymph))[2])
ROCRpred_log_km7_lymph = prediction(predict_log_km7_lymph, lymph_test_km7$case_lymph)
as.numeric(performance(ROCRpred_log_km7_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km7_lymph <- performance(ROCRpred_log_km7_lymph, "tpr","fpr")
plot(perf_log_km7_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=7)")
performance(ROCRpred_log_km7_lymph,"prec","rec")


# Training set 
train_predict_log_km7_lymph = predict(log_km7_lymph, type="response", newdata=lymph_train_km7)
cm_train_predict_log_km7_lymph = table(lymph_train_km7$case_lymph, train_predict_log_km7_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_km7_lymph) / colSums(cm_train_predict_log_km7_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_km7_lymph) / rowSums(cm_train_predict_log_km7_lymph))[2])
ROCRtrain_predict_log_km7_lymph = prediction(train_predict_log_km7_lymph, lymph_train_km7$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_km7_lymph, "auc")@y.values)

# ---------------
## GMM model: Cluster membership + Covariates (BMI,sex,age,smoking status)
# leukemia 
# creating subset for gmm model 
leuk_train_gmm <- leuk_gmm_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]
leuk_test_gmm <- leuk_gmm_test[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]

# change categorical to factors 
leuk_train_gmm$Smoking_status <- factor(leuk_train_gmm$Smoking_status)
leuk_test_gmm$Smoking_status <- factor(leuk_test_gmm$Smoking_status)

leuk_train_gmm$Smoking_status <- relevel(leuk_train_gmm$Smoking_status, ref="Never")
leuk_test_gmm$Smoking_status <- relevel(leuk_test_gmm$Smoking_status, ref="Never")

log_gmm_leu =glm(case_leuk ~ ., data=leuk_train_gmm, family=binomial)
plot_log_gmm_leu <- plot_model(log_gmm_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "GMM Model: Odds Ratios for Leukemia (k=2)",ci_method=wald)
plot_log_gmm_leu

# Calculate the odds ratio
exp(coef(log_gmm_leu))
exp(cbind(coef(log_gmm_leu), confint(log_gmm_leu)))

# predict on the test set
predict_log_gmm_leu = predict(log_gmm_leu, type="response", newdata=leuk_test_gmm)
cm_log_gmm_leu = table(leuk_test_gmm$case_leuk, predict_log_gmm_leu > 0.5)
print(cm_log_gmm_leu)

# Recall
print((diag(cm_log_gmm_leu) / colSums(cm_log_gmm_leu))[2])

# Precision 
print((diag(cm_log_gmm_leu) / rowSums(cm_log_gmm_leu))[2])
ROCRpred_log_gmm_leu = prediction(predict_log_gmm_leu, leuk_test_gmm$case_leuk)
as.numeric(performance(ROCRpred_log_gmm_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_gmm_leu <- performance(ROCRpred_log_gmm_leu, "tpr","fpr")
plot(perf_log_gmm_leu ,colorize=TRUE,main = "GMM model: ROC Curve for Leukemia (k=2)")
performance(ROCRpred_log_gmm_leu,"prec","rec")


# training - leuk 
train_predict_log_gmm_leu = predict(log_gmm_leu, type="response", newdata=leuk_train_gmm)
cm_train_predict_log_gmm_leu = table(leuk_train_gmm$case_leuk, train_predict_log_gmm_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_gmm_leu) / colSums(cm_train_predict_log_gmm_leu))[2])
# Precision 
print((diag(cm_train_predict_log_gmm_leu) / rowSums(cm_train_predict_log_gmm_leu))[2])
ROCRtrain_predict_log_gmm_leu = prediction(train_predict_log_gmm_leu, leuk_train_gmm$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_gmm_leu, "auc")@y.values)

# lymphoma
# creating subset for gmm model 
lymph_train_gmm <- lymph_gmm_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]
lymph_test_gmm <- lymph_gmm_test[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_0")]

# change categorical to factors 
lymph_train_gmm$Smoking_status <- factor(lymph_train_gmm$Smoking_status)
lymph_test_gmm$Smoking_status <- factor(lymph_test_gmm$Smoking_status)
lymph_train_gmm$Smoking_status <- relevel(lymph_train_gmm$Smoking_status, ref="Never")
lymph_test_gmm$Smoking_status <- relevel(lymph_test_gmm$Smoking_status, ref="Never")

log_gmm_lymph =glm(case_lymph ~ ., data=lymph_train_gmm, family=binomial)
summary(log_gmm_lymph)
plot_log_gmm_lymph <- plot_model(log_gmm_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "GMM Model: Odds Ratios for Lymphoma (k=2)",ci_method=wald)
plot_log_gmm_lymph

# Calculate the odds ratio
exp(coef(log_gmm_lymph))
exp(cbind(coef(log_gmm_lymph), confint(log_gmm_lymph)))

# predict on the test set
predict_log_gmm_lymph = predict(log_gmm_lymph, type="response", newdata=lymph_test_gmm)
cm_log_gmm_lymph = table(lymph_test_gmm$case_lymph, predict_log_gmm_lymph > 0.5)
print(cm_log_gmm_lymph)

# Recall
print((diag(cm_log_gmm_lymph) / colSums(cm_log_gmm_lymph))[2])

# Precision 
print((diag(cm_log_gmm_lymph) / rowSums(cm_log_gmm_lymph))[2])
ROCRpred_log_gmm_lymph = prediction(predict_log_gmm_lymph, lymph_test_gmm$case_lymph)
as.numeric(performance(ROCRpred_log_gmm_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_gmm_lymph <- performance(ROCRpred_log_gmm_lymph, "tpr","fpr")
plot(perf_log_gmm_lymph ,colorize=TRUE,main = "GMM model:ROC Curve for Lymphoma Patients (k=2)")
performance(ROCRpred_log_gmm_lymph,"prec","rec")

# training - lymph
train_predict_log_gmm_lymph = predict(log_gmm_lymph, type="response", newdata=lymph_train_gmm)
cm_train_predict_log_gmm_lymph = table(lymph_train_gmm$case_lymph, train_predict_log_gmm_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_gmm_lymph) / colSums(cm_train_predict_log_gmm_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_gmm_lymph) / rowSums(cm_train_predict_log_gmm_lymph))[2])
ROCRtrain_predict_log_gmm_lymph = prediction(train_predict_log_gmm_lymph, lymph_train_gmm$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_gmm_lymph, "auc")@y.values)

# ---------------
## GMM8 model: Cluster membership + Covariates (BMI,sex,age,smoking status)
# leukemia 
# creating subset for gmm model 
leuk_train_8gmm <- leuk_gmm8_train[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]
leuk_test_8gmm <- leuk_gmm8_test[, c("case_leuk","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]

# change categorical to factors 
leuk_train_8gmm$Smoking_status <- factor(leuk_train_8gmm$Smoking_status)
leuk_test_8gmm$Smoking_status <- factor(leuk_test_gmm$Smoking_status)

leuk_train_8gmm$Smoking_status <- relevel(leuk_train_8gmm$Smoking_status, ref="Never")
leuk_test_8gmm$Smoking_status <- relevel(leuk_test_8gmm$Smoking_status, ref="Never")

log_gmm8_leu =glm(case_leuk ~ ., data=leuk_train_8gmm, family=binomial)
plot_log_gmm8_leu <- plot_model(log_gmm8_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "GMM Model: Odds Ratios for Leukemia (k=8)",ci_method=wald)
plot_log_gmm8_leu

# Calculate the odds ratio
exp(coef(log_gmm8_leu))
exp(cbind(coef(log_gmm8_leu), confint(log_gmm8_leu)))

# predict on the test set
predict_log_gmm8_leu = predict(log_gmm8_leu, type="response", newdata=leuk_test_8gmm)
cm_log_gmm8_leu = table(leuk_test_8gmm$case_leuk, predict_log_gmm8_leu > 0.5)
print(cm_log_gmm8_leu)

# Recall
print((diag(cm_log_gmm8_leu) / colSums(cm_log_gmm8_leu))[2])

# Precision 
print((diag(cm_log_gmm8_leu) / rowSums(cm_log_gmm8_leu))[2])
ROCRpred_log_gmm8_leu = prediction(predict_log_gmm8_leu, leuk_test_8gmm$case_leuk)
as.numeric(performance(ROCRpred_log_gmm8_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_gmm8_leu <- performance(ROCRpred_log_gmm8_leu, "tpr","fpr")
plot(perf_log_gmm8_leu ,colorize=TRUE,main = "GMM model: ROC Curve for Leukemia (k=8)")
performance(ROCRpred_log_gmm8_leu,"prec","rec")

# training - leuk 
train_predict_log_8gmm_leu = predict(log_gmm8_leu, type="response", newdata=leuk_train_8gmm)
cm_train_predict_log_8gmm_leu = table(leuk_train_8gmm$case_leuk, train_predict_log_8gmm_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_8gmm_leu) / colSums(cm_train_predict_log_8gmm_leu))[2])
# Precision 
print((diag(cm_train_predict_log_8gmm_leu) / rowSums(cm_train_predict_log_8gmm_leu))[2])
ROCRtrain_predict_log_8gmm_leu = prediction(train_predict_log_8gmm_leu, leuk_train_8gmm$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_8gmm_leu, "auc")@y.values)

# lymphoma
# creating subset for gmm8 model 
lymph_train_8gmm <- lymph_gmm8_train[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]
lymph_test_8gmm <- lymph_gmm8_test[, c("case_lymph","BMI","Age","Sex1","Smoking_status","cluster_ncomponents_1","cluster_ncomponents_2","cluster_ncomponents_3","cluster_ncomponents_4","cluster_ncomponents_5","cluster_ncomponents_6","cluster_ncomponents_7")]

# change categorical to factors 
lymph_train_8gmm$Smoking_status <- factor(lymph_train_8gmm$Smoking_status)
lymph_test_8gmm$Smoking_status <- factor(lymph_test_8gmm$Smoking_status)
lymph_train_8gmm$Smoking_status <- relevel(lymph_train_8gmm$Smoking_status, ref="Never")
lymph_test_8gmm$Smoking_status <- relevel(lymph_test_8gmm$Smoking_status, ref="Never")

log_gmm8_lymph =glm(case_lymph ~ ., data=lymph_train_8gmm, family=binomial)
summary(log_gmm8_lymph)
plot_log_gmm8_lymph <- plot_model(log_gmm8_lymph, sort.est = TRUE,
                                  show.valuesvalue.offset = .3,show.values = TRUE, 
                                  show.p = TRUE, wrap.title = 50, 
                                  title = "GMM Model: Odds Ratios for Lymphoma (k=8)",
                                  ci_method=wald)
plot_log_gmm8_lymph

# Calculate the odds ratio
exp(coef(log_gmm8_lymph))
exp(cbind(coef(log_gmm8_lymph), confint(log_gmm8_lymph)))

# predict on the test set
predict_log_gmm8_lymph = predict(log_gmm8_lymph, type="response", newdata=lymph_test_8gmm)
cm_log_gmm8_lymph = table(lymph_test_8gmm$case_lymph, predict_log_gmm8_lymph > 0.5)
print(cm_log_gmm8_lymph)

# Recall
print((diag(cm_log_gmm8_lymph) / colSums(cm_log_gmm8_lymph))[2])

# Precision 
print((diag(cm_log_gmm8_lymph) / rowSums(cm_log_gmm8_lymph))[2])
ROCRpred_log_gmm8_lymph = prediction(predict_log_gmm8_lymph, lymph_test_8gmm$case_lymph)
as.numeric(performance(ROCRpred_log_gmm8_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_gmm8_lymph <- performance(ROCRpred_log_gmm8_lymph, "tpr","fpr")
plot(perf_log_gmm8_lymph ,colorize=TRUE,main = "GMM model:ROC Curve for Lymphoma Patients (k=8)")
performance(ROCRpred_log_gmm8_lymph,"prec","rec")

# training - lymph
train_predict_log_8gmm_lymph = predict(log_gmm8_lymph, type="response", newdata=lymph_train_8gmm)
cm_train_predict_log_8gmm_lymph = table(lymph_train_8gmm$case_lymph, train_predict_log_8gmm_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_8gmm_lymph) / colSums(cm_train_predict_log_8gmm_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_8gmm_lymph) / rowSums(cm_train_predict_log_8gmm_lymph))[2])
ROCRtrain_predict_log_8gmm_lymph = prediction(train_predict_log_8gmm_lymph, lymph_train_8gmm$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_8gmm_lymph, "auc")@y.values)

# ---------------------------------------------------------------------------------
## Model: cov + blood counts 
# leukemia 
leuk_train_bc <- leuk_km3_train[, c(2, 5:19, 23,29)]
leuk_test_bc <- leuk_km3_test[,c(2, 5:19, 23,29)]

leuk_train_bc$Smoking_status <- factor(leuk_train_bc$Smoking_status)
leuk_test_bc$Smoking_status <- factor(leuk_test_bc$Smoking_status)

leuk_train_bc$Smoking_status <- relevel(leuk_train_bc$Smoking_status, ref="Never")
leuk_test_bc$Smoking_status <- relevel(leuk_test_bc$Smoking_status, ref="Never")

log_bc_leu =glm(case_leuk ~ ., data=leuk_train_bc, family=binomial)
summary(log_bc_leu)
plot_log_bc_leu <- plot_model(log_bc_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Blood Counts Model: Odds Ratios for Leukemia",ci_method=wald)
plot_log_bc_leu

# Calculate the odds ratio
exp(coef(log_bc_leu))
exp(cbind(coef(log_bc_leu), confint(log_bc_leu)))

# predict on the test set
predict_log_bc_leu = predict(log_bc_leu, type="response", newdata=leuk_test_bc)
cm_log_bc_leu = table(leuk_test_bc$case_leuk, predict_log_bc_leu > 0.5)
print(cm_log_bc_leu)

# Recall
print((diag(cm_log_bc_leu) / colSums(cm_log_bc_leu))[2])

# Precision 
print((diag(cm_log_bc_leu) / rowSums(cm_log_bc_leu))[2])
ROCRpred_log_bc_leu = prediction(predict_log_bc_leu, leuk_test_bc$case_leuk)
as.numeric(performance(ROCRpred_log_bc_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_bc_leu <- performance(ROCRpred_log_bc_leu, "tpr","fpr")
plot(perf_log_bc_leu ,colorize=TRUE,main = "ROC Curve for Leukemia using Blood Counts")
performance(ROCRpred_log_bc_leu,"prec","rec")

# training - leuk 
train_predict_log_bc_leu = predict(log_bc_leu, type="response", newdata=leuk_train_bc)
cm_train_predict_log_bc_leu = table(leuk_train_bc$case_leuk, train_predict_log_bc_leu > 0.5)
# Recall
print((diag(cm_train_predict_log_bc_leu) / colSums(cm_train_predict_log_bc_leu))[2])
# Precision 
print((diag(cm_train_predict_log_bc_leu) / rowSums(cm_train_predict_log_bc_leu))[2])
ROCRtrain_predict_log_bc_leu = prediction(train_predict_log_bc_leu, leuk_train_bc$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_bc_leu, "auc")@y.values)

# lymphoma
lymph_train_bc <- lymph_km3_train[, c(3, 5:19, 23,29)]
lymph_test_bc <- lymph_km3_test[,c(3, 5:19, 23,29)]

lymph_train_bc$Smoking_status <- factor(lymph_train_bc$Smoking_status)
lymph_test_bc$Smoking_status <- factor(lymph_test_bc$Smoking_status)

lymph_train_bc$Smoking_status <- relevel(lymph_train_bc$Smoking_status, ref="Never")
lymph_test_bc$Smoking_status <- relevel(lymph_test_bc$Smoking_status, ref="Never")

log_bc_lymph =glm(case_lymph ~ ., data=lymph_train_bc, family=binomial)
summary(log_bc_lymph)
plot_log_bc_lymph <- plot_model(log_bc_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, title = "Blood Counts Model: Odds Ratios for Lymphoma",ci_method=wald)
plot_log_bc_lymph

# Calculate the odds ratio
exp(coef(log_bc_lymph))
exp(cbind(coef(log_bc_lymph), confint(log_bc_lymph)))

# predict on the test set
predict_log_bc_lymph = predict(log_bc_lymph, type="response", newdata=lymph_test_bc)
cm_log_bc_lymph = table(lymph_test_bc$case_lymph, predict_log_bc_lymph > 0.5)
print(cm_log_bc_lymph)

# Recall
print((diag(cm_log_bc_lymph) / colSums(cm_log_bc_lymph))[2])

# Precision 
print((diag(cm_log_bc_lymph) / rowSums(cm_log_bc_lymph))[2])
ROCRpred_log_bc_lymph = prediction(predict_log_bc_lymph,lymph_test_bc$case_lymph)
as.numeric(performance(ROCRpred_log_bc_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_bc_lymph <- performance(ROCRpred_log_bc_lymph, "tpr","fpr")
plot(perf_log_bc_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma using Blood Counts")
performance(ROCRpred_log_bc_lymph,"prec","rec")

# training - lymph
train_predict_log_bc_lymph = predict(log_bc_lymph, type="response", newdata=lymph_train_bc)
cm_train_predict_log_bc_lymph = table(lymph_train_bc$case_lymph, train_predict_log_bc_lymph > 0.5)
# Recall
print((diag(cm_train_predict_log_bc_lymph) / colSums(cm_train_predict_log_bc_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_bc_lymph) / rowSums(cm_train_predict_log_bc_lymph))[2])
ROCRtrain_predict_log_bc_lymph = prediction(train_predict_log_bc_lymph, lymph_train_bc$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_bc_lymph, "auc")@y.values)

# ---------------------------------------------------------------------------------
## Model: km-3 + cov + lifestyle 
# leukemia
leuk_train_comp <- leuk_km3_train[,c(2, 5:6, 20:29)]
leuk_test_comp <- leuk_km3_test[,c(2,5:6,20:29)]

leuk_train_comp$Smoking_status <- factor(leuk_train_comp$Smoking_status)
leuk_train_comp$Smoking_status <- factor(leuk_train_comp$Smoking_status)
leuk_train_comp$Smoking_status <- relevel(leuk_train_comp$Smoking_status, ref="Never")
leuk_train_comp$Smoking_status <- relevel(leuk_train_comp$Smoking_status,ref="Never")

leuk_train_comp$Alcohol_status <- factor(leuk_train_comp$Alcohol_status)
leuk_train_comp$Alcohol_status <- factor(leuk_train_comp$Alcohol_status)
leuk_train_comp$Alcohol_status <- relevel(leuk_train_comp$Alcohol_status, ref="Never")
leuk_train_comp$Alcohol_status <- relevel(leuk_train_comp$Alcohol_status,ref="Never")

log_comp_leu =glm(case_leuk ~ ., data=leuk_train_comp, family=binomial)
summary(log_comp_leu)
plot_log_comp_leu <- plot_model(log_comp_leu, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 40, title = "K-means Model with Risk Factors: Odds Ratios for Leukemia (k=3)",ci_method=wald)
plot_log_comp_leu

# Calculate the odds ratio
exp(coef(log_comp_leu))
exp(cbind(coef(log_comp_leu), confint(log_comp_leu)))

# predict on the test set
predict_log_comp_leu = predict(log_comp_leu, type="response", newdata=leuk_test_comp)
cm_log_comp_leu = table(leuk_test_comp$case_leuk, predict_log_comp_leu > 0.5)
print(cm_log_comp_leu)

# Recall
print((diag(cm_log_comp_leu) / colSums(cm_log_comp_leu))[2])

# Precision 
print((diag(cm_log_comp_leu) / rowSums(cm_log_comp_leu))[2])
ROCRpred_log_comp_leu = prediction(predict_log_comp_leu, leuk_test_comp$case_leuk)
as.numeric(performance(ROCRpred_log_comp_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_comp_leu <- performance(ROCRpred_log_comp_leu, "tpr","fpr")
plot(perf_log_comp_leu ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Leukemia (k=3)")
performance(ROCRpred_log_comp_leu,"prec","rec")

# Training - leuk 
train_predict_log_comp_leu = predict(log_comp_leu, type="response", newdata=leuk_train_comp)
cm_train_predict_log_comp_leu = table(leuk_train_comp$case_leuk, train_predict_log_comp_leu > 0.5)
print(cm_train_predict_log_comp_leu)
# Recall
print((diag(cm_train_predict_log_comp_leu) / colSums(cm_train_predict_log_comp_leu))[2])
# Precision 
print((diag(cm_train_predict_log_comp_leu) / rowSums(cm_train_predict_log_comp_leu))[2])
ROCRtrain_predict_log_comp_leu = prediction(train_predict_log_comp_leu, leuk_train_comp$case_leuk)
as.numeric(performance(ROCRtrain_predict_log_comp_leu, "auc")@y.values)

# lymphoma
lymph_train_comp <- lymph_km3_train[,c(3, 5:6, 20:29)]
lymph_test_comp <- lymph_km3_test[,c(3,5:6,20:29)]

lymph_train_comp$Smoking_status <- factor(lymph_train_comp$Smoking_status)
lymph_train_comp$Smoking_status <- factor(lymph_train_comp$Smoking_status)
lymph_train_comp$Smoking_status <- relevel(lymph_train_comp$Smoking_status, ref="Never")
lymph_train_comp$Smoking_status <- relevel(lymph_train_comp$Smoking_status,ref="Never")

lymph_train_comp$Alcohol_status <- factor(lymph_train_comp$Alcohol_status)
lymph_train_comp$Alcohol_status <- factor(lymph_train_comp$Alcohol_status)
lymph_train_comp$Alcohol_status <- relevel(lymph_train_comp$Alcohol_status, ref="Never")
lymph_train_comp$Alcohol_status <- relevel(lymph_train_comp$Alcohol_status,ref="Never")

log_comp_lymph =glm(case_lymph ~ ., data=lymph_train_comp, family=binomial)
summary(log_comp_lymph)
plot_log_comp_lymph <- plot_model(log_comp_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 40, title = "K-means Model with Risk Factors: Odds Ratios for Lymphoma (k=3)",ci_method=wald)
plot_log_comp_lymph


# Calculate the odds ratio
exp(coef(log_comp_lymph))
exp(cbind(coef(log_comp_lymph), confint(log_comp_lymph)))

# predict on the test set
predict_log_comp_lymph = predict(log_comp_lymph, type="response", newdata=lymph_test_comp)
cm_log_comp_lymph = table(lymph_test_comp$case_lymph, predict_log_comp_lymph > 0.5)
print(cm_log_comp_lymph)

# Recall
print((diag(cm_log_comp_lymph) / colSums(cm_log_comp_lymph))[2])

# Precision 
print((diag(cm_log_comp_lymph) / rowSums(cm_log_comp_lymph))[2])
ROCRpred_log_comp_lymph = prediction(predict_log_comp_lymph, lymph_test_comp$case_lymph)
as.numeric(performance(ROCRpred_log_comp_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_comp_lymph <- performance(ROCRpred_log_comp_lymph, "tpr","fpr")
plot(perf_log_comp_lymph ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Lymphoma(k=3)")
performance(ROCRpred_log_comp_lymph,"prec","rec")


# Training - lymph 
train_predict_log_comp_lymph = predict(log_comp_lymph, type="response", newdata=lymph_train_comp)
cm_train_predict_log_comp_lymph = table(lymph_train_comp$case_lymph, train_predict_log_comp_lymph > 0.5)
print(cm_train_predict_log_comp_lymph)
# Recall
print((diag(cm_train_predict_log_comp_lymph) / colSums(cm_train_predict_log_comp_lymph))[2])
# Precision 
print((diag(cm_train_predict_log_comp_lymph) / rowSums(cm_train_predict_log_comp_lymph))[2])
ROCRtrain_predict_log_comp_lymph = prediction(train_predict_log_comp_lymph, lymph_train_comp$case_lymph)
as.numeric(performance(ROCRtrain_predict_log_comp_lymph, "auc")@y.values)


# --------------- PLOT GENERATIONI 
# plotting all AUC
pdf("AUC_plots_leulymph.pdf")
plot(perf_log_cov_leu ,colorize=TRUE,main = "Cov Model: ROC Curve for Leukemia")
plot(perf_log_cov_lymph ,colorize=TRUE,main = "Cov Model:ROC:Curve for Lymphoma")
plot(perf_log_km3_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=3)")
plot(perf_log_km3_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=3)")
plot(perf_log_km6_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=6)")
plot(perf_log_km6_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=6)")
plot(perf_log_km7_leu ,colorize=TRUE,main = "K-means Model:ROC Curve for Leukemia(k=7)")
plot(perf_log_km7_lymph ,colorize=TRUE,main = "K-means Model:ROC Curve for Lymphoma(k=7)")
plot(perf_log_gmm_leu ,colorize=TRUE,main = "GMM model: ROC Curve for Leukemia (k=2)")
plot(perf_log_gmm_lymph ,colorize=TRUE,main = "GMM model:ROC Curve for Lymphoma Patients (k=2)")
plot(perf_log_gmm8_leu ,colorize=TRUE,main = "GMM model: ROC Curve for Leukemia (k=8)")
plot(perf_log_gmm8_lymph ,colorize=TRUE,main = "GMM model:ROC Curve for Lymphoma Patients (k=8)")
plot(perf_log_bc_leu ,colorize=TRUE,main = "ROC Curve for Leukemia using Blood Counts")
plot(perf_log_bc_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma using Blood Counts")
plot(perf_log_comp_leu ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Leukemia (k=3)")
plot(perf_log_comp_lymph ,colorize=TRUE,main = "K-means Model with Risk Factors: ROC Curve for Lymphoma(k=3)")
dev.off()

# plotting all odds ratio 
pdf("log_odds_ratio.pdf")
plot_log_cov_leu
plot_log_cov_lymph
plot_log_km3_leu
plot_log_km3_lymph
plot_log_km6_leu
plot_log_km6_lymph
plot_log_km7_leu
plot_log_km7_lymph
plot_log_gmm_leu
plot_log_gmm_lymph
plot_log_gmm8_leu
plot_log_gmm8_lymph
plot_log_bc_leu
plot_log_bc_lymph
plot_log_comp_leu
plot_log_comp_lymph
dev.off()

pdf("log_comp.pdf")
plot_log_gmm8_lymph
plot_log_gmm8_leu
plot_log_comp_lymph
plot_log_comp_leu
dev.off()

pdf("AUC_gmm8.pdf")
plot(perf_log_gmm8_leu ,colorize=TRUE,main = "GMM model: ROC Curve for Leukemia (k=8)")
plot(perf_log_gmm8_lymph ,colorize=TRUE,main = "GMM model:ROC Curve for Lymphoma Patients (k=8)")
dev.off()


# Combinng all AUC
auc1 <- roc(leuk_test_cov$case_leuk, predict_log_cov_leu)
auc2 <- roc(leuk_test_km3$case_leuk, predict_log_km3_leu)
auc3 <- roc(leuk_test_km6$case_leuk, predict_log_km6_leu)
auc4 <- roc(leuk_test_km7$case_leuk, predict_log_km7_leu)
auc5 <- roc(leuk_test_8gmm$case_leuk, predict_log_gmm8_leu)
auc6 <- roc(leuk_test_bc$case_leuk, predict_log_bc_leu)
auc7 <- roc(leuk_test_comp$case_leuk, predict_log_comp_leu)

pdf("combined_leuk.pdf")
plot(auc1, col = "blue")
lines(auc2, col = "red")
lines(auc3, col = "green")
lines(auc4, col = "black")
lines(auc5, col = "orange")
lines(auc6, col = "grey")
lines(auc7, col = "brown")
legend ("bottomright",legend = c("Cov model - AUC = 0.77", 
                                 "kmean=3 model - AUC = 0.77", 
                                 "kmean=6 model - AUC = 0.76",
                                 "kmean=7 model - AUC = 0.76",
                                 "gmm=8 model - AUC = 0.52",
                                 "blood count model - AUC = 0.76",
                                 "cov + km3 + risk factor model - AUC = 0.77"),
        col = c("blue", "red", "green","black","orange","grey","brown"), lty = 1,cex = 0.5)
dev.off()


# Combining all AUC for lymphoma
auc11 <- roc(lymph_test_cov$case_lymph, predict_log_cov_lymph)
auc12 <- roc(lymph_test_km3$case_lymph, predict_log_km3_lymph)
auc13 <- roc(lymph_test_km6$case_lymph, predict_log_km6_lymph)
auc14 <- roc(lymph_test_km7$case_lymph, predict_log_km7_lymph)
auc15 <- roc(lymph_test_8gmm$case_lymph, predict_log_gmm8_lymph)
auc16 <- roc(lymph_test_bc$case_lymph, predict_log_bc_lymph)
auc17 <- roc(lymph_test_comp$case_lymph, predict_log_comp_lymph)

pdf("combined_lymph.pdf")
plot(auc11, col = "blue")
lines(auc12, col = "red")
lines(auc13, col = "green")
lines(auc14, col = "black")
lines(auc15, col = "orange")
lines(auc16, col = "grey")
lines(auc17, col = "brown")
legend ("bottomright",legend = c("Cov model - AUC = 0.66", 
                                 "kmean=3 model - AUC = 0.65", 
                                 "kmean=6 model - AUC = 0.63",
                                 "kmean=7 model - AUC = 0.64",
                                 "gmm=8 model - AUC = 0.53",
                                 "blood count model - AUC = 0.60",
                                 "cov + km3 + risk factor model - AUC = 0.64"),
        col = c("blue", "red", "green","black","orange","grey","brown"), lty = 1,cex = 0.5)
dev.off()
