# Loading datasets --------------------------------------------------------
library(magrittr)
library(dplyr)
library(tidyverse)
library(tableone)
library(caret) #splitting data

setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
df<-readRDS('Para/Para_0217/ukb_data.rds') #Predictors
# para<-readRDS('Para/Para_0217/ukb_extracted.rds') #

vars<-readRDS('Para/Para_0217/annot.rds') #Predictor list

pop_vars<-c('Age_recr.0.0',
            'Sex.0.0',
            'BMI.0.0',
            'Mood_swings.0.0',
            'Smoking_status.0.0', 'Alc_drinker_status.0.0','Lymphocyte_count.0.0',
            'Monocyte_count.0.0','Reti_count.0.0','WBC_count.0.0','RBC_count.0.0',
            'Hgb_conc.0.0','Haematocrit_perc.0.0','Platelet_count.0.0',
            'Basophil_count.0.0','Eosinophil_count.0.0','Neutrophil_count.0.0',
            'Immature_ret_fraction.0.0','High_light_scatter_reti_count.0.0',
            'C_reactive_protein.0.0','Traff_int_major_rd.0.0','housing_score','health_score')
leuk_case<-readRDS('Outputs/Outcome_cases_raw/Leukaemia/output_final.rds') #Leukaemia cases
lymph_case<-readRDS('Outputs/Outcome_cases_raw/Lymphoma/output_final.rds') #Lymphoma cases
controls<-readRDS('raw_control/ukb_extracted.rds')

leuk_case<-subset(leuk_case,(time_to_diagnosis>180))
lymph_case<-subset(lymph_case,(time_to_diagnosis>180))
leuk_case$case_leuk<-leuk_case$case
lymph_case$case_lymph<-lymph_case$case
controls$control<-1

# Generating Dataframes ------------------------------------------------------
head(df)
table1df<-df[pop_vars]
table1df$eid<-row.names(table1df)
controls$eid<-row.names(controls)
table1df_leuk<-merge(table1df,leuk_case[,c('eid','case_leuk')],by='eid')
table1df_lymph<-merge(table1df,lymph_case[,c('eid','case_lymph')],by='eid')
table1df_controls<-merge(table1df,controls[,c('eid','control')],by='eid')
write.csv(table1df_leuk, "Outputs/df_leuk.csv")
write.csv(table1df_lymph, "Outputs/df_lymph.csv")

controls <- controls %>% mutate_if(is.character, list(~na_if(.,""))) 
summary(controls)
na_rows_bool=apply(controls[,3:length(colnames(controls))-2], MARGIN=1 ,function(x) {sum(is.na(x))==ncol(controls[,3:length(colnames(controls))-2])})
table(na_rows_bool)
control_df<-controls[na_rows_bool, ]
control_df<-control_df[,-which(names(control_df) == "control")]
table1df_controls<-table1_df_controls[,c('eid')]
write.csv(table1df_controls, "Outputs/df_control.csv")

# we created a column for pt status (leuk,lymphoma and healthy)
df_leuk <- read_csv("Outputs/df_leuk.csv")
df_lymph <- read_csv("Outputs/df_lymph.csv")
df_control <- read_csv("Outputs/df_control.csv")

# Join dataframes together (leukaemia + lymphoma)
df_lymph = df_lymph[-1]
df_leuk = df_leuk[-1]
columns = colnames(df_leuk)
columns = columns[-25]
cases = full_join(df_leuk, df_lymph, by = columns)

# creating a column with case_status and non-cases as 0 
cases$case_status = 1
cases$case_leuk = ifelse(is.na(cases$case_leuk), 0, 1)
cases$case_lymph = ifelse(is.na(cases$case_lymph), 0, 1)

# tidy up controls 
df_control = df_control[-1]
df_control = df_control[-25] #getting rid of control column 
df_control$case_leuk = 0 
df_control$case_lymph = 0 
df_control$case_status = 0  #creating new cols for control dataset 

# creating a column with case_status and non-cases as 0 
columns = colnames(cases)
combined_df = full_join(cases, df_control, by = columns)
# removing pts with multiple cancers & creating a new column for outcomes
combined_df <- combined_df[!(combined_df$case_lymph == 1 & combined_df$case_leuk == 1), ]
combined_df$cancer <- ifelse(combined_df$case_leuk == 1, 'leukemia',
                      ifelse(combined_df$case_lymph == 1, 'lymphoma',
                             ifelse(combined_df$case_status == 0, 'healthy',NA)))

# saving combined data 
write.csv(combined_df, "Outputs/combined_df.csv")


# splitting training and test data 
# ----- import combined data if have not done yet 
setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
combined_df <- read_csv("Outputs/combined_df.csv")

# ----- 80/20 splitting (caret packages)
set.seed(1234)
trainIndex <- createDataPartition(combined_df$cancer, p = 0.8, list = FALSE)
train_df <- combined_df[trainIndex, ]
test_df <- combined_df[-trainIndex, ]
write.csv(train_df, "Models/train_df.csv")
write.csv(test_df, "Models/test_df.csv")

# Summary Statistics ------------------------------------------------------

case_df<-subset(table1df,table1df$case==1)
control_df<-subset(table1df,table1df$case==0)
summary(case_df)
sd(case_df[complete.cases(case_df[,]),'Red blood cell (erythrocyte) count.0.0'])
summary(control_df)
sd(control_df[complete.cases(control_df[,]),'Red blood cell (erythrocyte) count.0.0'])

#Cases
hist(case_df[,'Lymphocyte count.0.0'])
hist(case_df[,'Monocyte count.0.0'])
hist(case_df[,'Reticulocyte count.0.0'])
hist(case_df[,'White blood cell (leukocyte) count.0.0'])
hist(case_df[,'Red blood cell (erythrocyte) count.0.0'])
hist(case_df[,'Basophill count.0.0'])
hist(case_df[,'Eosinophill count.0.0'])
hist(case_df[,'Neutrophill count.0.0'])
#Controls
hist(control_df[,'Lymphocyte count.0.0'])
hist(control_df[,'Monocyte count.0.0'])
hist(control_df[,'Reticulocyte count.0.0'])
hist(control_df[,'White blood cell (leukocyte) count.0.0'])
hist(control_df[,'Red blood cell (erythrocyte) count.0.0'])
hist(control_df[,'Basophill count.0.0'])
hist(control_df[,'Eosinophill count.0.0'])
hist(control_df[,'Neutrophill count.0.0'])

#Sex
nrow(subset(case_df,Sex.0.0==1))
nrow(subset(case_df,Sex.0.0==0))
nrow(subset(control_df,Sex.0.0==1))
nrow(subset(control_df,Sex.0.0==0))

#Age
hist(case_df[,1])
hist(control_df[,1])
summary(case_df[,1])
sd(case_df[,1])
summary(control_df[,1])
sd(control_df[complete.cases(control_df[,]),1])

#Tidied data
tidy_df<-read.csv('tidy_df.csv')
CreateTableOne(data=subset(tidy_df,cancer!='lymp'),strata='cancer')
t.test(Age.at.recruitment.0.0~cancer,data=subset(tidy_df,cancer!='leuk'))
library(ggplot2)

leuk<-subset(tidy_df,cancer=='leuk')
lymp<-subset(tidy_df,cancer=='lymp')
control<-subset(tidy_df,cancer=='healthy')

ggplot(data=tidy_df,aes(Lymphocyte.count.0.0,color=cancer,fill=cancer))+
  scale_x_log10()+
  geom_histogram(binwidth=0.05)
coord_cartesian(xlim = c(0.5, 10))+
  
  ggplot(data=tidy_df,aes(White.blood.cell.leukocyte.count.0.0,color=cancer,fill=cancer))+
  scale_x_log10()+
  geom_histogram(binwidth=0.05)+
  coord_cartesian(xlim = c(2, 20))


