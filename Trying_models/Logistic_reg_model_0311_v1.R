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


# loading dataset 
setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
df_train_case = read.csv("matching/matched_df.csv") # matched training data
df_test_case = read.csv("imp_test/test_imputed_df.csv") #imputed test data
train_df_km = read.csv("Models/train_df_kmeans.csv") #km-training
train_df_gmm = read.csv("Models/train_df_gmm.csv") #gmm-training 
test_df_km = read.csv("Models/test_df_kmeans.csv")#km-test
test_df_gmm = read.csv("Models/test_df_gmm.csv")#gmm-test

# creating a full dataframe for training set 
train_df_km$X <- rownames(train_df_km)
df_train_case <- df_train_case[, c(1, 26, 27, 28)]
df_train_km <- merge(df_train_case, train_df_km, by = "X")

train_df_gmm$X <- rownames(train_df_gmm)
df_train_gmm <- merge(df_train_case, train_df_gmm, by = "X")

# creating a full dataframe for test set 
df_test_case <- df_test_case[, c(1, 25, 26, 27)]
df_test_km <- merge(df_test_case, test_df_km, by = "X")
df_test_gmm <- merge(df_test_case, test_df_gmm, by = "X")

# creating leukemia df 
leuk_km_train <- subset(df_train_km, case_leuk == 1 | case_status == 0) #km 
leuk_km_test <- subset(df_test_km, case_leuk == 1 | case_status == 0)
leuk_gmm_train <- subset(df_train_gmm, case_leuk == 1 | case_status == 0) #gmm 
leuk_gmm_test <- subset(df_test_gmm, case_leuk == 1 | case_status == 0)

# renaming sex 0 column
leuk_km_train <- leuk_km_train %>% rename(Sex0 = Sex.0.0_0)
leuk_km_test <- leuk_km_test %>% rename(Sex0 = Sex.0.0_0.0)
leuk_gmm_train <- leuk_gmm_train %>% rename(Sex0 = Sex.0.0_0)
leuk_gmm_test <- leuk_gmm_test %>% rename(Sex0 = Sex.0.0_0.0)

leuk_km_train <- leuk_km_train %>% rename(Mood_swings = Mood_swings.0.0_0)
leuk_km_test <- leuk_km_test %>% rename(Mood_swings = Mood_swings.0.0_0)
leuk_gmm_train <- leuk_gmm_train %>% rename(Mood_swings = Mood_swings.0.0_0)
leuk_gmm_test <- leuk_gmm_test %>% rename(Mood_swings = Mood_swings.0.0_0)

# rename hard-coded columns to categorical col - smoking / drinking 
leuk_km_train$Alcohol_status <- ifelse(leuk_km_train$Alc_drinker_status.0.0_1== 1, "Past",
                            ifelse(leuk_km_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                   ifelse(leuk_km_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km_train$Smoking_status <- ifelse(leuk_km_train$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_km_train$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km_train$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_km_test$Alcohol_status <- ifelse(leuk_km_test$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_km_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_km_test$Smoking_status <- ifelse(leuk_km_test$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_km_test$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_km_test$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_gmm_test$Alcohol_status <- ifelse(leuk_gmm_test$Alc_drinker_status.0.0_1== 1, "Past",
                                      ifelse(leuk_gmm_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                             ifelse(leuk_gmm_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_gmm_test$Smoking_status <- ifelse(leuk_gmm_test$Smoking_status.0.0_1 == 1, "Past",
                                      ifelse(leuk_gmm_test$Smoking_status.0.0_2 == 1, "Current",
                                             ifelse(leuk_gmm_test$Smoking_status.0.0_0 == 1, "Never", NA)))
leuk_gmm_train$Alcohol_status <- ifelse(leuk_gmm_train$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(leuk_gmm_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_gmm_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
leuk_gmm_train$Smoking_status <- ifelse(leuk_gmm_train$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(leuk_gmm_train$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(leuk_gmm_train$Smoking_status.0.0_0 == 1, "Never", NA)))



leuk_km_test <- select(leuk_km_test, -Sex.0.0_1.0,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_km_train <- select(leuk_km_train, -Sex.0.0_1,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_gmm_test <- select(leuk_gmm_test, -Sex.0.0_1.0,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
leuk_gmm_train <- select(leuk_gmm_train, -Sex.0.0_1,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)

## lymphoma dataframe cleaning 
# creating lymphoma df 
lymph_km_train <- subset(df_train_km, case_lymph == 1 | case_status == 0) #km
lymph_km_test <- subset(df_test_km, case_lymph == 1 | case_status == 0)
lymph_gmm_train <- subset(df_train_gmm, case_lymph == 1 | case_status == 0) #gmm 
lymph_gmm_test <- subset(df_test_gmm, case_lymph == 1 | case_status == 0)

# renaming sex and mood swing col
lymph_km_train <- lymph_km_train %>% rename(Sex0 = Sex.0.0_0)
lymph_km_test <- lymph_km_test %>% rename(Sex0 = Sex.0.0_0.0)
lymph_gmm_train <- lymph_gmm_train %>% rename(Sex0 = Sex.0.0_0)
lymph_gmm_test <- lymph_gmm_test %>% rename(Sex0 = Sex.0.0_0.0)

lymph_km_train <- lymph_km_train %>% rename(Mood_swings = Mood_swings.0.0_0)
lymph_km_test <- lymph_km_test %>% rename(Mood_swings = Mood_swings.0.0_0)
lymph_gmm_train <- lymph_gmm_train %>% rename(Mood_swings = Mood_swings.0.0_0)
lymph_gmm_test <- lymph_gmm_test %>% rename(Mood_swings = Mood_swings.0.0_0)

# rename hard-coded columns to categorical col - smoking / drinking 
lymph_km_train$Alcohol_status <- ifelse(lymph_km_train$Alc_drinker_status.0.0_1== 1, "Past",
                                       ifelse(lymph_km_train$Alc_drinker_status.0.0_2 == 1, "Current",
                                              ifelse(lymph_km_train$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km_train$Smoking_status <- ifelse(lymph_km_train$Smoking_status.0.0_1 == 1, "Past",
                                       ifelse(lymph_km_train$Smoking_status.0.0_2 == 1, "Current",
                                              ifelse(lymph_km_train$Smoking_status.0.0_0 == 1, "Never", NA)))
lymph_km_test$Alcohol_status <- ifelse(lymph_km_test$Alc_drinker_status.0.0_1== 1, "Past",
                                      ifelse(lymph_km_test$Alc_drinker_status.0.0_2 == 1, "Current",
                                             ifelse(lymph_km_test$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
lymph_km_test$Smoking_status <- ifelse(lymph_km_test$Smoking_status.0.0_1 == 1, "Past",
                                      ifelse(lymph_km_test$Smoking_status.0.0_2 == 1, "Current",
                                             ifelse(lymph_km_test$Smoking_status.0.0_0 == 1, "Never", NA)))
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

lymph_km_test <- select(lymph_km_test, -Sex.0.0_1.0,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_km_train <- select(lymph_km_train, -Sex.0.0_1,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_gmm_test <- select(lymph_gmm_test, -Sex.0.0_1.0,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)
lymph_gmm_train <- select(lymph_gmm_train, -Sex.0.0_1,-Mood_swings.0.0_1,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2,-Smoking_status.0.0_1,-Smoking_status.0.0_0,-Smoking_status.0.0_2)

#-------------------------------------------------------------------------------
### Fundamental model - Covariates  (BMI,sex,age,smoking status)
## leukemia 
# creating subset for fundamental model 
leuk_train_cov <- leuk_km_train[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Smoking_status")]
leuk_test_cov <- leuk_km_test[,c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Smoking_status")]

log_cov_leu = glm(case_leuk ~ ., data=leuk_train_cov, family=binomial)
summary(log_cov_leu)
plot_log_cov_leu <- plot_model(log_cov_leu, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_cov_leu ,colorize=TRUE,main = "ROC Curve for Leukemia Patients")
performance(ROCRpred_log_cov_leu,"prec","rec")

## lymphoma
# creating subset for fundamental model 
lymph_train_cov <- lymph_km_train[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Smoking_status")]
lymph_test_cov <- lymph_km_test[,c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Smoking_status")]

log_cov_lymph =glm(case_lymph ~ ., data=lymph_train_cov, family=binomial)
summary(log_cov_lymph)
plot_log_cov_lymph <- plot_model(log_cov_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_cov_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma Patients")
performance(ROCRpred_log_cov_lymph,"prec","rec")

# ---------------------------------------------------------------------------------
## K-means model: Cluster membership + Covariates (BMI,sex,age,smoking status)
# leukemia 
# creating subset for K-means model 
leuk_train_km <- leuk_km_train[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]
leuk_test_km <- leuk_km_test[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1",
                                   "Smoking_status.0.0_2","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_3","cluster_kmeans_4")]
leuk_test_km$cluster_kmeans_2 <- rep(0, nrow(leuk_test_km))

log_km_leu =glm(case_leuk ~ ., data=leuk_train_km, family=binomial)
summary(log_km_leu)
plot_log_km_leu <- plot_model(log_km_leu, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
plot_log_km_leu

# Calculate the odds ratio
exp(coef(log_km_leu))
exp(cbind(coef(log_km_leu), confint(log_km_leu)))

# predict on the test set
predict_log_km_leu = predict(log_km_leu, type="response", newdata=leuk_test_km)
cm_log_km_leu = table(leuk_test_km$case_leuk, predict_log_km_leu > 0.5)
print(cm_log_km_leu)

# Recall
print((diag(cm_log_km_leu) / colSums(cm_log_km_leu))[2])

# Precision 
print((diag(cm_log_km_leu) / rowSums(cm_log_km_leu))[2])
ROCRpred_log_km_leu = prediction(predict_log_km_leu, leuk_test_km$case_leuk)
as.numeric(performance(ROCRpred_log_km_leu, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km_leu <- performance(ROCRpred_log_km_leu, "tpr","fpr")
plot(perf_log_km_leu ,colorize=TRUE,main = "ROC Curve for Leukemia Patients")
performance(ROCRpred_log_km_leu,"prec","rec")

# lymphoma
# creating subset for K-means model - because we do not have cluster 2 
lymph_train_km <- lymph_km_train[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_2","cluster_kmeans_3","cluster_kmeans_4")]
lymph_test_km <- lymph_km_test[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1",
                                 "Smoking_status.0.0_2","cluster_kmeans_0","cluster_kmeans_1","cluster_kmeans_3","cluster_kmeans_4")]
lymph_test_km$cluster_kmeans_2 <- rep(0, nrow(lymph_test_km))

log_km_lymph =glm(case_lymph ~ ., data=lymph_train_km, family=binomial)
summary(log_km_lymph)
plot_log_km_lymph <- plot_model(log_km_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
plot_log_km_lymph

# Calculate the odds ratio
exp(coef(log_km_lymph))
exp(cbind(coef(log_km_lymph), confint(log_km_lymph)))

# predict on the test set
predict_log_km_lymph = predict(log_km_lymph, type="response", newdata=lymph_test_km)
cm_log_km_lymph = table(lymph_test_km$case_lymph, predict_log_km_lymph > 0.5)
print(cm_log_km_lymph)

# Recall
print((diag(cm_log_km_lymph) / colSums(cm_log_km_lymph))[2])

# Precision 
print((diag(cm_log_km_lymph) / rowSums(cm_log_km_lymph))[2])
ROCRpred_log_km_lymph = prediction(predict_log_km_lymph, lymph_test_km$case_lymph)
as.numeric(performance(ROCRpred_log_km_lymph, "auc")@y.values)

# plotting AUC and ROC curve
perf_log_km_lymph <- performance(ROCRpred_log_km_lymph, "tpr","fpr")
plot(perf_log_km_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma Patients")
performance(ROCRpred_log_km_lymph,"prec","rec")

# ---------------------------------------------------------------------------------
## GMM model: Cluster membership + Covariates (BMI,sex,age,smoking status)
# leukemia 
# creating subset for gmm model 
leuk_train_gmm <- leuk_gmm_train[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                     "cluster_ncomponents_0","cluster_ncomponents_1")]
leuk_test_gmm <- leuk_gmm_test[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1",
                                 "Smoking_status.0.0_2","cluster_ncomponents_0","cluster_ncomponents_1")]

log_gmm_leu =glm(case_leuk ~ ., data=leuk_train_gmm, family=binomial)
summary(log_km_leu)
plot_log_gmm_leu <- plot_model(log_gmm_leu, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_gmm_leu ,colorize=TRUE,main = "ROC Curve for Leukemia Patients")
performance(ROCRpred_log_gmm_leu,"prec","rec")

# lymphoma
# creating subset for gmm model 
lymph_train_gmm <- lymph_gmm_train[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                     "cluster_ncomponents_0","cluster_ncomponents_1")]
lymph_test_gmm <- lymph_gmm_test[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1",
                                   "Smoking_status.0.0_2","cluster_ncomponents_0","cluster_ncomponents_1")]

log_gmm_lymph =glm(case_lymph ~ ., data=lymph_train_gmm, family=binomial)
summary(log_gmm_lymph)
plot_log_gmm_lymph <- plot_model(log_gmm_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_gmm_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma Patients")
performance(ROCRpred_log_gmm_lymph,"prec","rec")

# ---------------------------------------------------------------------------------
## Model: cov + blood counts 
# leukemia 
leuk_train_bc <- leuk_km_train[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                   "Lymphocyte_count.0.0","Monocyte_count.0.0","Reti_count.0.0","WBC_count.0.0","RBC_count.0.0","Hgb_conc.0.0",
                                   "Haematocrit_perc.0.0","Platelet_count.0.0","Basophil_count.0.0","Eosinophil_count.0.0","Neutrophil_count.0.0",
                                   "Immature_ret_fraction.0.0","High_light_scatter_reti_count.0.0")]
leuk_test_bc <- leuk_km_train[, c("case_leuk","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                   "Lymphocyte_count.0.0","Monocyte_count.0.0","Reti_count.0.0","WBC_count.0.0","RBC_count.0.0","Hgb_conc.0.0",
                                   "Haematocrit_perc.0.0","Platelet_count.0.0","Basophil_count.0.0","Eosinophil_count.0.0","Neutrophil_count.0.0",
                                   "Immature_ret_fraction.0.0","High_light_scatter_reti_count.0.0")]

log_bc_leu =glm(case_leuk ~ ., data=leuk_train_bc, family=binomial)
summary(log_bc_leu)
plot_log_bc_leu <- plot_model(log_bc_leu, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_bc_leu ,colorize=TRUE,main = "ROC Curve for Leukemia Patients using Blood Counts")
performance(ROCRpred_log_bc_leu,"prec","rec")

# lymphoma
lymph_train_bc <- lymph_km_train[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                   "Lymphocyte_count.0.0","Monocyte_count.0.0","Reti_count.0.0","WBC_count.0.0","RBC_count.0.0","Hgb_conc.0.0",
                                   "Haematocrit_perc.0.0","Platelet_count.0.0","Basophil_count.0.0","Eosinophil_count.0.0","Neutrophil_count.0.0",
                                   "Immature_ret_fraction.0.0","High_light_scatter_reti_count.0.0")]
lymph_test_bc <- lymph_km_train[, c("case_lymph","BMI.0.0","Age_recr.0.0","Sex0","Sex1","Smoking_status.0.0_0","Smoking_status.0.0_1","Smoking_status.0.0_2",
                                  "Lymphocyte_count.0.0","Monocyte_count.0.0","Reti_count.0.0","WBC_count.0.0","RBC_count.0.0","Hgb_conc.0.0",
                                  "Haematocrit_perc.0.0","Platelet_count.0.0","Basophil_count.0.0","Eosinophil_count.0.0","Neutrophil_count.0.0",
                                  "Immature_ret_fraction.0.0","High_light_scatter_reti_count.0.0")]

log_bc_lymph = glm(case_lymph ~ ., data=lymph_train_bc, family=binomial)

lymph_train_bc.1 = lymph_train_bc[,-which(colnames(lymph_train_bc) %in% c("Sex0", "Smoking_status.0.0_0"))]
log_bc_lymph.1 = glm(case_lymph ~ ., data=lymph_train_bc.1, family=binomial)
summary(log_bc_lymph.1)
plot_log_bc_lymph <- plot_model(log_bc_lymph.1, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_bc_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma Patients using Blood Counts")
performance(ROCRpred_log_bc_lymph,"prec","rec")


# ---------------------------------------------------------------------------------
## Model: gmm cluster + cov + lifestyle 
# leukemia
leuk_train_comp <- leuk_km_train[,c(2, 5:6, 20:37)]
leuk_test_comp <- leuk_km_test[,c(2,5:6,20:36)]
leuk_test_comp$cluster_kmeans_2 <- rep(0, nrow(leuk_test_comp))

log_comp_leu =glm(case_leuk ~ ., data=leuk_train_comp, family=binomial)
summary(log_comp_leu)
plot_log_comp_leu <- plot_model(log_comp_leu, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_comp_leu ,colorize=TRUE,main = "ROC Curve for Leukemia Patients ")
performance(ROCRpred_log_comp_leu,"prec","rec")

# lymphoma
lymph_train_comp <- lymph_km_train[,c(3, 5:6, 20:37)]
lymph_test_comp <- lymph_km_test[,c(3,5:6,20:36)]
lymph_test_comp$cluster_kmeans_2 <- rep(0, nrow(lymph_test_comp))

log_comp_lymph =glm(case_lymph ~ ., data=lymph_train_comp, family=binomial)
summary(log_comp_lymph)
plot_log_comp_lymph <- plot_model(log_comp_lymph, sort.est = TRUE, show.valuesvalue.offset = .3,ci_method=wald)
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
plot(perf_log_comp_lymph ,colorize=TRUE,main = "ROC Curve for Lymphoma Patients ")
performance(ROCRpred_log_comp_lymph,"prec","rec")




