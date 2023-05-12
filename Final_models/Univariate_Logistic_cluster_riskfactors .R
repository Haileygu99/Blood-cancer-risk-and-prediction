# loading library 
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

# loading dataset 
setwd("/rds/general/project/hda-22-23/live/TDS/Group5")
train_3k <- read_csv("Models/train_df_3kmeans.csv")
str(train_3k)

colnames(train_3k)

# data cleaning 
train_3k <- train_3k %>% rename(Mood_swings = Mood_swings.0.0_1)
train_3k <- train_3k %>% rename(Traff_int_major_rd = Traff_int_major_rd.0.0,C_reactive_protein = C_reactive_protein.0.0)
train_3k$Alcohol_status <- ifelse(train_3k$Alc_drinker_status.0.0_1== 1, "Past",
                                        ifelse(train_3k$Alc_drinker_status.0.0_2 == 1, "Current",
                                               ifelse(train_3k$Alc_drinker_status.0.0_0 == 1, "Never", NA)))
train_3k <- select(train_3k, -Sex.0.0_0,-Mood_swings.0.0_0,-Alc_drinker_status.0.0_0,-Alc_drinker_status.0.0_1,-Alc_drinker_status.0.0_2)
train_3k$Alcohol_status <- factor(train_3k$Alcohol_status)
train_3k$Alcohol_status<- relevel(train_3k$Alcohol_status, ref="Never")

# univariate
# logistic regression on cluster_kmeans_0 without adjusting for anything 
model_0_unadjusted <- glm(cluster_kmeans_0 ~ C_reactive_protein + Mood_swings + Alcohol_status + Traff_int_major_rd + housing_score , train_3k, family = 'binomial')
summary(model_0_unadjusted)
plot_model_0 <- plot_model(model_0_unadjusted , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                           title = "Odds Ratios - Risk factors (k=0)",ci_method=wald)

# extract the coefficients and standard errors from the model fit summary 
coefs_0_unadjusted <- coef(model_0_unadjusted)
se_0_unadjusted <- sqrt(diag(vcov(model_0_unadjusted)))
print(coefs_0_unadjusted)
print(se_0_unadjusted)

# calculating OR and CIs 
ors_0_unadjusted <- exp(coefs_0_unadjusted)
ci_0_unadjusted <- exp(coefs_0_unadjusted - (1.96 * se_0_unadjusted))
print(ors_0_unadjusted)
print(ci_0_unadjusted)

# calculating OR and CIs 
or_df_0_unadjusted <- data.frame(predictor = names(coefs_0_unadjusted),
                                 or = ors_0_unadjusted,
                                 lower = ci_0_unadjusted,
                                 upper = exp(coefs_0_unadjusted+(1.96 * se_0_unadjusted)))
print(or_df_0_unadjusted)

ggplot(or_df_0_unadjusted, aes(y=predictor, x = or, xmin=lower, xmax = upper)) +
  geom_pointrange()+
  labs(x = "OR (95% CI)", y= "") +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  annotate("text", x = .8, y = 6.4, label = "") + 
  annotate("text", x = 1.2, y = 6.4, label = "") +
  annotate("text", x = 2, y = 6.8, label = "") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))

# univariate for k_0
# Define the blood count variable names in a vector
count_vars <- c("C_reactive_protein", "Mood_swings", "Alcohol_status",
                "Traff_int_major_rd", "housing_score")

# Create an empty list to store the model objects
model_list <- list()

# Loop through the blood count variable names
for (i in 1:length(count_vars)) {
  
  # Get the name of the current blood count variable
  var_name <- count_vars[i]
  
  # Create the formula string for the current blood count variable
  formula_str <- paste("cluster_kmeans_0 ~", var_name)
  
  # Fit the logistic regression model using the current blood count variable
  model <- glm(formula_str, data = train_3k, family = "binomial")
  
  # Store the model object in the list
  model_list[[i]] <- model
  
  # Print the summary of the model
  print(summary(model))
  
  # Create an empty data frame to store the odds ratios
  odds_df <- data.frame(Blood_Count_Var = character(),
                        Odds_Ratio = numeric(),
                        Lower_CI = numeric(),
                        Upper_CI = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through the risk variables variable names
  for (i in 1:length(count_vars)) {
    
    # Get the name of the current blood count variable
    var_name <- count_vars[i]
    
    # Create the formula string for the current blood count variable
    formula_str <- paste("cluster_kmeans_0 ~", var_name)
    
    # Fit the logistic regression model using the current blood count variable
    model <- glm(formula_str, data = train_3k, family = "binomial")
    
    # Extract the coefficient estimate and confidence intervals for the blood count variable
    coef <- summary(model)$coefficients[2,]
    
    # Calculate the odds ratio and confidence intervals
    odds_ratio <- exp(coef[1])
    ci_lower <- exp(coef[1] - 1.96 * coef[2])
    ci_upper <- exp(coef[1] + 1.96 * coef[2])
    
    # Add the odds ratio and confidence intervals to the data frame
    odds_df <- rbind(odds_df, data.frame(Blood_Count_Var = var_name,
                                         Odds_Ratio = odds_ratio,
                                         Lower_CI = ci_lower,
                                         Upper_CI = ci_upper,
                                         stringsAsFactors = FALSE))
  }
  
  # Print the resulting data frame with the odds ratios
  print(odds_df)
}

###############-----------------------------------------
# univariate for k_1

model_1_unadjusted <- glm(cluster_kmeans_1 ~ C_reactive_protein + Mood_swings + Alcohol_status + Traff_int_major_rd + housing_score, train_3k, family = 'binomial')
summary(model_1_unadjusted)

coefs_1_unadjusted <- coef(model_1_unadjusted)
se_1_unadjusted <- sqrt(diag(vcov(model_1_unadjusted)))

print(coefs_1_unadjusted)
print(se_1_unadjusted)


ors_1_unadjusted <- exp(coefs_1_unadjusted)
ci_1_unadjusted <- exp(coefs_1_unadjusted - (1.96 * se_1_unadjusted))

print(ors_1_unadjusted)
print(ci_1_unadjusted)


or_df_1_unadjusted <- data.frame(predictor = names(coefs_1_unadjusted),
                                 or = ors_1_unadjusted,
                                 lower = ci_1_unadjusted,
                                 upper = exp(coefs_1_unadjusted+(1.96 * se_1_unadjusted)))

print(or_df_1_unadjusted)

ggplot(or_df_1_unadjusted, aes(y=predictor, x = or, xmin=lower, xmax = upper)) +
  geom_pointrange()+
  labs(x = "OR (95% CI)", y= "") +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  annotate("text", x = .8, y = 6.4, label = "") + 
  annotate("text", x = 1.2, y = 6.4, label = "") +
  annotate("text", x = 2, y = 6.8, label = "") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))

# Loop through the blood count variable names
for (i in 1:length(count_vars)) {
  
  # Get the name of the current blood count variable
  var_name <- count_vars[i]
  
  # Create the formula string for the current blood count variable
  formula_str <- paste("cluster_kmeans_1 ~", var_name)
  
  # Fit the logistic regression model using the current blood count variable
  model <- glm(formula_str, data = train_3k, family = "binomial")
  
  # Store the model object in the list
  model_list[[i]] <- model
  
  # Print the summary of the model
  print(summary(model))
}

#### k=2 --------------------
## multivariate 
model_2_unadjusted <- glm(cluster_kmeans_2 ~ C_reactive_protein + Mood_swings + Alcohol_status + Traff_int_major_rd + housing_score , train_3k, family = 'binomial')
summary(model_2_unadjusted)

coefs_2_unadjusted <- coef(model_2_unadjusted)
se_2_unadjusted <- sqrt(diag(vcov(model_2_unadjusted)))

print(coefs_2_unadjusted)
print(se_2_unadjusted)


ors_2_unadjusted <- exp(coefs_2_unadjusted)
ci_2_unadjusted <- exp(coefs_2_unadjusted - (1.96 * se_2_unadjusted))

print(ors_2_unadjusted)
print(ci_2_unadjusted)


or_df_2_unadjusted <- data.frame(predictor = names(coefs_2_unadjusted),
                                 or = ors_2_unadjusted,
                                 lower = ci_2_unadjusted,
                                 upper = exp(coefs_2_unadjusted+(1.96 * se_2_unadjusted)))

print(or_df_2_unadjusted)

ggplot(or_df_2_unadjusted, aes(y=predictor, x = or, xmin=lower, xmax = upper)) +
  geom_pointrange()+
  labs(x = "OR (95% CI)", y= "") +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  annotate("text", x = .8, y = 6.4, label = "") + 
  annotate("text", x = 1.2, y = 6.4, label = "") +
  annotate("text", x = 2, y = 6.8, label = "") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))

# Create an empty list to store the model objects
model_list <- list()

# Loop through the blood count variable names
for (i in 1:length(count_vars)) {
  
  # Get the name of the current blood count variable
  var_name <- count_vars[i]
  
  # Create the formula string for the current blood count variable
  formula_str <- paste("cluster_kmeans_2 ~", var_name)
  
  # Fit the logistic regression model using the current blood count variable
  model <- glm(formula_str, data = train_3k, family = "binomial")
  
  # Store the model object in the list
  model_list[[i]] <- model
  
  # Print the summary of the model
  print(summary(model))
}

# Loop through the blood count variable names
for (i in 1:length(count_vars)) {
  
  # Get the name of the current blood count variable
  var_name <- count_vars[i]
  
  # Create the formula string for the current blood count variable
  formula_str <- paste("cluster_kmeans_2 ~", var_name)
  
  # Fit the logistic regression model using the current blood count variable
  model <- glm(formula_str, data = train_3k, family = "binomial")
  
  # Extract the coefficient estimate and confidence intervals for the blood count variable
  coef <- summary(model)$coefficients[2,]
  
  # Calculate the odds ratio and confidence intervals
  odds_ratio <- exp(coef[1])
  ci_lower <- exp(coef[1] - 1.96 * coef[2])
  ci_upper <- exp(coef[1] + 1.96 * coef[2])
  
  # Add the odds ratio and confidence intervals to the data frame
  odds_df <- rbind(odds_df, data.frame(Blood_Count_Var = var_name,
                                       Odds_Ratio = odds_ratio,
                                       Lower_CI = ci_lower,
                                       Upper_CI = ci_upper,
                                       stringsAsFactors = FALSE))
}

# Print the resulting data frame with the odds ratios
print(odds_df)

### MAKING PLOTS
# Making important plots for univariate - C-reactive, Alcohol_Status 
plot_model_0 <- plot_model(model_0_unadjusted , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                           title = "Odds Ratios - Risk factors (k=0)",ci_method=wald)
plot_model_1 <- plot_model(model_1_unadjusted , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                           title = "Odds Ratios - Risk factors (k=1)",ci_method=wald)
plot_model_2 <- plot_model(model_0_unadjusted , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                           title = "Odds Ratios - Risk factors (k=2)",ci_method=wald)
# reactive protein
k0_cprotein =glm(cluster_kmeans_0 ~ C_reactive_protein, data=train_3k, family=binomial)
k1_cprotein =glm(cluster_kmeans_1~ C_reactive_protein, data=train_3k, family=binomial)
k2_cprotein =glm(cluster_kmeans_2 ~ C_reactive_protein, data=train_3k, family=binomial)
plot_k0_cp <- plot_model(k0_cprotein , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                           title = "Odds Ratios -C-reactive-protein (k=0)",ci_method=wald)
plot_k1_cp <- plot_model(k1_cprotein , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                         title = "Odds Ratios -C-reactive-protein (k=1)",ci_method=wald)
plot_k2_cp <- plot_model(k0_cprotein , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                         title = "Odds Ratios -C-reactive-protein (k=2)",ci_method=wald)

# Alcohol status 
k0_alc =glm(cluster_kmeans_0 ~ Alcohol_status, data=train_3k, family=binomial)
k1_alc =glm(cluster_kmeans_1~ Alcohol_status, data=train_3k, family=binomial)
k2_alc =glm(cluster_kmeans_2 ~ Alcohol_status, data=train_3k, family=binomial)
plot_k0_alc <- plot_model(k0_alc , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                         title = "Odds Ratios -Alcohol Intake(k=0)",ci_method=wald)
plot_k1_alc <- plot_model(k1_alc , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                         title = "Odds Ratios -Alcohol Intake(k=1)",ci_method=wald)
plot_k2_alc <- plot_model(k2_alc , sort.est = TRUE, show.valuesvalue.offset = .3,show.values = TRUE, show.p = TRUE, wrap.title = 50, 
                         title = "Odds Ratios -Alcohol Intake(k=2)",ci_method=wald)
pdf("Risk_factor_logmodels.pdf")
plot_k0_cp
plot_k1_cp
plot_k2_cp
plot_k0_alc
plot_k1_alc
plot_k2_alc
plot_model_0
plot_model_1
plot_model_2
dev.off()
